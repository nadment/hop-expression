/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.operator;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionParser;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.ParseExpressionException;
import org.apache.hop.expression.Token;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.OperandCountRange;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.Types;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Build a JSON object from a list of key=values pairs. <code>
 * JSON_OBJECT([KEY] key VALUE expression [, [KEY] key VALUE expression]...)</code>
 */
@FunctionPlugin
@NullMarked
public class JsonObjectFunction extends Function {

  public static final IOperandTypeChecker OTC = new JsonObjectFunctionOperandTypeChecker();

  private static final EnumSet<TypeFamily> VALUE_TYPES =
      EnumSet.of(
          TypeFamily.STRING,
          TypeFamily.BOOLEAN,
          TypeFamily.NUMERIC,
          TypeFamily.TEMPORAL,
          TypeFamily.JSON);

  public JsonObjectFunction() {
    super(
        "JSON_OBJECT",
        ReturnTypes.JSON_NULLABLE,
        OTC,
        OperatorCategory.JSON,
        "/docs/hop-user-manual/expression/json_object.html");
  }

  @Override
  public Set<String> getKeywords() {
    return Set.of("KEY", "VALUE");
  }

  /**
   * Parses <code>JSON_OBJECT([KEY] key VALUE expression [, [KEY] key VALUE expression]...)
   * </code>
   */
  @Override
  public IExpression parse(IExpressionParser parser, Token token) throws ExpressionException {

    List<IExpression> operands = new ArrayList<>();

    // Check argument will detect an error later
    if (parser.accept(Id.RPARENTHESIS)) {
      return new Call(token.start(), this, operands);
    }
    parser.expectNext(ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());

    do {
      // KEY is optional
      parser.acceptKeyword("KEY");

      // The key is an expression, its type is checked later
      if (!parser.hasNext() || parser.isNextKeyword("VALUE") || parser.isNext(Id.RPARENTHESIS)) {
        throw new ParseExpressionException(
            token.start(), ErrorCode.SYNTAX_ERROR_FUNCTION, this.getName());
      }
      operands.add(parser.parseExpression());

      if (parser.acceptKeyword("VALUE")) {
        operands.add(parser.parseExpression());
      } else {
        throw new ParseExpressionException(
            token.start(), ErrorCode.SYNTAX_ERROR_FUNCTION, this.getName());
      }

    } while (parser.accept(Id.COMMA));

    parser.expect(Id.RPARENTHESIS, ErrorCode.MISSING_RIGHT_PARENTHESIS);

    return new Call(token.start(), this, operands);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("JSON_OBJECT(");
    for (int i = 0; i < operands.length; i += 2) {
      if (i > 0) {
        writer.append(',');
      }
      writer.append("KEY ");
      operands[i].unparse(writer, 0, 0);
      writer.append(" VALUE ");
      operands[i + 1].unparse(writer, 0, 0);
    }
    writer.append(')');
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {

    ObjectNode node = JsonNodeFactory.instance.objectNode();

    for (int i = 0; i < operands.length; i += 2) {
      String key = operands[i].getValue(String.class);
      if (key == null) {
        throw new ExpressionException(ErrorCode.NULL_JSON_KEY);
      }
      node.set(key, toJson(operands[i + 1].getValue()));
    }

    return node;
  }

  /**
   * Converts a value to a JSON node. A date is written as a string in the ISO 8601 format, an array
   * as a JSON array of its elements.
   */
  private static JsonNode toJson(final @Nullable Object value) {
    JsonNodeFactory factory = JsonNodeFactory.instance;
    return switch (value) {
      case null -> factory.nullNode();
      case String str -> factory.textNode(str);
      case Boolean bool -> factory.booleanNode(bool);
      case Long integer -> factory.numberNode(integer);
      case BigDecimal number -> factory.numberNode(number);
      case JsonNode json -> json;
      case LocalDateTime date ->
          factory.textNode(DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(date));
      case ZonedDateTime date ->
          factory.textNode(DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(date));
      case Array array -> {
        int size = array.size();
        ArrayNode elements = factory.arrayNode(size);
        for (int i = 0; i < size; i++) {
          elements.add(toJson(array.get(i).getValue()));
        }
        yield elements;
      }
      default ->
          throw new ExpressionException(ErrorCode.UNSUPPORTED_JSON_TYPE, TypeName.fromValue(value));
    };
  }

  /** Returns whether a value of the type, or the elements of an array, can be written in JSON. */
  private static boolean isValueType(final Type type) {
    if (type instanceof ArrayType arrayType) {
      // A NULL element has no type
      Type elementType = arrayType.getElementType();
      return elementType.isName(TypeName.UNKNOWN) || isValueType(elementType);
    }
    return VALUE_TYPES.contains(type.getFamily());
  }

  public static class JsonObjectFunctionOperandTypeChecker implements IOperandTypeChecker {

    public JsonObjectFunctionOperandTypeChecker() {
      super();
    }

    @Override
    public boolean checkOperandTypes(Call call) {
      for (int i = 0; i < call.getOperandCount(); ) {
        // Key should be string
        IExpression key = call.getOperand(i++);
        if (!Types.isString(key.getType())) {
          return false;
        }

        IExpression value = call.getOperand(i++);
        if (value.isNull()) continue;
        if (!isValueType(value.getType())) {
          return false;
        }
      }

      return true;
    }

    @Override
    public IOperandCountRange getOperandCountRange() {
      return OperandCountRange.between(2, Integer.MAX_VALUE);
    }
  }
}
