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
import com.jayway.jsonpath.InvalidPathException;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.PathNotFoundException;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.IExpressionParser;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Token;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.util.JsonConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Extracts a scalar value from a JSON string. */
@FunctionPlugin
@NullMarked
public class JsonValueFunction extends Function {

  private static final Set<TypeName> SUPPORTED_TYPES =
      Set.of(TypeName.STRING, TypeName.BOOLEAN, TypeName.INTEGER, TypeName.NUMBER, TypeName.BINARY);

  /** The JSON path compiled ahead of evaluation when it is constant, otherwise {@code null}. */
  private final @Nullable JsonPath compiledPath;

  public JsonValueFunction() {
    this(null);
  }

  private JsonValueFunction(@Nullable JsonPath compiledPath) {
    super(
        "JSON_VALUE",
        ReturnTypes.JSON_VALUE,
        OperandTypes.JSON_VALUE,
        OperatorCategory.JSON,
        "/docs/hop-user-manual/expression/json_value.html");
    this.compiledPath = compiledPath;
  }

  @Override
  public Set<String> getKeywords() {
    return Set.of("RETURNING");
  }

  /** Parses <code>JSON_VALUE(json, path [RETURNING type])</code> */
  @Override
  public IExpression parse(IExpressionParser parser, Token token) throws ExpressionException {

    List<IExpression> operands = new ArrayList<>();
    if (parser.accept(Id.RPARENTHESIS)) {
      return new Call(token.start(), this, operands);
    }
    parser.expectNext(ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());

    operands.add(parser.parseExpression());
    if (parser.accept(Id.COMMA)) {
      operands.add(parser.parseExpression());
      if (parser.acceptKeyword("RETURNING")) {
        operands.add(parser.parseDataType(parser.next()));
      }
    }

    parser.expect(Id.RPARENTHESIS, ErrorCode.MISSING_RIGHT_PARENTHESIS);

    return new Call(token.start(), this, operands);
  }

  /**
   * {@inheritDoc}
   *
   * <p>A literal path that is not a valid JSON path is rejected when the expression is validated,
   * with the position of the call, not when a row is evaluated.
   */
  @Override
  protected void validateOperands(IExpressionContext context, Call call)
      throws ExpressionException {
    super.validateOperands(context, call);
    JsonConversion.validateLiteralPath(call, 1);
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    // Add default returning type arguments
    if (call.getOperandCount() == 2) {
      return new Call(
          call.getOperator(),
          call.getOperand(0),
          call.getOperand(1),
          Literal.of(StringType.STRING));
    }

    // Check if the returning type is supported
    Type type = call.getOperand(2).getType();
    if (!SUPPORTED_TYPES.contains(type.getName())) {
      throw new ExpressionException(ErrorCode.UNEXPECTED_DATA_TYPE, type, getName());
    }

    if (compiledPath == null) {
      JsonPath path = JsonConversion.compileConstantPath(call, 1);
      if (path != null) {
        return new Call(new JsonValueFunction(path), call.getOperands());
      }
    }

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    operands[0].unparse(writer, 0, 0);
    if (operands.length > 1) {
      writer.append(',');
      operands[1].unparse(writer, 0, 0);
    }
    // The returning type is written with its clause, as it is parsed
    if (operands.length > 2) {
      writer.append(" RETURNING ");
      writer.append(operands[2].getType().getLiteral());
    }
    writer.append(')');
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    JsonNode jsonNode = operands[0].getValue(JsonNode.class);
    if (jsonNode == null) return null;

    String path = operands[1].getValue(String.class);
    // Return NULL if the JSON path is null
    if (path == null) return null;

    JsonNode result;
    try {
      JsonPath jsonPath = (compiledPath != null) ? compiledPath : JsonConversion.compilePath(path);
      // A JsonPath function returns a Java value, convert it to a JSON node to apply the
      // returning type
      result =
          JsonConversion.toJson(jsonPath.read(jsonNode, JsonConversion.JSONPATH_CONFIGURATION));
    } catch (PathNotFoundException e) {
      // Returns NULL if the path does not locate an object
      return null;
    } catch (InvalidPathException | IllegalArgumentException e) {
      // An empty path is an illegal argument for the library
      throw new ExpressionException(ErrorCode.INVALID_JSON_PATH, path);
    }

    // Only a scalar is a value, an object or an array is returned by JSON_QUERY. A path with
    // several results, like a filter, gives an array
    if (result == null || !result.isValueNode() || result.isNull()) return null;

    Type type = operands[2].getType();
    if (type.isName(TypeName.BINARY)) {
      try {
        return result.binaryValue();
      } catch (IOException e) {
        // The string is not a valid Base64
        throw new ExpressionException(
            ErrorCode.CONVERSION_ERROR, TypeName.STRING, TypeName.BINARY, result.asText());
      }
    }

    // The scalar is converted to the returning type like a cast, with its precision and scale, an
    // impossible conversion raises an error
    return type.cast(type.isName(TypeName.STRING) ? result.asText() : toValue(result));
  }

  /** Returns the value of a JSON scalar: a string, a boolean, an integer or a number. */
  private static Object toValue(final JsonNode node) {
    if (node.isBoolean()) {
      return node.booleanValue();
    }
    if (node.isIntegralNumber()) {
      // An integer out of the range of a long is a number
      return node.canConvertToLong() ? (Object) node.longValue() : node.decimalValue();
    }
    if (node.isNumber()) {
      return node.decimalValue();
    }
    return node.asText();
  }
}
