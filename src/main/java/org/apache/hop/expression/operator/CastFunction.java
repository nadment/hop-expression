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
import org.apache.hop.expression.ParseExpressionException;
import org.apache.hop.expression.Token;
import org.apache.hop.expression.Token.Id;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Converts a value of one data type into another data type <code>
 * CAST(value AS type [FORMAT format])</code>.
 *
 * @see CastOperator
 * @see TryCastFunction
 */
@FunctionPlugin
@NullMarked
public class CastFunction extends Function {

  public static final Function INSTANCE = new CastFunction();

  public CastFunction() {
    this("CAST");
  }

  public CastFunction(String id) {
    super(
        id,
        ReturnTypes.CAST_OPERATOR,
        OperandTypes.CAST,
        OperatorCategory.CONVERSION,
        "/docs/cast.html");
  }

  @Override
  public Set<String> getKeywords() {
    return Set.of("AS", "FORMAT");
  }

  /** Parses <code>CAST(value AS type [FORMAT pattern])</code> */
  @Override
  public IExpression parse(IExpressionParser parser, Token token) throws ExpressionException {
    List<IExpression> operands = new ArrayList<>();

    operands.add(parser.parseExpression());

    parser.expectKeyword("AS", ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());

    if (!parser.hasNext() || parser.isNext(Id.RPARENTHESIS)) {
      throw new ParseExpressionException(
          token.end(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    operands.add(parser.parseDataType(parser.next()));

    if (parser.acceptKeyword("FORMAT")) {
      if (!parser.hasNext()) {
        throw new ParseExpressionException(
            token.end(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
      }
      Token format = parser.next();
      if (format.is(Id.LITERAL_STRING)) operands.add(parser.parseLiteralString(format));
      else
        throw new ParseExpressionException(
            format.start(), ErrorCode.SYNTAX_ERROR_FUNCTION, token.text());
    }

    parser.expect(Id.RPARENTHESIS, ErrorCode.MISSING_RIGHT_PARENTHESIS);

    return new Call(token.start(), this, operands);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" AS ");
    Type type = operands[1].getType();
    writer.append(type.getLiteral());
    if (operands.length == 3) {
      writer.append(" FORMAT ");
      operands[2].unparse(writer, 0, 0);
    }
    writer.append(')');
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    Object value = operands[0].getValue();
    Type type = operands[1].getType();
    String format = null;
    if (operands.length == 3) {
      format = operands[2].getValue(String.class);
    }

    return type.cast(value, format);
  }

  @Override
  public IExpression compile(final IExpressionContext context, Call call)
      throws ExpressionException {

    // Cast null value
    if (call.getOperand(0).isNull()) {
      return new Literal(null, call.getType());
    }

    // Check the format, instead of ignoring it when the conversion does not use it
    if (call.getOperandCount() == 3) {
      checkFormat(call.getOperand(0).getType(), call.getType(), call.getOperand(2));
    }

    // When cast without format
    if (call.getOperandCount() == 2) {

      // Cast constant value
      if (call.getOperand(0).isConstant() && call.getOperand(0).getType().isAtomic()) {
        try {
          return new Literal(call.getValue(), call.getType());
        } catch (Exception e) {
          // If an error occurs, do not change the call
          return call;
        }
      }

      // Remove unnecessary chained cast CAST(CAST(x as type(10)) as type(5)) → CAST(x as type(5))
      if (call.getOperand(0).isOperator(CastOperator.INSTANCE)) {
        Type toType = call.getType();
        Type fromType = call.getOperand(0).getType();
        // The inner cast must not have a format, and must not overflow where the outer cast does
        // not: its integer part must have at least as many digits
        if (call(call.getOperand(0)).getOperandCount() == 2
            && Types.isLosslessCast(toType, fromType)
            && fromType.getPrecision() - Math.max(fromType.getScale(), 0)
                >= toType.getPrecision() - Math.max(toType.getScale(), 0)) {
          return new Call(
              call.getOperator(), call(call.getOperand(0)).getOperand(0), Literal.of(toType));
        }
      }

      // Remove loss-less cast
      else if (Types.isLosslessCast(call.getOperand(0).getType(), call.getType())) {
        return call.getOperand(0);
      }

      // Use default format
      //      switch (call.getType().getId()) {
      //        case BINARY:
      //          String defaultBinaryFormat =
      //              context.getVariable(ExpressionContext.EXPRESSION_BINARY_FORMAT);
      //          return new Call(
      //              call.getOperator(),
      //              call.getOperand(0),
      //              call.getOperand(1),
      //              Literal.of(defaultBinaryFormat));
      //
      //        case DATE:
      //          String defaultDateFormat =
      // context.getVariable(ExpressionContext.EXPRESSION_DATE_FORMAT);
      //          return new Call(
      //              call.getOperator(),
      //              call.getOperand(0),
      //              call.getOperand(1),
      //              Literal.of(defaultDateFormat));
      //      }
    }

    // Cast a date to a string without truncation with a constant format
    // CAST(date AS STRING FORMAT 'format') → TO_CHAR(date,'format'), its format is compiled once
    else if (Types.isDate(call.getOperand(0).getType())
        && call.getType().equalsIgnoreNullability(StringType.STRING)
        && call.getOperand(2).isConstant()) {
      return new Call(ToCharFunction.INSTANCE, call.getOperand(0), call.getOperand(2));
    }

    // TODO: the CAST function and the :: operator should call the appropriate conversion function.

    return call;
  }

  /**
   * Checks that the conversion uses a format and that the format is valid. A number format converts
   * a string to a number or a number to a string, a date format converts a string to a date or a
   * date to a string.
   *
   * @param from the type of the value to convert
   * @param to the type to convert the value to
   * @param operand the format
   * @throws ExpressionException if the conversion does not use a format or the format is invalid
   */
  static void checkFormat(final Type from, final Type to, final IExpression operand) {
    String format = operand.getValue(String.class);
    if (Types.isString(from) && Types.isNumeric(to)
        || Types.isNumeric(from) && Types.isString(to)) {
      NumberFormat.of(format);
    } else if (Types.isString(from) && Types.isDate(to)
        || Types.isDate(from) && Types.isString(to)) {
      DateTimeFormat.of(format);
    } else {
      throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, format);
    }
  }
}
