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

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import java.util.Base64;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.Hex;
import org.apache.hop.expression.util.NumberFormat;

/** Converts a numeric or date expression to a string value. */
@FunctionPlugin
public class ToCharFunction extends Function {

  public ToCharFunction() {
    super(
        "TO_CHAR",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.NUMBER
            .or(OperandTypes.NUMBER_TEXT)
            .or(OperandTypes.DATE)
            .or(OperandTypes.DATE_TEXT)
            .or(OperandTypes.BINARY)
            .or(OperandTypes.BINARY_TEXT)
            .or(OperandTypes.BOOLEAN),
        OperatorCategory.CONVERSION,
        "/docs/to_char.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();

    String pattern = null;
    if (call.getOperandCount() > 1) {
      pattern = call.getOperand(1).getValue(String.class);
    }

    if (Types.isString(type) && call.getOperandCount() == 1) {
      return call.getOperand(0);
    }

    if (Types.isDate(type)) {
      if (pattern == null) {
        pattern = context.getVariable(ExpressionContext.EXPRESSION_DATE_FORMAT);
      }

      // Compile format to check it
      DateTimeFormat format = DateTimeFormat.of(pattern);
      return new Call(new DateToCharFunction(format), call.getOperands());
    }

    if (Types.isNumeric(type)) {
      if (pattern == null) {
        pattern = "TM";
      }

      // Compile format to check it
      NumberFormat format = NumberFormat.of(pattern);
      return new Call(new NumberToCharFunction(format), call.getOperand(0), Literal.of(pattern));
    }

    if (Types.isBoolean(type)) {
      return new Call(BooleanToCharFunction.INSTANCE, call.getOperands());
    }

    if (Types.isBinary(type)) {
      if (pattern == null) {
        pattern = context.getVariable(ExpressionContext.EXPRESSION_BINARY_FORMAT, "HEX");
      }

      // Normalize pattern
      pattern = pattern.toUpperCase();

      if (pattern.equals("HEX")) {
        return new Call(BinaryHexToCharFunction.INSTANCE, call.getOperands());
      }
      if (pattern.equals("BASE64")) {
        return new Call(BinaryBase64ToCharFunction.INSTANCE, call.getOperands());
      }
      if (pattern.equals("UTF8") || pattern.equals("UTF-8")) {
        return new Call(BinaryUtf8ToCharFunction.INSTANCE, call.getOperands());
      }

      throw new ExpressionException(ErrorCode.INVALID_BINARY_FORMAT, pattern);
    }

    return call;
  }

  /** Converts a date expression to a string value. */
  private static final class DateToCharFunction extends ToCharFunction {

    private final DateTimeFormat formatter;

    public DateToCharFunction(DateTimeFormat formatter) {
      super();
      this.formatter = formatter;
    }

    @Override
    public Object eval(final IExpression[] operands) {
      ZonedDateTime value = operands[0].getValue(ZonedDateTime.class);
      if (value == null) {
        return null;
      }
      return formatter.format(value);
    }
  }

  /** Converts a numeric expression to a string value. */
  private static final class NumberToCharFunction extends ToCharFunction {

    private final NumberFormat formatter;

    public NumberToCharFunction(NumberFormat formatter) {
      super();
      this.formatter = formatter;
    }

    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal value = operands[0].getValue(BigDecimal.class);
      if (value == null) {
        return null;
      }
      return formatter.format(value);
    }
  }

  /** Converts a boolean expression to a string value. */
  private static final class BooleanToCharFunction extends ToCharFunction {
    private static final ToCharFunction INSTANCE = new BooleanToCharFunction();

    public BooleanToCharFunction() {
      super();
    }

    @Override
    public Object eval(final IExpression[] operands) {
      Boolean value = operands[0].getValue(Boolean.class);
      if (value == null) {
        return null;
      }
      return StringType.convert(value);
    }
  }

  /** Converts a binary expression to a string value. */
  private static final class BinaryHexToCharFunction extends ToCharFunction {
    private static final ToCharFunction INSTANCE = new BinaryHexToCharFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      byte[] bytes = operands[0].getValue(byte[].class);
      if (bytes == null) {
        return null;
      }
      return Hex.encodeToString(bytes);
    }
  }

  private static final class BinaryUtf8ToCharFunction extends ToCharFunction {
    private static final ToCharFunction INSTANCE = new BinaryUtf8ToCharFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      byte[] bytes = operands[0].getValue(byte[].class);
      if (bytes == null) {
        return null;
      }
      return new String(bytes, StandardCharsets.UTF_8);
    }
  }

  private static final class BinaryBase64ToCharFunction extends ToCharFunction {
    private static final ToCharFunction INSTANCE = new BinaryBase64ToCharFunction();

    @Override
    public Object eval(final IExpression[] operands) {
      byte[] bytes = operands[0].getValue(byte[].class);
      if (bytes == null) {
        return null;
      }
      return Base64.getEncoder().encodeToString(bytes);
    }
  }
}
