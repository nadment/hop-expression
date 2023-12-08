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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.Category;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeFamily;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.Hex;
import org.apache.hop.expression.util.NumberFormat;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import java.util.Base64;

/**
 * Converts a numeric or date expression to a string value.
 */
@FunctionPlugin
public class ToCharFunction extends Function {

  private static final ToCharFunction ToCharDateFunction = new ToCharDate();
  private static final ToCharFunction ToCharBinaryFunction = new ToCharBinary();
  private static final ToCharFunction ToCharNumberFunction = new ToCharNumber();

  public ToCharFunction() {
    super("TO_CHAR", ReturnTypes.STRING_NULLABLE,
        OperandTypes.NUMERIC.or(OperandTypes.NUMERIC_TEXT).or(OperandTypes.TEMPORAL)
            .or(OperandTypes.TEMPORAL_TEXT).or(OperandTypes.BINARY).or(OperandTypes.BINARY_TEXT),
        Category.CONVERSION, "/docs/to_char.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();

    String pattern = null;
    if (call.getOperandCount() > 1) {
      pattern = call.getOperand(1).getValue(String.class);
    }

    if (type.isFamily(TypeFamily.STRING) && call.getOperandCount() == 1) {
      return call.getOperand(0);
    }

    if (type.isFamily(TypeFamily.TEMPORAL)) {
      if (pattern == null) {
        pattern = context.getVariable(ExpressionContext.EXPRESSION_DATE_FORMAT);
      }

      return new Call(ToCharDateFunction, call.getOperand(0), Literal.of(pattern));
    }

    if (type.isFamily(TypeFamily.NUMERIC)) {
      if (pattern == null) {
        pattern = "TM";
      }

      return new Call(ToCharNumberFunction, call.getOperand(0), Literal.of(pattern));
    }

    if (type.isFamily(TypeFamily.BINARY)) {
      if (pattern == null) {
        pattern = "HEX";
      }

      // Normalize pattern
      pattern = pattern.toUpperCase();
      if (pattern.equals("UTF-8")) {
        pattern = "UTF8";
      }

      if (!(pattern.equals("HEX") || pattern.equals("BASE64") || pattern.equals("UTF8"))) {
        throw new ExpressionException(ErrorCode.INVALID_BINARY_FORMAT, pattern);
      }

      return new Call(ToCharBinaryFunction, call.getOperand(0), Literal.of(pattern));
    }

    return call;
  }


  /**
   * Converts a date expression to a string value.
   */
  private static final class ToCharDate extends ToCharFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      ZonedDateTime value = operands[0].getValue(ZonedDateTime.class);
      if (value == null) {
        return null;
      }

      String pattern = operands[1].getValue(String.class);

      return DateTimeFormat.of(pattern).format(value);
    }
  }

  /**
   * Converts a numeric expression to a string value.
   */
  private static final class ToCharNumber extends ToCharFunction {

    @Override
    public Object eval(final IExpression[] operands) {
      BigDecimal value = operands[0].getValue(BigDecimal.class);
      if (value == null) {
        return null;
      }

      String pattern = operands[1].getValue(String.class);

      return NumberFormat.of(pattern).format(value);
    }
  }

  /**
   * Converts a binary expression to a string value.
   */
  private static final class ToCharBinary extends ToCharFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      byte[] bytes = operands[0].getValue(byte[].class);
      if (bytes == null) {
        return null;
      }

      String pattern = operands[1].getValue(String.class);
      if (pattern.equals("BASE64")) {
        return Base64.getEncoder().encodeToString(bytes);
      }
      if (pattern.equals("HEX")) {
        return Hex.encodeToString(bytes);
      }
      if (pattern.equals("UTF8")) {
        return new String(bytes, StandardCharsets.UTF_8);
      }

      throw new ExpressionException(ErrorCode.ILLEGAL_ARGUMENT, pattern);
    }
  }
}
