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

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Hex;

/** Converts the string expression to a binary value. */
@FunctionPlugin
public class ToBinaryFunction extends Function {

  private static final class ToBinaryHexString extends ToBinaryFunction {
    private static final ToBinaryFunction INSTANCE = new ToBinaryHexString();

    @Override
    public Object eval(final IExpression[] operands) {
      final String value = operands[0].getValue(String.class);
      if (value == null) {
        return null;
      }
      return Hex.decode(value);
    }
  }

  private static final class ToBinaryUtf8String extends ToBinaryFunction {
    private static final ToBinaryFunction INSTANCE = new ToBinaryUtf8String();

    @Override
    public Object eval(final IExpression[] operands) {
      final String value = operands[0].getValue(String.class);
      if (value == null) {
        return null;
      }
      return value.getBytes(StandardCharsets.UTF_8);
    }
  }

  private static final class ToBinaryBase64String extends ToBinaryFunction {
    private static final ToBinaryFunction INSTANCE = new ToBinaryBase64String();

    @Override
    public Object eval(final IExpression[] operands) {
      final String value = operands[0].getValue(String.class);
      if (value == null) {
        return null;
      }
      return Base64.getDecoder().decode(value);
    }
  }

  public ToBinaryFunction() {
    super(
        "TO_BINARY",
        ReturnTypes.BINARY_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.STRING_TEXT),
        OperatorCategory.CONVERSION,
        "/docs/to_binary.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    // Default format
    String format = context.getVariable(ExpressionContext.EXPRESSION_BINARY_FORMAT, "HEX");

    // With specified format
    if (call.getOperandCount() == 2) {
      format = call.getOperand(1).getValue(String.class);
    }

    if (format != null) {
      // Normalize pattern
      format = format.toUpperCase();

      if (format.equals("HEX")) {
        return new Call(ToBinaryHexString.INSTANCE, call.getOperands());
      }
      if (format.equals("BASE64")) {
        return new Call(ToBinaryBase64String.INSTANCE, call.getOperands());
      }
      if (format.equals("UTF8") || format.equals("UTF-8")) {
        return new Call(ToBinaryUtf8String.INSTANCE, call.getOperands());
      }
    }
    throw new ExpressionException(ErrorCode.INVALID_BINARY_FORMAT, format);
  }
}
