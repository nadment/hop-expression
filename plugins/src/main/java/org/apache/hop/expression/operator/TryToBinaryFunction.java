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
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Hex;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

/**
 * Converts the string expression to a binary value.
 */
@FunctionPlugin
public class TryToBinaryFunction extends Function {

  private static final TryToBinaryFunction TryToBinaryHexFunction = new TryToBinaryHexFunction();
  private static final TryToBinaryFunction TryToBinaryUtf8Function = new TryToBinaryUtf8Function();
  private static final TryToBinaryFunction TryToBinaryBase64Function =
      new TryToBinaryBase64Function();

  private static final class TryToBinaryHexFunction extends TryToBinaryFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      final String value = operands[0].getValue(String.class);
      if (value == null) {
        return null;
      }
      try {
        return Hex.decode(value);
      } catch (RuntimeException e) {
        return null;
      }
    }
  }

  private static final class TryToBinaryUtf8Function extends TryToBinaryFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      final String value = operands[0].getValue(String.class);
      if (value == null) {
        return null;
      }
      try {
        return value.getBytes(StandardCharsets.UTF_8);
      } catch (RuntimeException e) {
        return null;
      }

    }
  }

  private static final class TryToBinaryBase64Function extends TryToBinaryFunction {
    @Override
    public Object eval(final IExpression[] operands) {
      final String value = operands[0].getValue(String.class);
      if (value == null) {
        return null;
      }
      try {
        return Base64.getDecoder().decode(value);
      } catch (RuntimeException e) {
        return null;
      }
    }
  }

  public TryToBinaryFunction() {
    super("TRY_TO_BINARY", ReturnTypes.BINARY_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.STRING_TEXT), OperatorCategory.CONVERSION,
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

    // Normalize pattern
    format = format.toUpperCase();

    if (format.equals("HEX")) {
      return new Call(TryToBinaryHexFunction, call.getOperands());
    }
    if (format.equals("BASE64")) {
      return new Call(TryToBinaryBase64Function, call.getOperands());
    }
    if (format.equals("UTF8") || format.equals("UTF-8")) {
      return new Call(TryToBinaryUtf8Function, call.getOperands());
    }

    throw new ExpressionException(ErrorCode.INVALID_BINARY_FORMAT, format);
  }
}
