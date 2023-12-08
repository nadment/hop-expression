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
import org.apache.hop.expression.util.Hex;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

/**
 * Converts the string expression to a binary value.
 */
@FunctionPlugin
public class ToBinaryFunction extends Function {
  public ToBinaryFunction() {
    this("TO_BINARY");
  }

  protected ToBinaryFunction(String id) {
    super(id, ReturnTypes.BINARY_NULLABLE, OperandTypes.STRING.or(OperandTypes.STRING_TEXT),
        Category.CONVERSION, "/docs/to_binary.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    String format = context.getVariable(ExpressionContext.EXPRESSION_BINARY_FORMAT);

    // Default format
    if (format == null) {
      format = "HEX";
    }

    // With specified format
    if (call.getOperandCount() == 2) {
      format = call.getOperand(1).getValue(String.class);
    }

    // Normalize pattern
    format = format.toUpperCase();
    if (format.equals("UTF-8"))
      format = "UTF8";

    if (format.equals("HEX") || format.equals("BASE64") || format.equals("UTF8")) {
      return new Call(call.getOperator(), call.getOperand(0), Literal.of(format));
    }

    throw new ExpressionException(ErrorCode.INVALID_BINARY_FORMAT, format);
  }

  @Override
  public Object eval(final IExpression[] operands) {
    final String value = operands[0].getValue(String.class);
    if (value == null)
      return null;

    final String format = operands[1].getValue(String.class);

    if (format.equals("HEX")) {
      return Hex.decode(value);
    }
    if (format.equals("UTF8")) {
      return value.getBytes(StandardCharsets.UTF_8);
    }
    if (format.equals("BASE64")) {
      return Base64.getDecoder().decode(value);
    }

    throw new ExpressionException(ErrorCode.INVALID_BINARY_FORMAT, format);
  }
}
