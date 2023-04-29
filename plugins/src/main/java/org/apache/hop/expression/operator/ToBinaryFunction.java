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

import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Hex;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
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
  
  public ToBinaryFunction(final String id) {
    super(id, ReturnTypes.BINARY, OperandTypes.STRING_OPTIONAL_TEXT,
        OperatorCategory.CONVERSION, "/docs/to_binary.html");
  }

  @Override
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    String pattern = context.getVariable(ExpressionContext.EXPRESSION_BINARY_FORMAT);
    
    // Default format
    if (pattern == null) {
      pattern = "HEX";
    }

    // With specified format
    if (call.getOperandCount() == 2) {
      pattern = call.getOperand(1).getValue(context, String.class);
    }

    pattern = pattern.toUpperCase();
    
    if ( pattern.equals("UTF8") ) pattern = "UTF-8";
    
    if (!(pattern.equals("HEX") || pattern.equals("BASE64") || pattern.equals("UTF-8")
        || pattern.equals("UTF8"))) {
      throw new ExpressionException(ExpressionError.INVALID_BINARY_FORMAT, pattern);
    }

    return new Call(call.getOperator(), call.getOperand(0), Literal.of(pattern));
  }
  
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    String value = operands[0].getValue(context, String.class);
    if (value == null)
      return null;

    String format = operands[1].getValue(context, String.class);
    if (format.equals("HEX")) {
      return formatHex(value);
    }
    if (format.equals("BASE64")) {
      return formatBase64(value);
    }
    if (format.equals("UTF-8")) {
      return formatUtf8(value);
    }

    throw new ExpressionException(ExpressionError.INVALID_BINARY_FORMAT, format);
  }
  
  protected byte[] formatHex(String value) throws DecoderException {
    return Hex.decodeHex(value);
  }
  
  protected byte[] formatBase64(String value) {
    return Base64.getDecoder().decode(value);
  }
    
  protected byte[] formatUtf8(String value) {
    return value.getBytes(StandardCharsets.UTF_8);
  }
}
