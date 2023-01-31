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

import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.nio.charset.StandardCharsets;

/**
 * Converts the string expression to a binary value.
 */
@FunctionPlugin
public class ToBinaryFunction extends Function {

  public ToBinaryFunction() {
    super("TO_BINARY", true, ReturnTypes.BINARY, OperandTypes.STRING_OPTIONAL_STRING,
        OperatorCategory.STRING, "/docs/to_binary.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    String value = operands[0].getValue(context, String.class);
    if (value == null)
      return null;
    
    String format = context.getVariable(ExpressionContext.EXPRESSION_BINARY_FORMAT);
    if (operands.length > 1) {
      format = operands[1].getValue(context, String.class);  
    }    
    
    if ( format==null ) {
      return null;
    }
    format = format.toUpperCase(); 
    
    if ( format.equals("HEX")) {
      return Converter.parseBinaryHex(value);
    }
    if ( format.equals("BASE64")) {
      return Converter.parseBinaryBase64(value);
    }    
    if ( format.equals("UTF-8") || format.equals("UTF8")) {
      return value.getBytes(StandardCharsets.UTF_8);
    }
    
    throw new ExpressionException(ExpressionError.INVALID_BINARY_FORMAT, format);
  }
}
