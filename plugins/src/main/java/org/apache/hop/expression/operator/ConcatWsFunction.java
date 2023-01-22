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

import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.ByteArrayOutputStream;


/**
 * String concatenation function with separator
 */
@FunctionPlugin
public class ConcatWsFunction extends Function {

  // Function
  public ConcatWsFunction() {
    super("CONCAT_WS", true, ReturnTypes.FIRST_KNOWN,
        OperandTypes.or(OperandTypes.STRING_STRING_VARIADIC, OperandTypes.BINARY_BINARY_VARIADIC),
        "i18n::Operator.Category.String", "/docs/concat_ws.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {

    Object v0 = operands[0].getValue(context);
    if ( v0==null )
      return null;
    
    boolean notFirstValue = false;
    
    // Concat Binary
    if (v0 instanceof byte[]) {      
      ByteArrayOutputStream output = new ByteArrayOutputStream();
      byte[] separator = Coerce.toBinary(v0);
      for (int i = 1; i < operands.length; i++) {
        Object value = operands[i].getValue(context);
        
        if (value != null) {
          if ( notFirstValue ) {
            output.write(separator);          
          }
          notFirstValue = true;
          output.write(Coerce.toBinary(value));
        }
      }
      
      if ( output.size()==0 ) 
        return null;
      
      return output.toByteArray();
    }

    // Concat String
    StringBuilder builder = new StringBuilder();
    String separator = Coerce.toString(v0);
    for (int i = 1; i < operands.length; i++) {
      Object value = operands[i].getValue(context);
      if (value != null) {
        if ( notFirstValue ) {
          builder.append(separator);          
        }
        notFirstValue = true;
        builder.append(Coerce.toString(value));
      }      
    }

    if ( builder.length()==0 ) 
      return null;
    
    return builder.toString();
  }
}
