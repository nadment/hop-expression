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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.DataTypeName;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.OperandCountRange;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;

/**
 * Compares the select expression to each search expression in order. As soon as a search
 * expression matches the selection expression, the corresponding result expression is returned.
 */
@FunctionPlugin
public class DecodeFunction extends Function {
  public static final IOperandTypeChecker OTC = new DecodeOperandTypeChecker();
  
  public static class DecodeOperandTypeChecker implements IOperandTypeChecker {   
    
    public DecodeOperandTypeChecker() {
    }
    
    @Override
    public boolean checkOperandTypes(Call call) {
      DataTypeName search = call.getOperand(0).getType();      
      DataTypeName result = call.getOperand(2).getType();
      
      int count = ((call.getOperandCount()-1)/2)*2;
      for (int i=1 ; i<count; i+=2) {
        if ( !search.isSameFamily(call.getOperand(i).getType()) ) {
           return false;
        }
        if ( !result.isSameFamily(call.getOperand(i+1).getType()) ) {
          return false;
       }
      }

      // Check type if function has a default value
      if ( (call.getOperandCount()-1)>count && !result.isSameFamily(call.getOperand(count+1).getType()) ) {
          return false;
      }
      
      return true;
    }
    
    @Override
    public IOperandCountRange getOperandCountRange() {
     return OperandCountRange.between(3, Integer.MAX_VALUE);
    }
  } 
  
  public DecodeFunction() {
    super("DECODE", true, ReturnTypes.ARG2, OTC, "i18n::Operator.Category.Conditional", "/docs/decode.html");
  }
  
  @Override  
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].getValue(context);

    int index = -1;
    for (int i = 1, len = operands.length - 1; i < len; i += 2) {
      Object search = operands[i].getValue(context);
      if (Coerse.compare(value, search) == 0) {
        index = i + 1;
        break;
      }
    }
    if (index < 0 && operands.length % 2 == 0) {
      index = operands.length - 1;
    }
    if (index < 0)
      return null;

    return operands[index].getValue(context);
  }
}
