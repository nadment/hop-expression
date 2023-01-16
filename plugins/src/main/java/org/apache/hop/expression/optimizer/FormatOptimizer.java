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
package org.apache.hop.expression.optimizer;

import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.util.NumberFormat;

/**
 * Replace string pattern format operand with a compiled format
 */
public class FormatOptimizer extends Optimizer {
  @Override
  public IExpression apply(final IExpressionContext context, final Call call) {

    if (call.is(Operators.TO_NUMBER)) {
      NumberFormat format = NumberFormat.of("TM");
      
      try {
        if ( call.getOperandCount()==2 ) {
          String pattern = (String) call.getOperand(1).getValue(context);  
          format = NumberFormat.of(pattern);
        }
      } catch (ExpressionException e) {

      }
       
      return new Call(call.getOperator(), call.getOperand(0), Literal.of(format));
    }

    return call;
  }
}
