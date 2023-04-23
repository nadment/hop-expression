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
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.util.Random;

/**
 * Return a random number between 0 (inclusive) and 1 (exclusive).
 */
@FunctionPlugin(names = "RAND")
public class RandomFunction extends Function {

  public RandomFunction() {
    super("RANDOM", false, ReturnTypes.NUMBER, OperandTypes.OPTIONAL_NUMERIC,
        OperatorCategory.MATHEMATICAL, "/docs/random.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    
      Random random = operands[0].getValue(context, Random.class);         
      return random.nextDouble();    
  }
  
  @Override
  public IExpression compile(final IExpressionContext context, final Call call) {
    Random random = new Random();
    if (call.getOperandCount() == 1) {
      try {
        Long seed = call.getOperand(0).getValue(context, Long.class);
        random.setSeed(seed);
      } catch (ExpressionException e) {
        return call;
      }                 
    }
        
    return new Call(call.getOperator(), Literal.of(random));
  }
}
