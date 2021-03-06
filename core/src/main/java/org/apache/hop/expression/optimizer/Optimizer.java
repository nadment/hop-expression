/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression.optimizer;

import org.apache.hop.expression.ExpressionCall;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionList;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.optimizer.rules.ArithmeticRule;
import org.apache.hop.expression.optimizer.rules.CombineConcatRule;
import org.apache.hop.expression.optimizer.rules.DeterministicRule;
import org.apache.hop.expression.optimizer.rules.ReorderAssociativeRule;
import org.apache.hop.expression.optimizer.rules.SimplifyBooleanRule;
import org.apache.hop.expression.optimizer.rules.SimplifyExtractRule;
import org.apache.hop.expression.optimizer.rules.SimplifyInRule;
import org.apache.hop.expression.optimizer.rules.SimplifyLikeRule;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Optimizer {

  public interface Rule {
    public IExpression apply(IExpressionContext context, ExpressionCall call);
  }

  private static final List<Rule> RULES = Arrays.asList(new ReorderAssociativeRule(),
      new ArithmeticRule(), new SimplifyLikeRule(), new SimplifyInRule(), new SimplifyExtractRule(), new CombineConcatRule(),
      new SimplifyBooleanRule(), new DeterministicRule());

  /**
   * Try to optimize the expression.
   *
   * @param context the context
   * @param expression the expression to optimize
   * @return the optimized expression
   */
  public IExpression optimize(IExpressionContext context, IExpression expression)
      throws ExpressionException {

    if (expression instanceof ExpressionCall) {
      return optimizeCall(context, (ExpressionCall) expression);
    } else if (expression instanceof ExpressionList) {
      return optimizeList(context, (ExpressionList) expression);
    }

    return expression;
  }

  protected IExpression optimizeList(IExpressionContext context, ExpressionList list) {

    List<IExpression> elements = new ArrayList<>(list.size());
    for (IExpression element : list) {
      elements.add(optimize(context, element));
    }

    return new ExpressionList(elements);
  }

  protected IExpression optimizeCall(IExpressionContext context, ExpressionCall call) {

    List<IExpression> operands = new ArrayList<>(call.getOperandCount());
    for (int i = 0; i < call.getOperandCount(); i++) {
      IExpression operand = optimize(context, call.getOperand(i));
      for (Rule rule : RULES) {
        if (operand instanceof ExpressionCall) {
          operand = rule.apply(context, (ExpressionCall) operand);
        } 
      }
      operands.add(operand);
    }
    IExpression expression = call.clone(operands);
    
    for (Rule rule : RULES) {    
      if (expression instanceof ExpressionCall) {
        expression = rule.apply(context, (ExpressionCall) expression);
      }
    }

    
//    IExpression expression = call;
//    for (Rule rule : RULES) {
//      if (expression instanceof ExpressionCall) {
//        expression = rule.apply(context, (ExpressionCall) expression);
//      } else
//        break;
//    }

//    if (!original.equals(expression) && expression instanceof ExpressionCall)
//      return optimizeCall(context, (ExpressionCall) expression);

    return expression;
  }
}
