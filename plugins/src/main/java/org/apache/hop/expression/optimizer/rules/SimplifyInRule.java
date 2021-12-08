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
package org.apache.hop.expression.optimizer.rules;

import org.apache.hop.expression.ExpressionList;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCall;
import org.apache.hop.expression.OperatorRegistry;
import org.apache.hop.expression.optimizer.Optimizer.Rule;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Simplifies IN expressions list of elements.
 * 1. Remove duplicate expression in list.
 * 2. Sort expression on cost.
 */
public class SimplifyInRule implements Rule {
  public IExpression apply(IExpressionContext context, OperatorCall call) {
    if (call.isOperator(OperatorRegistry.IN)) {

      List<IExpression> list = new ArrayList<>();

      // Remove duplicate element in list
      for (IExpression expression : (ExpressionList) call.getOperand(1)) {
        // If this element is not present in newList then add it
        if (!list.contains(expression)) {
          list.add(expression);
        }
      }

      // Sort list on cost
      list.sort(Comparator.comparing(IExpression::getCost));
      
      return new OperatorCall(call.getOperator(), call.getOperand(0), new ExpressionList(list));
    }

    return call;
  }
}
