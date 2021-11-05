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

import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCall;
import org.apache.hop.expression.optimizer.Optimizer.Rule;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class ReorderAssociativeRule implements Rule {

  static final Set<Operator> ASSOCIATIVE = new HashSet<>(
      Arrays.asList(Operator.ADD, Operator.BOOLAND, Operator.BOOLOR, Operator.MULTIPLY));

  @Override
  public IExpression apply(IExpressionContext context, OperatorCall call) {

    if (ASSOCIATIVE.contains(call.getOperator())) {
      IExpression left = call.getOperand(0);
      IExpression right = call.getOperand(1);

      // Swap operands
      if (left.getCost() > right.getCost()) {
        return call.clone(right, left);
      }
    }
    return call;
  }
}
