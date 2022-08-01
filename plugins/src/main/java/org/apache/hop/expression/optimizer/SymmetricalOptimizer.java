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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Identifier;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Operator;

/**
 * Reorganize symmetrical operator
 * 
 * 1. Move low cost operand to the left
 * 2. Go up an operand if low cost
 * 3. Order identifier by name (only useful for test)
 */
public class SymmetricalOptimizer extends ExpressionCompiler {

  @Override
  public IExpression apply(IExpressionContext context, Call call) {

    if (call.getOperator().isSymmetrical()) {
      Operator operator = call.getOperator();
      IExpression left = call.getOperand(0);
      IExpression right = call.getOperand(1);

      // Move low cost operand to the left
      if (left.getCost() > right.getCost()) {
        return new Call(operator, right, left);
      }

      // Order identifier by name
      if (left.is(Kind.IDENTIFIER) && right.is(Kind.IDENTIFIER)) {
        if (((Identifier) left).getName().compareTo(((Identifier) right).getName()) > 0) {
          return new Call(operator, right, left);
        }
      }

      if (right.is(operator)) {
        Call subCall = (Call) right;
        IExpression subLeft = subCall.getOperand(0);
        IExpression subRight = subCall.getOperand(1);

        // Go up an operand if low cost
        if (subLeft.getCost() < left.getCost()) {
          return new Call(operator, subLeft, new Call(operator, left, subRight));
        }

        // Order identifier by name
        if (left.is(Kind.IDENTIFIER) && subLeft.is(Kind.IDENTIFIER)) {
          if (((Identifier) left).getName().compareTo(((Identifier) subLeft).getName()) < 0) {
            return new Call(operator, subLeft, new Call(operator, left, subRight));
          }
        }
      }
    }
    return call;
  }
}
