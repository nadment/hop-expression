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
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCall;
import org.apache.hop.expression.OperatorRegistry;
import org.apache.hop.expression.optimizer.Optimizer.Rule;

public class ArithmeticRule implements Rule {
  @Override
  public IExpression apply(IExpressionContext context, OperatorCall call) {
    try {
      // Eliminate double NEGATIVE
      if (call.isOperator(OperatorRegistry.NEGATIVE)) {
        IExpression operand = call.getOperand(0);
        if (operand.isOperator(OperatorRegistry.NEGATIVE)) {
          return ((OperatorCall) operand).getOperand(0);
        }
      }
      else if (call.isOperator(OperatorRegistry.ADD)) {
        IExpression left = call.getOperand(0);
        IExpression right = call.getOperand(1);
        if (left.isKind(Kind.LITERAL) && right.isOperator(OperatorRegistry.ADD)) {
          OperatorCall child = (OperatorCall) right;
          if (child.getOperand(0).isKind(Kind.LITERAL)) {
            IExpression operation =
                new OperatorCall(OperatorRegistry.ADD, left, child.getOperand(0));
            Literal literal = Literal.of(operation.eval(context));
            return new OperatorCall(OperatorRegistry.ADD, literal, child.getOperand(1));
          }
        }
      } else if (call.isOperator(OperatorRegistry.MULTIPLY)) {
        IExpression left = call.getOperand(0);
        IExpression right = call.getOperand(1);
        if (left.isKind(Kind.LITERAL) && right.isOperator(OperatorRegistry.MULTIPLY)) {
          OperatorCall child = (OperatorCall) right;
          if (child.getOperand(0).isKind(Kind.LITERAL)) {
            IExpression operation =
                new OperatorCall(OperatorRegistry.MULTIPLY, left, child.getOperand(0));
            Literal literal = Literal.of(operation.eval(context));
            return new OperatorCall(OperatorRegistry.MULTIPLY, literal, child.getOperand(1));
          }
        }
      }

      return call;
    } catch (Exception e) {
      return call;
    }
  }


}
