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
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCall;
import org.apache.hop.expression.optimizer.Optimizer.Rule;

public class ArithmeticRule implements Rule {
  @Override
  public IExpression apply(IExpressionContext context, OperatorCall call) {

    // Eliminate double NEGATIVE
    if (call.is(Operator.NEGATIVE)) {
      IExpression operand = call.getOperand(0);
      if (operand.is(Operator.NEGATIVE)) {
        return ((OperatorCall) operand).getOperand(0);
      }
    } else if (call.is(Operator.ADD)) {
      IExpression left = call.getOperand(0);
      IExpression right = call.getOperand(1);
      if (left.is(Kind.LITERAL) && right.is(Operator.ADD)) {
        OperatorCall child = (OperatorCall) right;
        if (child.getOperand(0).is(Kind.LITERAL)) {
          IExpression operation = new OperatorCall(Operator.ADD, left, child.getOperand(0));
          Literal literal = Literal.of(operation.eval(context));
          return new OperatorCall(Operator.ADD, literal, child.getOperand(1));
        }
      }
    } else if (call.is(Operator.MULTIPLY)) {
      IExpression left = call.getOperand(0);
      IExpression right = call.getOperand(1);
      if (left.is(Kind.LITERAL) && right.is(Operator.MULTIPLY)) {
        OperatorCall child = (OperatorCall) right;
        if (child.getOperand(0).is(Kind.LITERAL)) {
          IExpression operation = new OperatorCall(Operator.MULTIPLY, left, child.getOperand(0));
          Literal literal = Literal.of(operation.eval(context));
          return new OperatorCall(Operator.MULTIPLY, literal, child.getOperand(1));
        }
      }
    }

    return call;
  }


}
