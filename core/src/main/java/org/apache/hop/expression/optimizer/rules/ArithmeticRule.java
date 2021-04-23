/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.optimizer.rules;

import org.apache.hop.expression.ExpressionCall;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.optimizer.Optimizer;
import org.apache.hop.expression.optimizer.Optimizer.Rule;

public class ArithmeticRule implements Rule {
  @Override
  public IExpression apply(IExpressionContext context, ExpressionCall call) {

    // Eliminate double NEGATIVE
    if (call.getKind()==Kind.NEGATIVE) {
      IExpression operand = call.getOperand(0);
      if (operand.getKind()==Kind.NEGATIVE) {
        return ((ExpressionCall) operand).getOperand(0);
      }
    }    
    else if (call.getKind()==Kind.ADD) {
      IExpression left = call.getOperand(0);
      IExpression right = call.getOperand(1);
      if (left.getKind()==Kind.LITERAL && right.getKind()==Kind.ADD) {
        ExpressionCall child = (ExpressionCall) right;
        if (child.getOperand(0).getKind()==Kind.LITERAL) {
          IExpression operation = child.clone(left, child.getOperand(0));
          Literal literal = Literal.of(operation.eval(context));
          return child.clone(literal, child.getOperand(1));
        }
      }
    } else if (call.getKind()==Kind.MULTIPLY) {
      IExpression left = call.getOperand(0);
      IExpression right = call.getOperand(1);
      if (left.getKind()==Kind.LITERAL && right.getKind()==Kind.MULTIPLY) {
        ExpressionCall child = (ExpressionCall) right;
        if (child.getOperand(0).getKind()==Kind.LITERAL) {
          IExpression operation = child.clone(left, child.getOperand(0));
          Literal literal = Literal.of(operation.eval(context));
          return child.clone(literal, child.getOperand(1));
        }
      }
    }

    return call;
  }
}
