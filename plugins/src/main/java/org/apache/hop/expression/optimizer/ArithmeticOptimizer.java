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
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operators;

public class ArithmeticOptimizer extends ExpressionCompiler {
  @Override
  public IExpression apply(IExpressionContext context, Call call) {
    try {
      // Eliminate double NEGATIVE
      if (call.is(Operators.NEGATIVE)) {
        IExpression operand = call.getOperand(0);
        if (operand.is(Operators.NEGATIVE)) {
          return ((Call) operand).getOperand(0);
        }
      }
      else if (call.is(Operators.ADD)) {
        IExpression left = call.getOperand(0);
        IExpression right = call.getOperand(1);
        
        // Remove add 0
        if (Literal.ZERO.equals(left)) {
          return right;
        }   
        
        // Pull up literal
        if (left.is(Kind.LITERAL) && right.is(Operators.ADD)) {
          Call child = (Call) right;
          if (child.getOperand(0).is(Kind.LITERAL)) {
            IExpression expression =
                new Call(Operators.ADD, left, child.getOperand(0));
            Literal literal = Literal.of(expression.getValue(context));
            return new Call(Operators.ADD, literal, child.getOperand(1));
          }
        }
      }
      else if (call.is(Operators.SUBTRACT)) {
        IExpression left = call.getOperand(0);
        IExpression right = call.getOperand(1);
        
        // Remove subtract 0:  X-0=X
        if (Literal.ZERO.equals(right)) {
          return left;
        }   
        // If 0 subtract:  0-X=-X
        if (Literal.ZERO.equals(left)) {
          return new Call(Operators.NEGATIVE, right);
        }   
        
        // X-(-Z)=X+Z
        if (right.is(Operators.NEGATIVE)) {
          Call negative = (Call) right;         
          return new Call(Operators.ADD, left, negative.getOperand(0));
        }
      }
      else if (call.is(Operators.MULTIPLY)) {
        IExpression left = call.getOperand(0);
        IExpression right = call.getOperand(1);
        
        // If multiply 1
        if (Literal.ONE.equals(left)) {
          return right;
        }                  
        
        // Pull up literal
        if (left.is(Kind.LITERAL) && right.is(Operators.MULTIPLY)) {
          Call child = (Call) right;
          if (child.getOperand(0).is(Kind.LITERAL)) {
            IExpression operation =
                new Call(Operators.MULTIPLY, left, child.getOperand(0));
            Literal literal = Literal.of(operation.getValue(context));
            return new Call(Operators.MULTIPLY, literal, child.getOperand(1));
          }
        }
      }
      else if (call.is(Operators.DIVIDE)) {
        IExpression right = call.getOperand(1);
        
        // If divide by 1
        if (Literal.ONE.equals(right)) {
          return call.getOperand(0);
        }                  
        return call;
      }
    } catch (Exception e) {

    }
    
    return call;
  }


}
