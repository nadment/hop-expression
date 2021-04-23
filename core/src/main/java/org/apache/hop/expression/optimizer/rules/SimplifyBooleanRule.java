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

import static org.apache.hop.expression.Operator.coerceToBoolean;
import org.apache.hop.expression.ExpressionCall;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.optimizer.Optimizer.Rule;

/**
 * Simplifies boolean expressions:
 * 
 * 1. Simplifies expressions whose answer can be determined without evaluating both sides.
 * 2. Merge same expressions.
 * 3. Removes `Not` operator.
 */
public class SimplifyBooleanRule implements Rule {

  @Override
  public IExpression apply(IExpressionContext context, ExpressionCall call) {

    if (call.getKind()==Kind.LOGICAL_NOT) {
      IExpression operand = call.getOperand(0);

      // NOT(l > r) => l <= r
      if (operand.getKind()==Kind.GREATER_THAN) {
        return new ExpressionCall(Operator.LESS_THAN_OR_EQUAL,
            ((ExpressionCall) operand).getOperands());
      }
      // NOT(l >= r) => l < r
      else if (operand.getKind()==Kind.GREATER_THAN_OR_EQUAL) {
        return new ExpressionCall(Operator.LESS_THAN, ((ExpressionCall) operand).getOperands());
      }
      // NOT(l < r) => l >= r
      else if (operand.getKind()==Kind.LESS_THAN) {
        return new ExpressionCall(Operator.GREATER_THAN_OR_EQUAL,
            ((ExpressionCall) operand).getOperands());
      }
      // NOT(l <= r) => l > r
      else if (operand.getKind()==Kind.LESS_THAN_OR_EQUAL) {
        return new ExpressionCall(Operator.GREATER_THAN,
            ((ExpressionCall) operand).getOperands());
      }
      // NOT(NOT(e)) => e
      if (operand.getKind()==Kind.LOGICAL_NOT) {
        return ((ExpressionCall) operand).getOperand(0);
      }
    }

    else if (call.getKind()==Kind.LOGICAL_OR) {

      if (call.getOperand(0).getKind()==Kind.LITERAL) {
        Boolean value = coerceToBoolean(call.getOperand(0).eval(context));
        if (value == Boolean.TRUE)
          return Literal.TRUE;
      }

      if (call.getOperand(1).getKind()==Kind.LITERAL) {
        Boolean value = coerceToBoolean(call.getOperand(1).eval(context));
        if (value == Boolean.TRUE)
          return Literal.TRUE;
      }

      if (call.getOperand(0).getKind()==Kind.LITERAL && call.getOperand(1).getKind()==Kind.LITERAL) {
        Boolean left = coerceToBoolean(call.getOperand(0).eval(context));
        Boolean right = coerceToBoolean(call.getOperand(1).eval(context));
        if (left == Boolean.FALSE || right==Boolean.FALSE)
          return Literal.FALSE;
        
        return Literal.UNKNOWN;
      }
            
      // [field] OR [field] => [field]
      if (call.getOperand(0).equals(call.getOperand(1))) {
        return call.getOperand(0);
      }
    }

    else if (call.getKind()==Kind.LOGICAL_AND) {

      if (call.getOperand(0).getKind()==Kind.LITERAL) {
        Boolean value = coerceToBoolean(call.getOperand(0).eval(context));
        if (value == null)
          return Literal.UNKNOWN;
      }

      if (call.getOperand(1).getKind()==Kind.LITERAL) {
        Boolean value = coerceToBoolean(call.getOperand(1).eval(context));
        if (value == null)
          return Literal.UNKNOWN;
      }

      if (call.getOperand(0).getKind()==Kind.LITERAL && call.getOperand(1).getKind()==Kind.LITERAL) {
        Boolean value0 = coerceToBoolean(call.getOperand(0).eval(context));
        Boolean value1 = coerceToBoolean(call.getOperand(1).eval(context));
        if (value0 == Boolean.FALSE || value1==Boolean.FALSE)
          return Literal.FALSE;
        
        return Literal.TRUE;
      }
      
      // [field] AND [field] => [field]
      if (call.getOperand(0).equals(call.getOperand(1))) {
        return call.getOperand(0);
      }
    }

    return call;
  }

}