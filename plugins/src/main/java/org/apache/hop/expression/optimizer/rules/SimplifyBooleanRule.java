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

/**
 * Simplifies boolean expressions:
 * 
 * 1. Simplifies expressions whose answer can be determined without evaluating both sides.
 * 2. Merge same expressions.
 * 3. Removes `Not` operator.
 */
public class SimplifyBooleanRule implements Rule {

  @Override
  public IExpression apply(IExpressionContext context, OperatorCall call) {

    if ( call.is(Operator.BOOLNOT)) {

      IExpression operand = call.getOperand(0);;

      // NOT(l > r) => l <= r
      if ( operand.is(Operator.GREATER_THAN)) {
        return new OperatorCall(Operator.LESS_THAN_OR_EQUAL,
            ((OperatorCall) operand).getOperands());
        // return new LessThanOrEqual(((OperatorCall) operand).getOperands());
      }
      // NOT(l >= r) => l < r
      else if (operand.is(Operator.GREATER_THAN_OR_EQUAL)) {
        return new OperatorCall(Operator.LESS_THAN, ((OperatorCall) operand).getOperands());
      }
      // NOT(l < r) => l >= r
      else if (operand.is(Operator.LESS_THAN)) {
        return new OperatorCall(Operator.GREATER_THAN_OR_EQUAL,
            ((OperatorCall) operand).getOperands());
      }
      // NOT(l <= r) => l > r
      else if (operand.is(Operator.LESS_THAN_OR_EQUAL)) {
        return new OperatorCall(Operator.GREATER_THAN, ((OperatorCall) operand).getOperands());
      }
      // NOT(NOT(e)) => e
      if (operand.is(Operator.BOOLNOT)) {
        return ((OperatorCall) operand).getOperand(0);
      }
    }

    else if (call.is(Operator.BOOLOR)) {

      if (call.getOperand(0).is(Kind.LITERAL)) {
        Boolean value = Operator.coerceToBoolean(call.getOperand(0).eval(context));
        if (value == Boolean.TRUE)
          return Literal.TRUE;
      }

      if (call.getOperand(1).is(Kind.LITERAL)) {
        Boolean value = Operator.coerceToBoolean(call.getOperand(1).eval(context));
        if (value == Boolean.TRUE)
          return Literal.TRUE;
      }

      if (call.getOperand(0).is(Kind.LITERAL) && call.getOperand(1).is(Kind.LITERAL)) {
        Boolean left = Operator.coerceToBoolean(call.getOperand(0).eval(context));
        Boolean right = Operator.coerceToBoolean(call.getOperand(1).eval(context));
        if (left == Boolean.FALSE || right == Boolean.FALSE)
          return Literal.FALSE;

        return Literal.NULL;
      }

      // [field] OR [field] => [field]
      if (call.getOperand(0).equals(call.getOperand(1))) {
        return call.getOperand(0);
      }
    }

    else if (call.is(Operator.BOOLAND)) {

      if (call.getOperand(0).is(Kind.LITERAL)) {
        Boolean value = Operator.coerceToBoolean(call.getOperand(0).eval(context));
        if (value == null)
          return Literal.NULL;
      }

      if (call.getOperand(1).is(Kind.LITERAL)) {
        Boolean value = Operator.coerceToBoolean(call.getOperand(1).eval(context));
        if (value == null)
          return Literal.NULL;
      }

      if (call.getOperand(0).is(Kind.LITERAL) && call.getOperand(1).is(Kind.LITERAL)) {
        Boolean value0 = Operator.coerceToBoolean(call.getOperand(0).eval(context));
        Boolean value1 = Operator.coerceToBoolean(call.getOperand(1).eval(context));
        if (value0 == Boolean.FALSE || value1 == Boolean.FALSE)
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
