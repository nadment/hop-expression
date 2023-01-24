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
import org.apache.hop.expression.type.Coerce;

/**
 * Simplifies boolean expressions:
 * 
 * 1. Simplifies expressions whose answer can be determined without evaluating both sides.
 * 2. Merge same expressions.
 * 3. Removes `Not` operator.
 */
public class BooleanOptimizer extends Optimizer {

  @Override
  public IExpression apply(IExpressionContext context, Call call) {
    try {
      if (call.is(Operators.BOOLNOT)) {

        IExpression operand = call.getOperand(0);

        // NOT(l > r) => l <= r
        if (operand.is(Operators.GREATER_THAN)) {
          return new Call(Operators.LESS_THAN_OR_EQUAL,
              ((Call) operand).getOperands());
        }
        // NOT(l >= r) => l < r
        if (operand.is(Operators.GREATER_THAN_OR_EQUAL)) {
          return new Call(Operators.LESS_THAN,
              ((Call) operand).getOperands());
        }
        // NOT(l < r) => l >= r
        if (operand.is(Operators.LESS_THAN)) {
          return new Call(Operators.GREATER_THAN_OR_EQUAL,
              ((Call) operand).getOperands());
        }
        // NOT(l <= r) => l > r
        if (operand.is(Operators.LESS_THAN_OR_EQUAL)) {
          return new Call(Operators.GREATER_THAN,
              ((Call) operand).getOperands());
        }
        // NOT(NOT(e)) => e
        if (operand.is(Operators.BOOLNOT)) {
          return ((Call) operand).getOperand(0);
        }
        // NOT(e IS TRUE) => e IS FALSE
        if (operand.is(Operators.IS_TRUE)) {
          return new Call(Operators.IS_FALSE, ((Call) operand).getOperands());
        }
        // NOT(e IS FALSE) => e IS TRUE
        if (operand.is(Operators.IS_FALSE)) {
          return new Call(Operators.IS_TRUE, ((Call) operand).getOperands());
        }
        // NOT(e IS NULL) => e IS NOT NULL
        if (operand.is(Operators.IS_NULL)) {
          return new Call(Operators.IS_NOT_NULL, ((Call) operand).getOperands());
        }
        // NOT(e IS NOT NULL) => e IS NULL
        if (operand.is(Operators.IS_NOT_NULL)) {
          return new Call(Operators.IS_NULL, ((Call) operand).getOperands());
        }
      }

      else if (call.is(Operators.BOOLOR)) {

        if (call.getOperand(0).is(Kind.LITERAL)) {
          Boolean value = Coerce.toBoolean(call.getOperand(0).getValue(context));
          if (value == null)
            return call.getOperand(1);
          if (value == Boolean.TRUE)
            return Literal.TRUE;
        }

        if (call.getOperand(1).is(Kind.LITERAL)) {
          Boolean value = Coerce.toBoolean(call.getOperand(1).getValue(context));
          if (value == null)
            return call.getOperand(0);
          if (value == Boolean.TRUE)
            return Literal.TRUE;
        }
        
        // [field] OR [field] => [field]
        if (call.getOperand(0).equals(call.getOperand(1))) {
          return call.getOperand(0);
        }
      }

      else if (call.is(Operators.BOOLAND)) {
        boolean left = true;
        boolean right = true;

        if (call.getOperand(0).is(Kind.LITERAL)) {
          Boolean value = Coerce.toBoolean(call.getOperand(0).getValue(context));
          if (value == null)
            return Literal.NULL;
          if (value == Boolean.FALSE)
            left = false;
        }

        if (call.getOperand(1).is(Kind.LITERAL)) {
          Boolean value = Coerce.toBoolean(call.getOperand(1).getValue(context));
          if (value == null)
            return Literal.NULL;
          if (value == Boolean.FALSE)
            right = false;
        }

        // FALSE AND "field" => FALSE
        // "field" AND FALSE => FALSE
        if (!left || !right) {
          return Literal.FALSE;
        }

        // "field" AND "field" => "field"
        if (call.getOperand(0).equals(call.getOperand(1))) {
          return call.getOperand(0);
        }
      }

      return call;
    } catch (Exception e) {
      return call;
    }
  }

}
