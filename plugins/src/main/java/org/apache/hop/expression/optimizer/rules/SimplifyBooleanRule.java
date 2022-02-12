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


import org.apache.hop.expression.DataType;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.OperatorCall;
import org.apache.hop.expression.OperatorRegistry;
import org.apache.hop.expression.optimizer.OptimizerRule;

/**
 * Simplifies boolean expressions:
 * 
 * 1. Simplifies expressions whose answer can be determined without evaluating both sides.
 * 2. Merge same expressions.
 * 3. Removes `Not` operator.
 */
public class SimplifyBooleanRule implements OptimizerRule {

  @Override
  public IExpression apply(IExpressionContext context, OperatorCall call) {
    try {
      if (call.isOperator(OperatorRegistry.BOOLNOT)) {

        IExpression operand = call.getOperand(0);

        // NOT(l > r) => l <= r
        if (operand.isOperator(OperatorRegistry.GREATER_THAN)) {
          return new OperatorCall(OperatorRegistry.LESS_THAN_OR_EQUAL,
              ((OperatorCall) operand).getOperands());
        }
        // NOT(l >= r) => l < r
        else if (operand.isOperator(OperatorRegistry.GREATER_THAN_OR_EQUAL)) {
          return new OperatorCall(OperatorRegistry.LESS_THAN,
              ((OperatorCall) operand).getOperands());
        }
        // NOT(l < r) => l >= r
        else if (operand.isOperator(OperatorRegistry.LESS_THAN)) {
          return new OperatorCall(OperatorRegistry.GREATER_THAN_OR_EQUAL,
              ((OperatorCall) operand).getOperands());
        }
        // NOT(l <= r) => l > r
        else if (operand.isOperator(OperatorRegistry.LESS_THAN_OR_EQUAL)) {
          return new OperatorCall(OperatorRegistry.GREATER_THAN,
              ((OperatorCall) operand).getOperands());
        }
        // NOT(NOT(e)) => e
        if (operand.isOperator(OperatorRegistry.BOOLNOT)) {
          return ((OperatorCall) operand).getOperand(0);
        }
      }

      else if (call.isOperator(OperatorRegistry.BOOLOR)) {

        if (call.getOperand(0).isKind(Kind.LITERAL)) {
          Boolean value = DataType.toBoolean(call.getOperand(0).eval(context));
          if (value == null)
            return call.getOperand(1);
          if (value == Boolean.TRUE)
            return Literal.TRUE;
        }

        if (call.getOperand(1).isKind(Kind.LITERAL)) {
          Boolean value = DataType.toBoolean(call.getOperand(1).eval(context));
          if (value == null)
            return call.getOperand(0);
          if (value == Boolean.TRUE)
            return Literal.TRUE;
        }

        if (call.getOperand(0).isKind(Kind.LITERAL)) {
          Boolean value = DataType.toBoolean(call.getOperand(0).eval(context));
          if (value == Boolean.FALSE)
            return call.getOperand(1);
        }

        if (call.getOperand(1).isKind(Kind.LITERAL)) {
          Boolean value = DataType.toBoolean(call.getOperand(1).eval(context));
          if (value == Boolean.FALSE)
            return call.getOperand(0);
        }

        
        // [field] OR [field] => [field]
        if (call.getOperand(0).equals(call.getOperand(1))) {
          return call.getOperand(0);
        }
      }

      else if (call.isOperator(OperatorRegistry.BOOLAND)) {
        boolean left = true;
        boolean right = true;

        if (call.getOperand(0).isKind(Kind.LITERAL)) {
          Boolean value = DataType.toBoolean(call.getOperand(0).eval(context));
          if (value == null)
            return Literal.NULL;
          if (value == Boolean.FALSE)
            left = false;
        }

        if (call.getOperand(1).isKind(Kind.LITERAL)) {
          Boolean value = DataType.toBoolean(call.getOperand(1).eval(context));
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
