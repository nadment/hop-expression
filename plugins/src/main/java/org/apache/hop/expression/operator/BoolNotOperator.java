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
package org.apache.hop.expression.operator;

import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * Logical negation <code>NOT</code> operator
 *
 * <p>
 * Syntax of the operator:
 *
 * <ul>
 * <li><code>NOT(field IS TRUE)</code></li>
 * <li><code>NOT(field IN (list of values))</code></li>
 * <li><code>NOT(field BETWEEN start AND end)</code></li>
 * </ul>
 */
public class BoolNotOperator extends Operator {

  public BoolNotOperator() {
    super("BOOLNOT", "NOT", 150, false, ReturnTypes.BOOLEAN, OperandTypes.BOOLEAN,
        OperatorCategory.LOGICAL, "/docs/boolnot.html");
  }

  
  /**
   * Simplifies by removing unnecessary `Not` operator
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    IExpression operand = call.getOperand(0);

    // NOT(l > r) => l <= r
    if (operand.is(Operators.GREATER_THAN)) {
      return new Call(Operators.LESS_THAN_OR_EQUAL, ((Call) operand).getOperands());
    }
    // NOT(l >= r) => l < r
    if (operand.is(Operators.GREATER_THAN_OR_EQUAL)) {
      return new Call(Operators.LESS_THAN, ((Call) operand).getOperands());
    }
    // NOT(l < r) => l >= r
    if (operand.is(Operators.LESS_THAN)) {
      return new Call(Operators.GREATER_THAN_OR_EQUAL, ((Call) operand).getOperands());
    }
    // NOT(l <= r) => l > r
    if (operand.is(Operators.LESS_THAN_OR_EQUAL)) {
      return new Call(Operators.GREATER_THAN, ((Call) operand).getOperands());
    }
    // NOT(NOT(x)) => x
    if (operand.is(Operators.BOOLNOT)) {
      return ((Call) operand).getOperand(0);
    }
    // NOT(x IS TRUE) => x IS FALSE
    if (operand.is(Operators.IS_TRUE)) {
      return new Call(Operators.IS_FALSE, ((Call) operand).getOperands());
    }
    // NOT(x IS FALSE) => x IS TRUE
    if (operand.is(Operators.IS_FALSE)) {
      return new Call(Operators.IS_TRUE, ((Call) operand).getOperands());
    }
    // NOT(x IS NULL) => x IS NOT NULL
    if (operand.is(Operators.IS_NULL)) {
      return new Call(Operators.IS_NOT_NULL, ((Call) operand).getOperands());
    }
    // NOT(x IS NOT NULL) => x IS NULL
    if (operand.is(Operators.IS_NOT_NULL)) {
      return new Call(Operators.IS_NULL, ((Call) operand).getOperands());
    }

    return call;
  }
  
  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    Boolean value = operands[0].getValue(context, Boolean.class);
    if (value == null) {
      return null;
    }
    return !value;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("NOT ");
    operands[0].unparse(writer);
  }
}
