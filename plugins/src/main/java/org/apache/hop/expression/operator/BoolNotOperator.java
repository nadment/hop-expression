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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
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
    super("BOOLNOT", "NOT", 150, false, ReturnTypes.BOOLEAN_NULLABLE, OperandTypes.BOOLEAN, OperatorCategory.LOGICAL,
        "/docs/boolnot.html");
  }

  /**
   * Simplifies by removing unnecessary `Not` operator
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    IExpression operand = call.getOperand(0);

    // NOT(NOT(x)) → x
    if (operand.is(Operators.BOOLNOT)) {
      return operand.asCall().getOperand(0);
    }
    
    // If the operand is a call to a logical operator that can be inverted, then remove NOT
    // NOT(l=r) → l<>r
    // NOT(l<>r) → l=r
    // NOT(l>r) → l<=r
    // NOT(l>=r) → l<r
    // NOT(l<r) → l>=r
    // NOT(l<=r) → l>r
    // NOT(x IS TRUE) => x IS NOT TRUE
    // NOT(x IS NOT TRUE) => x IS TRUE
    // NOT(x IS FALSE) → x IS NOT FALSE
    // NOT(x IS NOT FALSE) → x IS FALSE
    // NOT(x IS NULL) → x IS NOT NULL
    // NOT(x IS NOT NULL) → x IS NULL
    // NOT(x IS NOT DISTINCT FROM y) → x IS DISTINCT FROM y
    // NOT(x IS DISTINCT FROM y) → x IS NOT DISTINCT FROM y
    // NOT(x IS SIMILAR TO y) → x IS NOT SIMILAR TO  y
    // NOT(x IS NOT SIMILAR TO y) → x IS SIMILAR TO  y
    // NOT(x IN (y,z)) → x NOT IN (y,z)
    // NOT(x NOT IN (y,z)) → x IN (y,z)
    if (operand.is(Kind.CALL)) {
      Call callOperand = operand.asCall();
      Operator operator = callOperand.getOperator().not();
      if ( operator!=null ) {
        return new Call(operator, callOperand.getOperands());
      }
    }
   
    return call;
  }
  
  @Override
  public Object eval(final IExpression[] operands) {
    Boolean value = operands[0].getValue(Boolean.class);
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
