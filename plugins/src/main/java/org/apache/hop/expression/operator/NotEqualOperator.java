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
import org.apache.hop.expression.Category;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.Comparison;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * Comparison not equals operator '<code>!=</code>' or '<code><></code>'.
 */
public class NotEqualOperator extends Operator {

  public NotEqualOperator(final String name) {
    super("NOT_EQUAL", name, 130, true, ReturnTypes.BOOLEAN_NULLABLE, OperandTypes.ANY_ANY,
        Category.COMPARISON, "/docs/not_equal.html");
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object left = operands[0].getValue();
    if (left == null) {
      return null;
    }
    Object right = operands[1].getValue();
    if (right == null) {
      return null;
    }

    return Comparison.compare(left, right) != 0;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Normalize symmetrical operator by moving the low-cost operand to the left
    if (left.getCost() > right.getCost()) {
      return new Call(this, right, left);
    }
    
    // Normalize symmetrical operator by ordering identifiers by name
    if (left.is(Kind.IDENTIFIER) && right.is(Kind.IDENTIFIER)
        && left.asIdentifier().getName().compareTo(right.asIdentifier().getName()) > 0) {
      return new Call(this, right, left).inferReturnType();
    }
    
    // Simplify TRUE<>x → X IS NOT TRUE
    if (left.equals(Literal.TRUE)) {
      return new Call(Operators.IS_NOT_TRUE, right);
    }

    // Simplify FALSE<>x → X IS NOT FALSE
    if (left.equals(Literal.FALSE)) {
      return new Call(Operators.IS_NOT_FALSE, right);
    }

    // Simplify x!=x → NULL AND x IS NULL
    if (left.equals(right)) {
      return new Call(Operators.BOOLAND, Literal.NULL, new Call(Operators.IS_NULL, left));
    }

    // Simplify 3!=X+1 → 3-1!=X
    if (left.isConstant() && right.is(Operators.ADD_NUMERIC)
        && right.asCall().getOperand(0).isConstant()) {
      return new Call(call.getOperator(),
          new Call(Operators.SUBTRACT_NUMERIC, left, right.asCall().getOperand(0)),
          right.asCall().getOperand(1));
    }

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(getName());
    operands[1].unparse(writer);
  }
}
