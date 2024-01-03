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
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.Comparison;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.Types;
import java.io.StringWriter;

/** Comparison less than or equal operator '<code>&lt;=</code>'. */
public class LessThanOrEqualOperator extends Operator {

  public LessThanOrEqualOperator() {
    super("LESS_THAN_OR_EQUAL", "<=", 130, true, ReturnTypes.BOOLEAN_NULLABLE, OperandTypes.COMPARABLE_ORDERED_COMPARABLE_ORDERED,
        OperatorCategory.COMPARISON, "/docs/less_than_or_equal.html");
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

    return Comparison.compare(left, right) <= 0;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Normalize low cost operand to the left
    if (left.getCost() > right.getCost()) {
      return new Call(Operators.GREATER_THAN_OR_EQUAL, right, left);
    }
    // Normalize the order of identifier by name
    if (left.is(Kind.IDENTIFIER) && right.is(Kind.IDENTIFIER)
        && left.asIdentifier().getName().compareTo(right.asIdentifier().getName()) > 0) {
      return new Call(Operators.GREATER_THAN_OR_EQUAL, right, left);
    }

    // Simplify if not nullable x<=x → TRUE
    if (left.equals(right) && !left.getType().isNullable() ) {
      return Literal.TRUE;
    }    
    // Simplify x<=NULL → NULL
    if (left.isNull() || right.isNull()) {
      return Literal.NULL;
    }
    // Simplify x<=x → NVL2(x,TRUE,NULL)
    if (left.equals(right)) {
      return new Call(Operators.NVL2, left, Literal.TRUE, Literal.NULL);
    }

    // Simplify only if x is data type boolean FALSE<=x → x IS NOT NULL
    if (left.equals(Literal.FALSE) && right.getType().is(TypeId.BOOLEAN)) {
      return new Call(Operators.IS_NOT_NULL, right);
    }
    // Simplify only if x is data type boolean x<=TRUE → x IS NOT NULL
    if (right.equals(Literal.TRUE) && left.getType().is(TypeId.BOOLEAN) ) {
      return new Call(Operators.IS_NOT_NULL, left);
    }    

    // Simplify 3<=X+1 → 3-1<=X
    if (left.isConstant() && right.is(Operators.ADD_NUMERIC)
        && right.asCall().getOperand(0).isConstant()) {
      return new Call(call.getOperator(),
          new Call(Operators.SUBTRACT_NUMERIC, left, right.asCall().getOperand(0)),
          right.asCall().getOperand(1));
    }

    return call;
  }
  
  @Override
  public Call castType(Call call) {
    Types.comparisonCoercion(call);    
    return super.castType(call);
  }
  
  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append("<=");
    operands[1].unparse(writer);
  }
}
