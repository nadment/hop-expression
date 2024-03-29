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

import static org.apache.hop.expression.type.Types.coerceOperandType;
import static org.apache.hop.expression.type.Types.getCommonTypeForComparison;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.Tuple;
import org.apache.hop.expression.type.Comparison;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.Types;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Logical <code>IN</code> operator tests for a value's membership in a list of values. The IN
 * operator is a shorthand for multiple OR conditions.
 *
 * <p>
 * Syntax of the operator:
 *
 * <ul>
 * <li><code>field [NOT] IN (list of values)</code>
 * </ul>
 */
public class InOperator extends Operator {

  private final boolean not;

  public InOperator(boolean not) {
    super(not ? "NOT IN":"IN", 120, true, ReturnTypes.BOOLEAN_NULLABLE, null, OperatorCategory.COMPARISON,
        "/docs/in.html");
    this.not = not;
  }

  
  @Override
  public Operator not() {
    return not ? Operators.IN:Operators.NOT_IN;
  }
  
  @Override
  public boolean checkOperandTypes(final Call call) {
    Type type = call.getOperand(0).getType();
    if ( type.is(TypeId.UNKNOWN)) return false;
    
    Tuple tuple = call.getOperand(1).asTuple();
    for (IExpression operand : tuple) {
      if (!type.isCoercible(operand.getType())) {
        return false;
      }
    }

    return true;
  }

  /**
   * Simplifies IN expressions list of elements.
   * 1. Remove duplicate expressions in list.
   * 2. Sort expressions on cost.
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    List<IExpression> list = new ArrayList<>();

    IExpression reference = call.getOperand(0);
    Tuple tuple = call.getOperand(1).asTuple();

    // NULL if left side expression is always NULL
    if (reference.isNull()) {
      return new Literal(null, Types.BOOLEAN);
    }

    // Try to evaluate all operands to detect error like division by zero X IN (1,2,3/0)
    if (call.isConstant()) {
      for (IExpression o : tuple) {
        o.getValue();
      }
    }

    // Remove null and duplicate element in list
    for (IExpression expression : tuple) {

      if ( !not && expression.isNull()) {
        continue;
      }

      // Simplify B in (A,B,C) → B=B (only if reference is not nullable)
      if (reference.equals(expression) && !reference.getType().isNullable()) {
        return new Call(Operators.EQUAL, reference, reference);
      }

      // If this element is not present in new list then add it
      if (!list.contains(expression)) {
        list.add(expression);
      }
    }

    // Simplify x IN (a) → x = a
    if (list.size() == 1) {
      return new Call(Operators.EQUAL, reference, list.get(0));
    }

    // Sort list on cost
    list.sort(Comparator.comparing(IExpression::getCost));

    // Rebuild tuple
    call = new Call(this, call.getOperand(0), new Tuple(list));
    call.inferReturnType();
    
    return call;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    Type type = call.getOperand(0).getType();

    // Determine common type
    Tuple tuple = call.getOperand(1).asTuple();
    for (IExpression operand : tuple) {
      type = getCommonTypeForComparison(type, operand.getType());
    }

    // Coerce term
    boolean coercedTerm = coerceOperandType(call, type, 0);

    // Coerce tuple values
    boolean coercedValues = false;
    List<IExpression> list = new ArrayList<>();
    for (IExpression operand : tuple) {
      if (Types.needToCast(operand, type)) {
        operand = Types.cast(operand, type);
        coercedValues = true;
      }
      list.add(operand);
    }

    if (coercedValues) {
      call.setOperand(1, new Tuple(list));
      call.inferReturnType();
    }

    return coercedTerm || coercedValues;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object left = operands[0].getValue();
    if (left == null) {
      return null;
    }

    Tuple tuple = (Tuple) operands[1];

    // c1 NOT IN (c2, c3, NULL) is syntactically equivalent to (c1<>c2 AND c1<>c3 AND c1<>NULL)
    if (not) {
      Boolean result = Boolean.TRUE;
      for (IExpression expression : tuple) {
        Object value = expression.getValue();
        if (value == null)
          return null;

        if (Comparison.equals(left, value)) {
          result = Boolean.FALSE;
        }
      }
      return result;
    }

    // c1 IN (c2, c3, NULL) is syntactically equivalent to (c1=c2 or c1=c3 or c1=NULL)
    for (IExpression expression : tuple) {
      Object value = expression.getValue();
      if (Comparison.equals(left, value)) {
        return Boolean.TRUE;
      }
    }
    return Boolean.FALSE;
  }
  
  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(not ? " NOT IN (" : " IN (");
    operands[1].asTuple().unparseValues(writer);
    writer.append(')');
  }
}
