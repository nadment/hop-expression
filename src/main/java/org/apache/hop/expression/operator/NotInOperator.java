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

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/**
 * Logical <code>IN</code> operator tests for a value's membership in a list of values. The IN
 * operator is a shorthand for multiple OR conditions.
 *
 * <p>Syntax of the operator:
 *
 * <ul>
 *   <li><code>field NOT IN (list of values)</code>
 * </ul>
 */
public class NotInOperator extends Operator {

  public static final NotInOperator INSTANCE = new NotInOperator();

  public NotInOperator() {
    super(
        "NOT IN",
        120,
        Associativity.LEFT,
        ReturnTypes.BOOLEAN_NULLABLE,
        null,
        OperatorCategory.COMPARISON,
        "/docs/in.html");
  }

  @Override
  public Operator not() {
    return InOperator.INSTANCE;
  }

  @Override
  public boolean checkOperandTypes(final Call call) {
    Type type = call.getOperand(0).getType();
    Array array = (Array) call.getOperand(1);
    for (IExpression operand : array) {
      if (!type.isCoercible(operand.getType())) {
        return false;
      }
    }

    return true;
  }

  /**
   * Simplifies IN expressions list of elements.
   *
   * <ul>
   *   <li>1. Remove duplicate expressions in list.*
   *   <li>2. Sort expressions on cost.
   * </ul>
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    List<IExpression> list = new ArrayList<>();

    IExpression reference = call.getOperand(0);
    Array array = (Array) call.getOperand(1);

    // NULL if left side expression is always NULL
    if (reference.isNull()) {
      return Literal.NULL_BOOLEAN;
    }

    // Try to evaluate all operands to detect error like division by zero X IN (1,2,3/0)
    if (call.isConstant()) {
      for (IExpression o : array) {
        o.getValue();
      }
    }

    // Remove null and duplicate element in list
    for (IExpression expression : array) {

      // Simplify B in (A,B,C) → B=B (only if reference is not nullable)
      if (reference.equals(expression) && !reference.getType().isNullable()) {
        return new Call(EqualOperator.INSTANCE, reference, reference);
      }

      // If this element is not present in new list then add it
      if (!list.contains(expression)) {
        list.add(expression);
      }
    }

    // Simplify x IN (a) → x = a
    if (list.size() == 1) {
      return new Call(EqualOperator.INSTANCE, reference, list.get(0));
    }

    // Sort list on cost
    list.sort(Comparator.comparing(IExpression::getCost));

    // Rebuild call
    call = new Call(this, call.getOperand(0), new Array(array.getType(), list));
    call.inferReturnType();

    return call;
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    Type type = call.getOperand(0).getType();

    // Determine common type
    Array array = (Array) call.getOperand(1);
    for (IExpression operand : array) {
      type = getCommonTypeForComparison(type, operand.getType());
    }

    // Coerce term
    boolean coercedTerm = coerceOperandType(call, type, 0);

    // Coerce values
    boolean coercedValues = false;
    List<IExpression> list = new ArrayList<>();
    for (IExpression operand : array) {
      if (Types.needToCast(operand, type)) {
        operand = Types.cast(operand, type);
        coercedValues = true;
      }
      list.add(operand);
    }

    if (coercedValues) {
      call.setOperand(1, new Array(list));
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

    Array array = (Array) operands[1];
    Type type = operands[0].getType();

    // c1 NOT IN (c2, c3, NULL) is syntactically equivalent to (c1<>c2 AND c1<>c3 AND c1<>NULL)
    Boolean result = Boolean.TRUE;
    for (IExpression expression : array) {
      Object value = expression.getValue();
      if (value == null) return null;
      if (type.compareEqual(left, value)) {
        result = Boolean.FALSE;
      }
    }
    return result;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" NOT IN (");
    array(operands[1]).unparseValues(writer);
    writer.append(')');
  }
}
