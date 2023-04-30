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
import org.apache.hop.expression.Tuple;
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
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
 * <li><code>field [NOT] IN list of values</code>
 * </ul>
 *
 * <p>
 * <b>NOTE</b> If the <code>NOT</code> clause is present the parser will generate a equivalent to
 * <code>
 * NOT (field IN list of values ...)</code>
 */
public class InOperator extends Operator {

  public InOperator() {
    super("IN", 120, true, ReturnTypes.BOOLEAN, OperandTypes.AT_LEAST_ONE_SAME_VARIADIC,
        OperatorCategory.COMPARISON, "/docs/in.html");
  }
  
  /**
   * Simplifies IN expressions list of elements.
   * 1. Remove duplicate expressions in list.
   * 2. Sort expressions on cost.
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call)
      throws ExpressionException {
    List<IExpression> list = new ArrayList<>();

    IExpression value = call.getOperand(0);
    Tuple tuple = (Tuple) call.getOperand(1);
    
    // Remove null and duplicate element in list
    for (IExpression expression : tuple) {

      if (expression.isNull()) {
        continue;
      }

      // Simplify B in (A,B,C) to B=B
      if ( value.equals(expression) ) {
        return new Call(Operators.EQUAL, value, value);
      }
      
      // If this element is not present in new list then add it
      if (!list.contains(expression)) {
        list.add(expression);
      }
    }

    // Sort list on cost
    list.sort(Comparator.comparing(IExpression::getCost));

    return new Call(this, call.getOperand(0), new Tuple(list));
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object left = operands[0].getValue(context);
    if (left == null) {
      return null;
    }

    Tuple tuple = (Tuple) operands[1];
    for (IExpression expression : tuple) {
      Object value = expression.getValue(context);
      if (Converter.compare(left, value) == 0) {
        return Boolean.TRUE;
      }
    }

    return Boolean.FALSE;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" IN ");
    operands[1].unparse(writer);
  }
}
