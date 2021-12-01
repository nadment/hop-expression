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

import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionList;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import java.io.StringWriter;

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
public class In extends Operator {

  public In() throws ExpressionException {
    super("IN", 120, true, true, "i18n::Operator.Category.Comparison");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object left = operands[0].eval(context);
    if (left == null) {
      return null;
    }

    ExpressionList list = (ExpressionList) operands[1];
    for (IExpression expression : list) {
      Object value = expression.eval(context);
      if (compareTo(left, value) == 0) {
        return Boolean.TRUE;
      }
    }

    return Boolean.FALSE;
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    operands[0].write(writer);
    writer.append(" IN ");
    operands[1].write(writer);
  }
}
