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

/** An operator describing the <code>CASE</code> operator. */
public class Case extends Operator {

  public Case() {
    super("CASE_WHEN", "CASE", 120, true, "i18n::Operator.Category.Conditional");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    int index = 0;
    IExpression switchExpression = operands[0];
    ExpressionList whenList = (ExpressionList) operands[1];
    ExpressionList thenList = (ExpressionList) operands[2];
    IExpression elseExpression = operands[3];

    if (switchExpression == null) {
      for (IExpression whenOperand : whenList) {
        Object condition = whenOperand.eval(context);
        if (coerceToBoolean(condition)) {
          return thenList.get(index).eval(context);
        }
        index++;
      }
    } else {
      Object condition = switchExpression.eval(context);
      for (IExpression whenOperand : whenList) {
        Object value = whenOperand.eval(context);
        if (compareTo(condition, value) == 0) {
          return thenList.get(index).eval(context);
        }
        index++;
      }
    }

    return elseExpression.eval(context);
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    writer.append("CASE");

    // Form switch expression
    IExpression switchExpression = operands[0];
    if (switchExpression != null) {
      writer.append(' ');
      switchExpression.write(writer);
    }

    int index = 0;
    ExpressionList whenList = (ExpressionList) operands[1];
    ExpressionList thenList = (ExpressionList) operands[2];
    for (IExpression whenOperand : whenList) {
      writer.append(" WHEN ");
      whenOperand.write(writer);
      IExpression thenOperand = thenList.get(index++);
      writer.append(" THEN ");
      thenOperand.write(writer);
    }

    IExpression elseExpression = operands[3];
    if (elseExpression != null) {
      writer.append(" ELSE ");
      elseExpression.write(writer);
    }
    writer.append(" END");
  }
}