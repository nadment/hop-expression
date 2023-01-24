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

import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Tuple;
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/** An operator describing the <code>CASE</code> operator. */
public class CaseOperator extends Operator {

  public CaseOperator() {
    super("CASE", 120, true, true, ReturnTypes.UNKNOWN, OperandTypes.CASE,
        OperatorCategory.CONDITIONAL, "/docs/case.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    int index = 0;
    IExpression switchExpression = operands[0];
    Tuple whenTuple = (Tuple) operands[1];
    Tuple thenTuple = (Tuple) operands[2];
    IExpression elseExpression = operands[3];

    if (switchExpression == null) {
      for (IExpression whenOperand : whenTuple) {
        Object condition = whenOperand.getValue(context);
        if (Coerce.isTrue(condition)) {
          return thenTuple.get(index).getValue(context);
        }
        index++;
      }
    } else {
      Object condition = switchExpression.getValue(context);
      for (IExpression whenOperand : whenTuple) {
        Object value = whenOperand.getValue(context);
        if (Coerce.compare(condition, value) == 0) {
          return thenTuple.get(index).getValue(context);
        }
        index++;
      }
    }

    return elseExpression.getValue(context);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("CASE");

    // Form switch expression
    IExpression switchExpression = operands[0];
    if (switchExpression != null) {
      writer.append(' ');
      switchExpression.unparse(writer);
    }

    int index = 0;
    Tuple whenTuple = (Tuple) operands[1];
    Tuple thenTuple = (Tuple) operands[2];
    for (IExpression whenOperand : whenTuple) {
      writer.append(" WHEN ");
      whenOperand.unparse(writer);
      IExpression thenOperand = thenTuple.get(index++);
      writer.append(" THEN ");
      thenOperand.unparse(writer);
    }

    IExpression elseExpression = operands[3];
    if (elseExpression != null) {
      writer.append(" ELSE ");
      elseExpression.unparse(writer);
    }
    writer.append(" END");
  }
}
