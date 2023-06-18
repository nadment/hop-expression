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
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Comparison;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * <code>BETWEEN</code> operator.
 */
public class BetweenOperator extends Operator {

  public enum Between {
    /** The lower bound and upper bound must be in order */
    ASYMMETRIC,
    /** The lower bound and upper bound order is irrelevant */
    SYMMETRIC
  }

  public final Between between;

  public BetweenOperator(Between between) {
    super("BETWEEN", 120, true, ReturnTypes.BOOLEAN, OperandTypes.SAME_SAME_SAME,
        OperatorCategory.COMPARISON, "/docs/between.html");
    this.between = between;
  }

  @Override
  public Object eval(IExpression[] operands) throws Exception {
    Object value = operands[0].getValue();
    Object start = operands[1].getValue();
    Object end = operands[2].getValue();

    if (value == null || start == null || end == null) {
      return null;
    }

    // If lower bound is greater than upper bound
    if (between == Between.SYMMETRIC && Comparison.compare(start, end) >= 0) {
      return Comparison.compare(value, end) >= 0 && Comparison.compare(value, start) <= 0;
    }

    return Comparison.compare(value, start) >= 0 && Comparison.compare(value, end) <= 0;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" BETWEEN ");
    if (between == Between.SYMMETRIC) {
      writer.append("SYMMETRIC ");
    }
    operands[1].unparse(writer);
    writer.append(" AND ");
    operands[2].unparse(writer);
  }
}
