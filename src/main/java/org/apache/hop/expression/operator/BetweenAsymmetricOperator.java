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

import java.io.StringWriter;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/** <code>BETWEEN ASYMMETRIC</code> operator. */
public class BetweenAsymmetricOperator extends Operator {
  public static final BetweenAsymmetricOperator INSTANCE = new BetweenAsymmetricOperator();

  public BetweenAsymmetricOperator() {
    super(
        "BETWEEN",
        120,
        true,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.BETWEEN,
        OperatorCategory.COMPARISON,
        "/docs/between.html");
  }

  @Override
  public boolean coerceOperandsType(Call call) {
    return Types.coercionComparisonOperator(call);
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object value = operands[0].getValue();
    Object start = operands[1].getValue();
    Object end = operands[2].getValue();

    if (value == null || start == null || end == null) {
      return null;
    }

    Type type = operands[0].getType();
    return type.compare(value, start) >= 0 && type.compare(value, end) <= 0;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" BETWEEN ");
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" AND ");
    operands[2].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
