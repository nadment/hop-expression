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

import ch.obermuhlner.math.big.BigDecimalMath;
import java.math.BigDecimal;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/** Calculates the arc tangent (inverted tangent) of y / x in the range -pi to pi. */
@FunctionPlugin
public class Atan2Function extends Function {

  public Atan2Function() {
    super(
        "ATAN2",
        ReturnTypes.NUMBER_NULLABLE,
        OperandTypes.NUMBER_NUMBER,
        OperatorCategory.TRIGONOMETRY,
        "/docs/atan2.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    BigDecimal value0 = operands[0].getValue(BigDecimal.class);
    if (value0 == null) return null;
    BigDecimal value1 = operands[1].getValue(BigDecimal.class);
    if (value1 == null) return null;

    return BigDecimalMath.atan2(value0, value1, MATH_CONTEXT);
  }
}
