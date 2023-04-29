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

import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Returns a number raised to the specified power exponent.
 */
@FunctionPlugin(names = "POW")
public class PowerFunction extends Function {

  public PowerFunction() {
    super("POWER", ReturnTypes.NUMBER, OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.MATHEMATICAL, "/docs/power.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Double number = operands[0].getValue(context, Double.class);
    if (number == null) {
      return null;
    }
    Double exponent = operands[1].getValue(context, Double.class);
    if (exponent == null) {
      return null;
    }

    return FastMath.pow(number, exponent);
  }
}
