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

import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Round down numeric expressions or truncates.
 */
@FunctionPlugin(names = "TRUNC")
public class TruncateFunction extends Function {

  public TruncateFunction() {
    super("TRUNCATE", true, ReturnTypes.BIGNUMBER, OperandTypes.NUMERIC_OPTIONAL_NUMERIC,
        "i18n::Operator.Category.Mathematical", "/docs/truncate.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object value = operands[0].getValue(context);
    if (value == null)
      return null;

    BigDecimal number = Coerse.toBigNumber(value);
    int scale = 0;
    if (operands.length == 2) {
      Object pattern = operands[1].getValue(context);
      if (pattern == null)
        return null;
      scale = Coerse.toInteger(pattern).intValue();
    }

    if (scale > number.scale())
      scale = number.scale();
    return number.movePointRight(scale).setScale(0, RoundingMode.DOWN).movePointLeft(scale);
  }

}
