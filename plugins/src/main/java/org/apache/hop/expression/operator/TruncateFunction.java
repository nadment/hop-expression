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

import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Round down numeric expressions or truncates.
 */
@FunctionPlugin(names = "TRUNC")
public class TruncateFunction extends Function {

  public TruncateFunction() {
    super("TRUNCATE", ReturnTypes.NUMBER, OperandTypes.NUMERIC_OPTIONAL_NUMERIC,
        Category.MATHEMATICAL, "/docs/truncate.html");
  }

  @Override
  public Object eval(IExpression[] operands)
      throws Exception {
    BigDecimal value = operands[0].getValue(BigDecimal.class);
    if (value == null)
      return null;

    int scale = 0;
    if (operands.length == 2) {
      Long pattern = operands[1].getValue(Long.class);
      if (pattern == null)
        return null;
      scale = pattern.intValue();
    }

    if (scale > value.scale())
      scale = value.scale();
    return value.movePointRight(scale).setScale(0, RoundingMode.DOWN).movePointLeft(scale);
  }
}
