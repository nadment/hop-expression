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
import org.apache.hop.expression.ExpressionException;
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
 * Returns the values rounded to the nearest equal or larger integer.
 */
@FunctionPlugin(id = "CEILING", names="CEIL", category = "i18n::Operator.Category.Mathematical", documentationUrl = "/docs/ceiling.html")
public class CeilingFunction extends Function {

  public CeilingFunction() {
    super("CEILING", true, ReturnTypes.ARG0, OperandTypes.NUMERIC, "i18n::Operator.Category.Mathematical", "/docs/ceiling.html");
  }
  
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].getValue(context);
    if (value == null)
      return null;
    if (value instanceof Long)
      return value;
    if (value instanceof BigDecimal) {
      return Coerse.toBigNumber(value).setScale(0, RoundingMode.CEILING);
    }
    return FastMath.ceil(Coerse.toNumber(value));
  }

}