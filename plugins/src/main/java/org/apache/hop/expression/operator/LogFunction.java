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
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Returns the specified base logarithm of a numeric value.
 */
@FunctionPlugin
public class LogFunction extends Function {

  public LogFunction() {
    super("LOG", true, ReturnTypes.NUMBER, OperandTypes.NUMERIC_NUMERIC,
        OperatorCategory.TRIGONOMETRY, "/docs/log.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object base = operands[0].getValue(context);
    if (base == null)
      return null;

    Object value = operands[1].getValue(context);
    if (value == null)
      return null;
    Double number = Coerce.toNumber(value);
    if (number <= 0)
      throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, value);

    return FastMath.log(number) / FastMath.log(Coerce.toNumber(base));
  }
}
