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

/**
 * 
 */
@FunctionPlugin(id = "POWER", category = "i18n::Operator.Category.Mathematical", documentationUrl = "/docs/power.html")
public class PowerFunction extends Function {

  public PowerFunction() {
    super("POWER", true, ReturnTypes.NUMBER, OperandTypes.NUMERIC_NUMERIC, "i18n::Operator.Category.Mathematical", "/docs/power.html");
  }
  
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].getValue(context);
    Object v1 = operands[1].getValue(context);
    if (v0 == null || v1 == null) {
      return null;
    }
    
    Double number = Coerse.toNumber(v0);
    Double exponent = Coerse.toNumber(v1);
        
    if (exponent == 0)
      return 1L;

    return FastMath.pow(number, exponent);
  }
}
