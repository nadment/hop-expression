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

import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * The COALESCE function returns the first of its arguments that is not null. Null is returned
 * only if all arguments are null.
 */
@FunctionPlugin(id = "COALESCE", category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/coalesce.html")
public class CoalesceFunction extends Function {

  public CoalesceFunction() {
    super("COALESCE", true, ReturnTypes.FIRST_KNOWN, OperandTypes.AT_LEAST_ONE_SAME_VARIADIC, "i18n::Operator.Category.Conditional", "/docs/coalesce.html");
  }
  
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    for (IExpression operand : operands) {
      Object value = operand.getValue(context);
      if (value != null)
        return value;
    }

    return null;
  }
}
