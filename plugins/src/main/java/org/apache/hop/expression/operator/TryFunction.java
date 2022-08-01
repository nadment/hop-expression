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
 * This function returns the value of expression. If an error occurs, null is returned.
 */
@FunctionPlugin(id = "TRY", category = "i18n::Operator.Category.Special", documentationUrl = "/docs/try.html")
public class TryFunction extends Function {

  public TryFunction() {
    super("TRY", true, ReturnTypes.ARG0, OperandTypes.ANY, "i18n::Operator.Category.Special", "/docs/try.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {

    try {
      return operands[0].getValue(context);
    } catch (Exception e) {
      return null;
    }
  }
}
