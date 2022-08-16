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
 * 
 */
@FunctionPlugin(id = "NVL2", category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/nvl2.html")
public class Nvl2Function extends Function {

  public Nvl2Function() {
    super("NVL2", true, ReturnTypes.ARG1_OR_ARG2, OperandTypes.ANY_ANY_ANY, "i18n::Operator.Category.Conditional", "/docs/nvl2.html");
    // TODO: OperandTypes.BOOLEAN_SAME_SAME
  }
 
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object condition = operands[0].getValue(context);

    if (condition == null) {
      return operands[2].getValue(context);
    }

    return operands[1].getValue(context);
  }
}