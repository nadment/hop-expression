/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression.operator;

import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.IExpressionProcessor;
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Returns the first value over a group of rows.
 */
@FunctionPlugin
public class FirstValueFunction extends AggregateFunction {

  public FirstValueFunction() {
    super("FIRST_VALUE", ReturnTypes.ARG0, OperandTypes.ANY_OPTIONAL_BOOLEAN,
        "/docs/first_value.html");
  }

  @Override
  public IExpressionProcessor createProcessor(IExpressionContext context, IExpression[] operands) {
    boolean ignoreNull = false;

    if (operands.length == 2) {
      try {
        ignoreNull = Coerce.toBoolean(operands[1].getValue(context));
      } catch (ExpressionException e) {
        // Ignore
      }
    }

    return new FirstValueProcessor(ignoreNull);
  }
}
