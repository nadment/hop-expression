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
package org.apache.hop.expression;

import java.io.StringWriter;
import java.lang.reflect.Method;

/** A <code>Function</code> is a type of operator which has conventional function-call syntax. */

public class Function extends Operator {

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  private final Object instance;
  private final Method method;
  private final int minArgs;
  private final int maxArgs;

  /**
   * Creates an function operator.
   *
   * Note that some operator has syntax of function CAST, TRY_CAST, CONCAT, EXTRACT.
   * 
   * @param id The unique identifier of the function
   * @param name The name of function
   */
  public Function(String id, String name, boolean isDeterministic, Object instance,
      Method method, int min, int max, String category, String documentationUrl) {
    super(id, name, 10, true, isDeterministic, category, documentationUrl);

    this.instance = instance;
    this.method = method;
    this.minArgs = min;
    this.maxArgs = max;
  }

  /**
   * Check if the number of arguments is correct.
   *
   * @param len the number of arguments set
   * @throws error if not enough or too many arguments
   */
  @Override
  public void checkNumberOfArguments(IExpression[] operands) {

    if (operands.length < minArgs) {      
      throw new IllegalArgumentException(ExpressionError.NOT_ENOUGH_ARGUMENT.message(this.getId()));
    }

    if (operands.length > maxArgs) {
      throw new IllegalArgumentException(ExpressionError.TOO_MANY_ARGUMENT.message(this.getId()));
    }
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    try {
      return method.invoke(instance, context, operands);
    } catch (Exception e) {
      Throwable throwable = e.getCause();
      if (throwable instanceof ExpressionException) {
        throw (ExpressionException) throwable;
      }
      throw new ExpressionException(ExpressionError.FUNCTION_CALL_ERROR, this.getId(), throwable.getMessage());
    }
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    boolean first = true;
    for (IExpression operand : operands) {
      if (!first)
        writer.append(',');
      else
        first = false;
      operand.unparse(writer);
    }
    writer.append(')');
  }
}


