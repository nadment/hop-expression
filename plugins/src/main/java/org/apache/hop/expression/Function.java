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

import org.apache.hop.i18n.BaseMessages;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Objects;

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
   * @param name The name of function
   * @param alias The alias of function or null if none
   */
  public Function(String name, String alias, boolean isDeterministic, Object instance,
      Method method, int min, int max, String category) throws ExpressionException {
    super(name, alias, 10, true, isDeterministic, category);

    this.instance = instance;
    this.method = method;
    this.minArgs = min;
    this.maxArgs = max;
  }

  @Override
  public boolean equals(Object o) {
    if (o == null) {
      return false;
    }
    if (o == this) {
      return true;
    }
    if (this.getClass() != o.getClass()) {
      return false;
    }
    Function fx = (Function) o;
    return name.equals(fx.name) && (alias != null && alias.equals(fx.alias));
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, alias);
  }

  /**
   * Check if the number of arguments is correct.
   *
   * @param len the number of arguments set
   * @throws error if not enough or too many arguments
   */
  public void checkNumberOfArguments(List<IExpression> operands) throws ExpressionException {

    if (operands.size() < minArgs) {
      throw ExpressionException.create("Expression.NotEnoughArguments", this.getName());
    }

    if (operands.size() > maxArgs) {
      throw ExpressionException.create("Expression.TooManyNumberOfArguments", this.getName());
    }
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    try {
      return method.invoke(instance, context, operands);
    } catch (Exception e) {
      if (e.getCause() instanceof ExpressionException) {
        throw (ExpressionException) e.getCause();
      }
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Expression.FunctionError", this.getName(), e.getMessage()),
          e);
    }
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    boolean first = true;
    for (IExpression operand : operands) {
      if (!first)
        writer.append(',');
      else
        first = false;
      operand.write(writer);
    }
    writer.append(')');
  }
}


