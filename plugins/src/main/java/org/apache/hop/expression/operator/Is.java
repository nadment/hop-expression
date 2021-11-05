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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import java.io.StringWriter;

/**
 * An operator describing the <code>IS</code> operator.
 *
 * <p>
 * Syntax of the operator:
 *
 * <ul>
 * <li><code>field IS TRUE</code>
 * <li><code>field IS FALSE</code>
 * <li><code>field IS NULL</code>
 * </ul>
 */
public class Is extends Operator {

  public Is() {
    super("IS", 140, true, "i18n::Operator.Category.Comparison");
  }


  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object left = operands[0].eval(context);
    Object right = operands[1].eval(context);

    if (left == null) {
      if (right == null) {
        return Boolean.TRUE;
      }
      return Boolean.FALSE;
    }

    return left.equals(right);
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    operands[0].write(writer);
    writer.append(" IS ");
    operands[1].write(writer);
  }
}
