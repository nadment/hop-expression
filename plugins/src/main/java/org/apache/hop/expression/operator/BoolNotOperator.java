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
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.io.StringWriter;

/**
 * Logical negation <code>NOT</code> operator
 *
 * <p>
 * Syntax of the operator:
 *
 * <ul>
 * <li><code>field [NOT] TRUE</code>
 * <li><code>field [NOT] IN (list of values)</code>
 * <li><code>field [NOT] BETWEEN start AND end</code>
 * </ul>
 */
public class BoolNotOperator extends Operator {

  public BoolNotOperator() {
    super("BOOLNOT", "NOT", 150, false, true, ReturnTypes.BOOLEAN, OperandTypes.BOOLEAN, "i18n::Operator.Category.Logical",
        "/docs/boolnot.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].getValue(context);
    if (value == null) {
      return null;
    }
    return !Coerse.toBoolean(value);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("NOT ");
    operands[0].unparse(writer);
  }
}