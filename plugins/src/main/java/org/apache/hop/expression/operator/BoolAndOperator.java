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
 * Logical conjunction <code>AND</code> operator.
 */
public class BoolAndOperator extends Operator {

  public BoolAndOperator() {
    super("BOOLAND", "AND", 160, true, true, ReturnTypes.BOOLEAN, OperandTypes.BOOLEAN_BOOLEAN, "i18n::Operator.Category.Logical",
        "/docs/booland.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object left = operands[0].getValue(context);
    if (left == null) {
      return null;
    }
    Object right = operands[1].getValue(context);
    if (right == null) {
      return null;
    }
    return Boolean.logicalAnd(Coerse.toBoolean(left), Coerse.toBoolean(right));
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" AND ");
    operands[1].unparse(writer);
  }
}
