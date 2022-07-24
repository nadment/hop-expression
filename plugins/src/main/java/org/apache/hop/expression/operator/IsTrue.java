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
import org.apache.hop.expression.IReturnTypeInference;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.ReturnTypes;
import java.io.StringWriter;

/**
 * An operator describing the <code>IS TRUE</code> operator.
 */
public class IsTrue extends Operator {

  public IsTrue() {
    super("IS TRUE", 140, true, true, "i18n::Operator.Category.Comparison", "/docs/isTrue.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);

    if (value == Boolean.TRUE) {
      return Boolean.TRUE;
    }
    return Boolean.FALSE;
  }

  @Override
  public IReturnTypeInference getReturnTypeInference() {
    return ReturnTypes.BOOLEAN;
  }
  
  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" IS TRUE");
  }
}
