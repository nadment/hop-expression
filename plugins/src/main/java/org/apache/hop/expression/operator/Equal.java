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

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import java.io.StringWriter;

/** 
 * Comparison equals operator.
 * <br><strong>Syntax:</strong> <code>x = y</code>
 */
public class Equal extends Operator {

  public Equal() {
    super("EQUAL", "=", 130, true, true, "i18n::Operator.Category.Comparison", "/docs/equal.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    // Treats NULLs as unknown values
    // NULL is not equal ( = ) to anything—not even to another NULL.
    Object left = operands[0].eval(context);
    if (left == null) {
      return null;
    }
    Object right = operands[1].eval(context);
    if (right == null) {
      return null;
    }
    return DataType.compareTo(left, right) == 0;
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    operands[0].write(writer);
    writer.append('=');
    operands[1].write(writer);
  }
}
