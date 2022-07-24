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
import org.apache.hop.expression.util.Coerse;
import java.io.StringWriter;

/** 
 * Logical <code>XOR</code> operator.
 */
public class BoolXor extends Operator {

  public BoolXor() {
    super("BOOLXOR", "XOR", 170, true, true, "i18n::Operator.Category.Logical", "/docs/boolxor.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object left = operands[0].eval(context);
    Object right = operands[1].eval(context);
    if (left == null) {
      return Coerse.toBoolean(right);
    }
    if (right == null) {
      return Coerse.toBoolean(left);
    }
    return Boolean.logicalXor(Coerse.toBoolean(left), Coerse.toBoolean(right));
  }
  
  @Override
  public boolean isSymmetrical() {
    return true;
  }

  @Override
  public IReturnTypeInference getReturnTypeInference() {
    return ReturnTypes.BOOLEAN;
  }
  
  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" XOR ");
    operands[1].unparse(writer);
  }
}
