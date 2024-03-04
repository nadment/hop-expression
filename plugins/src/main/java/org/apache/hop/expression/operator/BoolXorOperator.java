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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * Logical exclusive disjunction <code>XOR</code> operator.
 * <ul>
 *  <li>True if one expression is non-zero and the other expression is zero.</li>
 *  <li>False if both expressions are non-zero or both expressions are zero.</li>
 *  <li>NULL if one or both expressions are NULL.</li>
 * </ul>
 */
public class BoolXorOperator extends Operator {

  public BoolXorOperator() {
    super("BOOLXOR", "XOR", 170, true, ReturnTypes.BOOLEAN_NULLABLE, OperandTypes.BOOLEAN_BOOLEAN,
        OperatorCategory.LOGICAL, "/docs/boolxor.html");
  } 
  
  /**
   * Simplifies XOR expressions whose answer can be determined without evaluating both sides.
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Boolean left = operands[0].getValue(Boolean.class);
    if (left == null) {
      return null;
    }

    Boolean right = operands[1].getValue(Boolean.class);
    if (right == null) {
        return null;
    }
    return Boolean.logicalXor(left, right);
  }

  @Override
  public boolean isSymmetrical() {
    return true;
  }
  
  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" XOR ");
    operands[1].unparse(writer);
  }
}
