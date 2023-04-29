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
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * Logical conjunction <code>AND</code> operator.
 */
public class BoolAndOperator extends Operator {

  public BoolAndOperator() {
    super("BOOLAND", "AND", 160, true, ReturnTypes.BOOLEAN, OperandTypes.BOOLEAN_BOOLEAN,
        OperatorCategory.LOGICAL, "/docs/booland.html");
  }

  /**
   * Simplifies AND expressions whose answer can be determined without evaluating both sides.
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    boolean left = true;
    boolean right = true;

    if (call.getOperand(0).is(Kind.LITERAL)) {
      Boolean value = call.getOperand(0).getValue(context, Boolean.class);
      if (value == null)
        return Literal.NULL;
      if (value == Boolean.FALSE)
        left = false;
    }

    if (call.getOperand(1).is(Kind.LITERAL)) {
      Boolean value = call.getOperand(1).getValue(context, Boolean.class);
      if (value == null)
        return Literal.NULL;
      if (value == Boolean.FALSE)
        right = false;
    }

    // FALSE AND x => FALSE
    // x AND FALSE => FALSE
    if (!left || !right) {
      return Literal.FALSE;
    }

    // x AND x => x
    if (call.getOperand(0).equals(call.getOperand(1))) {
      return call.getOperand(0);
    }

    return call;
  }
  
  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    Boolean left = operands[0].getValue(context, Boolean.class);
    if (left == null) {
      return null;
    }
    Boolean right = operands[1].getValue(context, Boolean.class);
    if (right == null) {
      return null;
    }
    return Boolean.logicalAnd(left, right);
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
