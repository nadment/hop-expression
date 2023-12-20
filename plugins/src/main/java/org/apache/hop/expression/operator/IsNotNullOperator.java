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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * An operator describing the <code>IS NOT NULL</code> operator.
 */
public class IsNotNullOperator extends Operator {

  public IsNotNullOperator() {
    super("IS NOT NULL", 140, true, ReturnTypes.BOOLEAN_NOT_NULL, OperandTypes.ANY, OperatorCategory.COMPARISON,
        "/docs/is-null.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression operand = call.getOperand(0);

    // CAST(x AS type) IS NOT NULL → x IS NOT NULL
    if (operand.is(Operators.CAST)) {
      return new Call(call.getOperator(), operand.asCall().getOperands());
    }

    // If the operand is not nullable, always true
    if (!operand.getType().isNullable()) {
      return Literal.TRUE;
    }
    
    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object value = operands[0].getValue();
    return value != null;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" IS NOT NULL");
  }
}
