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

import java.io.StringWriter;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Bitwise NOT operator. <br>
 * <strong>Syntax:</strong> <code>~x</code>
 */
@FunctionPlugin
public class BitNotFunction extends Function {

  public static final BitNotFunction INSTANCE = new BitNotFunction("~");

  public BitNotFunction() {
    super(
        "BIT_NOT",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.INTEGER,
        OperatorCategory.BITWISE,
        "/docs/bit_not.html");
  }

  public BitNotFunction(String name) {
    super(
        "BIT_NOT",
        name,
        40,
        Associativity.LEFT,
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.INTEGER,
        OperatorCategory.BITWISE,
        "/docs/bit_not.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression operand = call.getOperand(0);

    // Simplify reverses itself "~(~(A))" to "A"
    if (operand.isOperator(BitNotFunction.INSTANCE)) {
      return call(operand).getOperand(0);
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Long value = operands[0].getValue(Long.class);
    if (value == null) return null;

    return ~value;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append('~');
    operands[0].unparse(writer, 0, 0);
  }
}
