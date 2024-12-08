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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Characters;

/**
 * Trims white space from the beginning and end of the string, and replaces all other white space
 * with single blanks.
 *
 * <p>White space is defined as any sequence of blanks, null characters, newlines (line feeds),
 * carriage returns, horizontal tabs and form feeds (vertical tabs).
 */
@FunctionPlugin
public class SqueezeFunction extends Function {

  public static final SqueezeFunction INSTANCE = new SqueezeFunction();

  public SqueezeFunction() {
    super(
        "SQUEEZE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING,
        OperatorCategory.STRING,
        "/docs/squeeze.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    IExpression operand = call.getOperand(0);

    // Repetitions of functions that do not have any effects on the result
    if (operand.isOperator(call.getOperator())) {
      return new Call(call.getOperator(), call(operand).getOperand(0));
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) return null;
    final int size = value.length();
    final char[] newChars = new char[size];
    int count = 0;
    int whitespacesCount = 0;
    boolean startWhitespaces = true;
    for (int i = 0; i < size; i++) {
      final char c = value.charAt(i);
      if (Characters.isSpace(c)) {
        if (whitespacesCount == 0 && !startWhitespaces) {
          newChars[count++] = ' ';
        }
        whitespacesCount++;
      } else {
        startWhitespaces = false;
        newChars[count++] = c;
        whitespacesCount = 0;
      }
    }
    if (startWhitespaces) {
      return "";
    }
    return new String(newChars, 0, count - (whitespacesCount > 0 ? 1 : 0));
  }
}
