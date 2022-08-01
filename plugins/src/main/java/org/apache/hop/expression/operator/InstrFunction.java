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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;

/**
 * Returns the position in the string that is the first character of a specified occurrence of the
 * substring.
 */
@FunctionPlugin(id = "INSTR", category = "i18n::Operator.Category.String", documentationUrl = "/docs/instr.html")
public class InstrFunction extends Function {

  public InstrFunction() {
    super("INSTR", true, ReturnTypes.INTEGER, OperandTypes.STRING_STRING_OPTIONAL_NUMERIC, "i18n::Operator.Category.String", "/docs/instr.html");
  }
  
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].getValue(context);
    Object v1 = operands[1].getValue(context);

    if (v0 == null || v1 == null) {
      return null;
    }

    String str = Coerse.toString(v0);
    String substr = Coerse.toString(v1);

    // If 3 operands
    int start = 0;
    if (operands.length == 3) {
      start = Coerse.toInteger(operands[2].getValue(context)).intValue();

      if (start > 0)
        start -= 1;
      else if (start < 0) {
        return Long.valueOf(str.lastIndexOf(substr, str.length() + start) + 1L);
      }
    }

    return Long.valueOf(str.indexOf(substr, start) + 1L);
  }

}
