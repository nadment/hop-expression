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
 * Returns the portion of the string from string, starting from the character/byte specified by
 * start, with optionally limited length.
 */
@FunctionPlugin(names = {"SUBSTR"})
public class SubstringFunction extends Function {

  public SubstringFunction() {
    super("SUBSTRING", true, ReturnTypes.STRING, OperandTypes.STRING_NUMERIC_OPTIONAL_STRING, "i18n::Operator.Category.String", "/docs/substring.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    String string = Coerse.toString(operands[0].getValue(context));
    int length = string.length();
    int start = Coerse.toInteger(operands[1].getValue(context)).intValue();

    // These compatibility conditions violate the Standard
    if (start == 0) {
      start = 1;
    } else if (start < 0) {
      start = length + start + 1;
    }

    // Only 2 operands
    if (operands.length == 2) {
      return string.substring(start - 1);
    }

    int end = start + Coerse.toInteger(operands[2].getValue(context)).intValue();
    // SQL Standard requires "data exception - substring error" when
    // end < start but expression does not throw it for compatibility
    start = Math.max(start, 1);
    end = Math.min(end, length + 1);
    if (start > length || end <= start) {
      // TODO: option to treatEmptyStringsAsNull
      return null;
    }

    return string.substring(start - 1, end - 1);
  }
}
