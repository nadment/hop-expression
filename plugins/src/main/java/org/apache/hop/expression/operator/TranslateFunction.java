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

import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * 
 */
@FunctionPlugin
public class TranslateFunction extends Function {

  public TranslateFunction() {
    super("TRANSLATE", true, ReturnTypes.STRING, OperandTypes.STRING_STRING_STRING,
        OperatorCategory.STRING, "/docs/translate.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].getValue(context);
    if (v1 == null)
      return null;
    Object v2 = operands[2].getValue(context);
    if (v2 == null)
      return null;

    String string = Coerce.toString(v0);
    String findChars = Coerce.toString(v1);
    String replaceChars = Coerce.toString(v2);

    StringBuilder buffer = new StringBuilder(string.length());
    // if shorter than findChars, then characters are removed
    // (if null, we don't access replaceChars at all)
    if (replaceChars == null)
      replaceChars = "";
    int replaceSize = replaceChars.length();

    for (int i = 0, size = string.length(); i < size; i++) {
      char ch = string.charAt(i);
      int index = findChars.indexOf(ch);
      if (index >= 0) {
        if (index < replaceSize) {
          buffer.append(replaceChars.charAt(index));
        }
      } else {
        buffer.append(ch);
      }
    }
    return buffer.toString();
  }
}
