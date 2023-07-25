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

import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Trims white space from the beginning and end of the string, and replaces all other white space
 * with single blanks.
 * 
 * White space is defined as any sequence of blanks, null characters, newlines (line feeds),
 * carriage returns, horizontal tabs and form feeds (vertical tabs).
 */
@FunctionPlugin
public class SqueezeFunction extends Function {

  public SqueezeFunction() {
    super("SQUEEZE", ReturnTypes.STRING, OperandTypes.STRING, Category.STRING,
        "/docs/squeeze.html");
  }

  @Override
  public Object eval(IExpression[] operands)
      throws Exception {
    String value = operands[0].getValue(String.class);
    if (value == null)
      return null;

    char[] a = value.toCharArray();
    int n = 1;
    for (int i = 1; i < a.length; i++) {
      a[n] = a[i];
      if (!Character.isSpaceChar(a[n]))
        n++;
      else {
        a[n] = ' ';
        if (a[n - 1] != ' ')
          n++;
      }
    }

    return new String(a, 0, n);
  }
}
