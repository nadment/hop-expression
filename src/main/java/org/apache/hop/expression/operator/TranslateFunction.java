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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** */
@FunctionPlugin
@NullMarked
public class TranslateFunction extends Function {

  public TranslateFunction() {
    super(
        "TRANSLATE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING_STRING_STRING,
        OperatorCategory.STRING,
        "/docs/hop-user-manual/expression/translate.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String string = operands[0].getValue(String.class);
    if (string == null) return null;
    String findChars = operands[1].getValue(String.class);
    if (findChars == null) return null;
    String replaceChars = operands[2].getValue(String.class);
    // if shorter than findChars, then characters are removed
    // (if null, we don't access replaceChars at all)
    if (replaceChars == null) replaceChars = "";
    StringBuilder buffer = new StringBuilder(string.length());

    // The characters are code points, a surrogate pair is a single character
    int[] find = findChars.codePoints().toArray();
    int[] replace = replaceChars.codePoints().toArray();

    string
        .codePoints()
        .forEach(
            ch -> {
              int index = indexOf(find, ch);
              if (index < 0) {
                buffer.appendCodePoint(ch);
              } else if (index < replace.length) {
                buffer.appendCodePoint(replace[index]);
              }
            });
    return buffer.toString();
  }

  private static int indexOf(final int[] chars, final int ch) {
    for (int i = 0; i < chars.length; i++) {
      if (chars[i] == ch) return i;
    }
    return -1;
  }
}
