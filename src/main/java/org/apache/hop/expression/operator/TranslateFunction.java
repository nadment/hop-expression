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
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** */
@FunctionPlugin
@NullMarked
public class TranslateFunction extends Function {

  /**
   * The characters to find and their replacements, as code points, converted ahead of evaluation
   * when they are constant, otherwise {@code null}.
   */
  private final int @Nullable [] find;

  private final int @Nullable [] replace;

  public TranslateFunction() {
    this(null, null);
  }

  private TranslateFunction(int @Nullable [] find, int @Nullable [] replace) {
    super(
        "TRANSLATE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING_STRING_STRING,
        OperatorCategory.STRING,
        "/docs/hop-user-manual/expression/translate.html");
    this.find = find;
    this.replace = replace;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Convert constant characters once, not for each row
    if (find == null && call.getOperand(1).isConstant() && call.getOperand(2).isConstant()) {
      String findChars = call.getOperand(1).getValue(String.class);
      // A null value is handled at evaluation, which returns null
      if (findChars != null) {
        String replaceChars = call.getOperand(2).getValue(String.class);
        return new Call(
            new TranslateFunction(codePoints(findChars), codePoints(replaceChars)),
            call.getOperands());
      }
    }
    return call;
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String string = operands[0].getValue(String.class);
    if (string == null) return null;

    if (find != null && replace != null) {
      return translate(string, find, replace);
    }

    String findChars = operands[1].getValue(String.class);
    if (findChars == null) return null;
    String replaceChars = operands[2].getValue(String.class);
    return translate(string, codePoints(findChars), codePoints(replaceChars));
  }

  /**
   * Returns the characters of a string as code points, a surrogate pair is a single character.
   *
   * <p>If the replacement characters are null, they are treated as an empty string, so all the
   * characters to find are removed.
   */
  private static int[] codePoints(final @Nullable String chars) {
    return (chars == null) ? new int[0] : chars.codePoints().toArray();
  }

  private static String translate(final String string, final int[] find, final int[] replace) {
    StringBuilder buffer = new StringBuilder(string.length());
    int length = string.length();
    for (int i = 0; i < length; ) {
      int ch = string.codePointAt(i);
      i += Character.charCount(ch);

      int index = indexOf(find, ch);
      if (index < 0) {
        buffer.appendCodePoint(ch);
      } else if (index < replace.length) {
        // If replace is shorter than find, then the characters without replacement are removed
        buffer.appendCodePoint(replace[index]);
      }
    }
    return buffer.toString();
  }

  private static int indexOf(final int[] chars, final int ch) {
    for (int i = 0; i < chars.length; i++) {
      if (chars[i] == ch) return i;
    }
    return -1;
  }
}
