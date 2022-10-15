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
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.text.Normalizer;
import java.util.regex.Pattern;

/**
 * The function removes accents (diacritic marks) from a given string.
 * Note that ligatures will be left as is.
 */
@FunctionPlugin
public class UnaccentFunction extends Function {

  private static final Pattern DIACRITICS =
      Pattern.compile("[\\p{InCombiningDiacriticalMarks}\\p{IsLm}\\p{IsSk}]+");

  public UnaccentFunction() {
    super("UNACCENT", true, ReturnTypes.STRING, OperandTypes.STRING,
        "i18n::Operator.Category.String", "/docs/unaccent.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;

    String str = Coerse.toString(v0);

    final StringBuilder decomposed =
        new StringBuilder(Normalizer.normalize(str, Normalizer.Form.NFD));

    for (int i = 0; i < decomposed.length(); i++) {
      if (decomposed.charAt(i) == '\u0141') {
        decomposed.deleteCharAt(i);
        decomposed.insert(i, 'L');
      } else if (decomposed.charAt(i) == '\u0142') {
        decomposed.deleteCharAt(i);
        decomposed.insert(i, 'l');
      }
    }

    // Note that this doesn't correctly remove ligatures...
    return DIACRITICS.matcher(decomposed).replaceAll("");
  }

}
