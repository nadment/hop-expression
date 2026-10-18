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

import java.util.Locale;
import org.apache.commons.codec.language.Soundex;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Characters;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Returns a string that contains a phonetic representation of the input string. */
@FunctionPlugin
@NullMarked
public class SoundexFunction extends Function {

  private static final Soundex SOUNDEX = new Soundex();

  public SoundexFunction() {
    super(
        "SOUNDEX",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING,
        OperatorCategory.STRING,
        "/docs/soundex.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) return null;
    return soundex(value);
  }

  /**
   * Returns the Soundex code of a string, the diacritic marks are removed first and the Latin
   * letters that have no decomposition, like Ø or Æ, are transliterated.
   *
   * @param value the string
   * @return the Soundex code, empty if the string has no letter
   */
  public static String soundex(final String value) {
    String latin = Characters.transliterate(Characters.unaccent(value));
    try {
      return SOUNDEX.soundex(latin);
    } catch (IllegalArgumentException e) {
      // Soundex only maps the letters A to Z, report the first other letter
      String letter =
          latin
              .codePoints()
              .filter(Character::isLetter)
              .mapToObj(Character::toString)
              .filter(ch -> !ch.toUpperCase(Locale.ENGLISH).matches("[A-Z]+"))
              .findFirst()
              .orElse(latin);
      throw new ExpressionException(ErrorCode.UNSUPPORTED_SOUNDEX_LETTER, letter);
    }
  }
}
