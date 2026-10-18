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
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Splits a string on the specified list of delimiter characters and returns the part at the
 * specified position. @See {@link SplitPartFunction}
 */
@FunctionPlugin
@NullMarked
public class StrtokFunction extends Function {

  /** The maximum number of tokens, the last one holds the rest of the string. */
  private static final int MAX_TOKENS = 256;

  private static final IOperandTypeChecker OTC =
      OperandTypes.STRING
          .or(OperandTypes.STRING_STRING)
          .or(OperandTypes.STRING_INTEGER)
          .or(OperandTypes.STRING_STRING_INTEGER);

  public StrtokFunction() {
    super("STRTOK", ReturnTypes.STRING_NULLABLE, OTC, OperatorCategory.STRING, "/docs/strtok.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String str = operands[0].getValue(String.class);
    if (str == null) return null;

    // Default value
    String delimiter = " ";
    int index = 1;

    if (operands.length == 2) {
      Object v1 = operands[1].getValue();
      if (v1 == null) return null;

      if (v1 instanceof Number number) {
        // Clamp to the int range, the conversion would wrap a part number out of the int range
        index = Math.clamp(number.longValue(), Integer.MIN_VALUE, Integer.MAX_VALUE);
      } else {
        delimiter = (String) v1;
      }
    } else if (operands.length == 3) {
      delimiter = operands[1].getValue(String.class);
      if (delimiter == null) return null;

      Long v2 = operands[2].getValue(Long.class);
      if (v2 == null) return null;
      index = Math.clamp(v2, Integer.MIN_VALUE, Integer.MAX_VALUE);
    }

    // An empty string has no token
    if (str.isEmpty()) return null;

    // A negative part number counts from the end, only then the tokens are counted
    if (index < 0) index += countTokens(str, delimiter) + 1;

    // If the part index is out of range, the returned value is NULL.
    if (index < 1 || index > MAX_TOKENS) {
      return null;
    }

    return token(str, delimiter, index);
  }

  /**
   * Returns the nth token of a string split on each character of the delimiters, a surrogate pair
   * being a single character. The empty tokens are kept and the last of the {@link #MAX_TOKENS}
   * tokens holds the rest of the string. Only the requested token is extracted.
   *
   * @return the token, or {@code null} if the string has fewer tokens
   */
  private static @Nullable String token(final String str, final String delimiters, final int n) {
    int start = 0;
    int part = 1;
    int i = 0;
    while (i < str.length() && part < MAX_TOKENS) {
      int codePoint = str.codePointAt(i);
      int next = i + Character.charCount(codePoint);
      if (delimiters.indexOf(codePoint) >= 0) {
        if (part == n) {
          return str.substring(start, i);
        }
        part++;
        start = next;
      }
      i = next;
    }
    return (part == n) ? str.substring(start) : null;
  }

  /** Returns the number of tokens of a non-empty string, at most {@link #MAX_TOKENS}. */
  private static int countTokens(final String str, final String delimiters) {
    int count = 1;
    int i = 0;
    while (i < str.length() && count < MAX_TOKENS) {
      int codePoint = str.codePointAt(i);
      if (delimiters.indexOf(codePoint) >= 0) {
        count++;
      }
      i += Character.charCount(codePoint);
    }
    return count;
  }
}
