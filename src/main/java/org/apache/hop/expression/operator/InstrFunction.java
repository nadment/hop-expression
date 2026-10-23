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

/**
 * Returns the position in the string that is the first character of a specified occurrence of the
 * substring.
 */
@FunctionPlugin
@NullMarked
public class InstrFunction extends Function {

  public InstrFunction() {
    super(
        "INSTR",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.STRING_STRING
            .or(OperandTypes.STRING_STRING_INTEGER)
            .or(OperandTypes.STRING_STRING_INTEGER_INTEGER),
        OperatorCategory.STRING,
        "/docs/hop-user-manual/expression/instr.html");
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String str = operands[0].getValue(String.class);
    if (str == null) {
      return null;
    }
    String substr = operands[1].getValue(String.class);
    if (substr == null) {
      return null;
    }

    // Fast path, the first occurrence from the beginning
    if (operands.length == 2) {
      return toPosition(str, Characters.indexOf(str, substr, 0));
    }

    Long position = operands[2].getValue(Long.class);
    if (position == null) {
      return null;
    }
    if (position == 0) {
      throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 3, position);
    }

    // The occurrence to find, must be positive
    int occurrence = 1;
    if (operands.length == 4) {
      Long nth = operands[3].getValue(Long.class);
      if (nth == null) {
        return null;
      }
      if (nth < 1) {
        throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 4, nth);
      }
      // Clamp to the int range, the conversion would wrap a larger occurrence
      occurrence = Math.clamp(nth, 1, Integer.MAX_VALUE);
    }

    // A position beyond the start or the end of the string finds nothing, not even an empty
    // substring. Checked on the long value, the conversion to int would wrap a value out of
    // the int range. The position is in characters, a surrogate pair is a single character
    long length = Characters.length(str);
    if (position > length + 1 || position < -length - 1) {
      return 0L;
    }

    // Convert the 1-based position to a 0-based index, a negative position counts back from
    // the end (-1 is the last character) and searches towards the beginning
    if (position > 0) {
      return toPosition(
          str, indexOf(str, substr, Characters.offset(str, position - 1), occurrence));
    }
    long index = length + position;
    int start = (index < 0) ? -1 : Characters.offset(str, index);
    return toPosition(str, lastIndexOf(str, substr, start, occurrence));
  }

  /**
   * Searches forward the nth occurrence of a substring. The next search starts one character after
   * the previous match, so the overlapping occurrences are counted, like {@code
   * INSTR('AAA','AA',1,2)} returns 2.
   *
   * @return the index of the occurrence, or -1 if not found
   */
  private static int indexOf(String str, String substr, int start, int occurrence) {
    int index;
    while ((index = Characters.indexOf(str, substr, start)) >= 0 && --occurrence > 0) {
      // An empty substring is found at the end of the string, there is nothing after it
      if (index == str.length()) {
        return -1;
      }
      start = str.offsetByCodePoints(index, 1);
    }
    return index;
  }

  /**
   * Searches backward the nth occurrence of a substring, the overlapping occurrences are counted.
   *
   * @return the index of the occurrence, or -1 if not found
   */
  private static int lastIndexOf(String str, String substr, int start, int occurrence) {
    int index;
    while ((index = Characters.lastIndexOf(str, substr, start)) >= 0 && --occurrence > 0) {
      if (index == 0) {
        return -1;
      }
      start = str.offsetByCodePoints(index, -1);
    }
    return index;
  }

  /** Converts an index in the string to a 1-based position in characters, 0 if not found. */
  private static Long toPosition(String str, int index) {
    return (index < 0) ? 0L : Characters.position(str, index) + 1L;
  }
}
