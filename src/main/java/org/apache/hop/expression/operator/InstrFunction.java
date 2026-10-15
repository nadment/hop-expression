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
        "/docs/instr.html");
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

    int start = 0;
    boolean forward = true;
    int occurrence = 1;
    int result = 0;

    // If 3 operands, indicate the position to start
    if (operands.length >= 3) {
      Long position = operands[2].getValue(Long.class);
      if (position == null) {
        return null;
      }
      if (position == 0) {
        throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 3, position);
      }

      // Clamp to just outside the string, the conversion to int would wrap a value out of the
      // int range, and a position beyond the string finds nothing either way
      long bound = Characters.length(str) + 1L;
      start = (int) Math.clamp(position, -bound, bound);

      // Convert the 1-based position to a 0-based index, a negative position counts back from
      // the end (-1 is the last character) and searches towards the beginning
      // The position is in characters, a surrogate pair is a single character
      forward = start > 0;
      long index = forward ? start - 1L : Characters.length(str) + start;
      start = (index < 0) ? -1 : Characters.offset(str, index);

      // The occurrence to find, must be positive
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
    }

    // The next search starts one character after the previous match, so the overlapping
    // occurrences are counted, like INSTR('AAA','AA',1,2) returns 2
    if (forward) {
      while ((result = str.indexOf(substr, start)) >= 0) {
        if (--occurrence <= 0) break;
        start = result + 1;
      }
    } else {
      while ((result = str.lastIndexOf(substr, start)) >= 0) {
        if (--occurrence <= 0) break;
        start = result - 1;
      }
    }

    return Long.valueOf((result < 0) ? 0L : Characters.position(str, result) + 1L);
  }
}
