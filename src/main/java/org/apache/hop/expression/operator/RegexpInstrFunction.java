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

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.expression.util.Regexp;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** */
@FunctionPlugin
@NullMarked
public class RegexpInstrFunction extends Function {

  /** The pattern compiled ahead of evaluation when it is constant, otherwise {@code null}. */
  private final @Nullable Pattern compiledPattern;

  public RegexpInstrFunction() {
    this(null);
  }

  private RegexpInstrFunction(@Nullable Pattern compiledPattern) {
    super(
        "REGEXP_INSTR",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.STRING_STRING
            .or(OperandTypes.STRING_STRING_INTEGER)
            .or(OperandTypes.STRING_STRING_INTEGER_INTEGER)
            .or(OperandTypes.STRING_STRING_INTEGER_INTEGER_INTEGER)
            .or(OperandTypes.STRING_STRING_INTEGER_INTEGER_INTEGER_STRING),
        OperatorCategory.STRING,
        "/docs/hop-user-manual/expression/regexp_instr.html");
    this.compiledPattern = compiledPattern;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    if (compiledPattern == null) {
      Pattern pattern = Regexp.compileConstantPattern(call, 1, 5);
      if (pattern != null) {
        return new Call(new RegexpInstrFunction(pattern), call.getOperands());
      }
    }
    return call;
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String input = operands[0].getValue(String.class);
    if (input == null) {
      return null;
    }

    String regexp = operands[1].getValue(String.class);
    if (regexp == null) {
      return null;
    }

    // Default position 1
    long position = 1;
    if (operands.length >= 3) {
      Long v2 = operands[2].getValue(Long.class);
      if (v2 != null) {
        position = v2;
      }
    }
    if (position < 1) {
      throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 3, position);
    }

    // Default occurrence
    int occurrence = 1;
    if (operands.length >= 4) {
      Long v3 = operands[3].getValue(Long.class);
      if (v3 != null) {
        if (v3 < 1) {
          throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 4, v3);
        }
        // Clamp to the int range, the conversion would wrap a larger occurrence
        occurrence = Math.clamp(v3, 1, Integer.MAX_VALUE);
      }
    }

    // Return-option
    int returnOption = 0;
    if (operands.length >= 5) {
      Long v4 = operands[4].getValue(Long.class);
      if (v4 != null) {
        // Compare the long value, the conversion to int would wrap a multiple of 2^32 to 0
        returnOption = (v4 == 0) ? 0 : 1;
      }
    }

    // Flags
    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 6) {
      String v5 = operands[5].getValue(String.class);
      flags = Regexp.parseFlags(v5);
    }

    // An empty pattern matches nothing and a search starting after the end of the string finds
    // nothing, checked after the arguments so they are still validated
    if (regexp.isEmpty() || position - 1 > Characters.length(input)) {
      return 0L;
    }

    try {
      Pattern p = (compiledPattern != null) ? compiledPattern : Pattern.compile(regexp, flags);
      Matcher matcher = p.matcher(input);

      // The position is 1-based, the matcher start index is 0-based
      // The position is in characters, a surrogate pair is a single character
      boolean found = matcher.find(Characters.offset(input, position - 1));
      for (int index = 1; index < occurrence && found; index++) {
        found = matcher.find();
      }

      if (found) {
        int index = (returnOption == 0) ? matcher.start() : matcher.end();
        return Long.valueOf(1L + Characters.position(input, index));
      }

      return 0L;
    } catch (PatternSyntaxException e) {
      throw new ExpressionException(ErrorCode.INVALID_REGEXP_PATTERN, regexp);
    }
  }
}
