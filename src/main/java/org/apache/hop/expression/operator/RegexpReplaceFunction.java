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
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.expression.util.Regexp;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** */
@FunctionPlugin
@NullMarked
public class RegexpReplaceFunction extends Function {

  private static final IOperandTypeChecker OTC =
      OperandTypes.explicit(
              TypeName.STRING,
              TypeName.STRING,
              TypeName.STRING,
              TypeName.INTEGER,
              TypeName.INTEGER,
              TypeName.STRING)
          .optional(i -> i >= 2);

  /** The pattern compiled ahead of evaluation when it is constant, otherwise {@code null}. */
  private final @Nullable Pattern compiledPattern;

  public RegexpReplaceFunction() {
    this(null);
  }

  private RegexpReplaceFunction(@Nullable Pattern compiledPattern) {
    super(
        "REGEXP_REPLACE",
        ReturnTypes.STRING_NULLABLE,
        OTC,
        OperatorCategory.STRING,
        "/docs/regexp_replace.html");
    this.compiledPattern = compiledPattern;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    if (compiledPattern == null) {
      Pattern pattern = Regexp.compileConstantPattern(call, 1, 5);
      if (pattern != null) {
        return new Call(new RegexpReplaceFunction(pattern), call.getOperands());
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

    String pattern = operands[1].getValue(String.class);
    if (pattern == null) {
      return null;
    }

    // Default empty string
    String replacement = "";
    if (operands.length >= 3) {
      replacement = operands[2].getValue(String.class);
      if (replacement == null) {
        return null;
      }
    }

    // Default position 1
    long position = 1;
    if (operands.length >= 4) {
      Long v3 = operands[3].getValue(Long.class);
      if (v3 != null) {
        position = v3;
      }
    }
    if (position < 1) {
      throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 4, position);
    }

    // Default occurrence 0
    int occurrence = 0;
    if (operands.length >= 5) {
      Long v4 = operands[4].getValue(Long.class);
      if (v4 != null) {
        // The occurrence 0 replaces all the occurrences
        if (v4 < 0) {
          throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 5, v4);
        }
        // Clamp to the int range, the conversion would wrap a larger occurrence
        occurrence = Math.clamp(v4, 0, Integer.MAX_VALUE);
      }
    }

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 6) {
      String v5 = operands[5].getValue(String.class);
      flags = Regexp.parseFlags(v5);
    }

    // An empty pattern matches nothing and a search starting after the end of the string replaces
    // nothing, checked after the arguments so they are still validated
    if (pattern.isEmpty() || position - 1 > Characters.length(input)) {
      return input;
    }

    try {

      // Back reference
      if ((replacement.indexOf('\\') >= 0) || (replacement.indexOf('$') >= 0)) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < replacement.length(); i++) {
          char c = replacement.charAt(i);
          if (c == '$') {
            builder.append('\\');
          } else if (c == '\\' && ++i < replacement.length()) {
            c = replacement.charAt(i);
            builder.append(c >= '0' && c <= '9' ? '$' : '\\');
          }
          builder.append(c);
        }
        replacement = builder.toString();
      }

      Pattern p = (compiledPattern != null) ? compiledPattern : Pattern.compile(pattern, flags);
      Matcher matcher = p.matcher(input);
      StringBuilder buffer = new StringBuilder();

      // The position is 1-based, the matcher start index is 0-based.
      // Do not use replaceAll(), it resets the matcher and would search from the beginning.
      // The position is in characters, a surrogate pair is a single character
      boolean found = matcher.find(Characters.offset(input, position - 1));
      int index = 1;
      while (found) {
        if (occurrence == 0 || index == occurrence) {
          matcher.appendReplacement(buffer, replacement);
          if (occurrence != 0) {
            break;
          }
        }
        index++;
        found = matcher.find();
      }
      matcher.appendTail(buffer);
      return buffer.toString();
    } catch (PatternSyntaxException e) {
      throw new ExpressionException(ErrorCode.INVALID_REGEXP_PATTERN, pattern);
    }
  }
}
