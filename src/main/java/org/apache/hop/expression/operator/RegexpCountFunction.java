/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
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

/**
 * Searches a string for a regular expression pattern and returns an integer that indicates the
 * number of times the pattern occurs in the string. If no match is found, then the function returns
 * 0.
 *
 * <p>REGEXP_COUNT ( source, pattern [, start [, parameters ] ] )
 */
@FunctionPlugin
@NullMarked
public class RegexpCountFunction extends Function {

  /** The pattern compiled ahead of evaluation when it is constant, otherwise {@code null}. */
  private final @Nullable Pattern compiledPattern;

  public RegexpCountFunction() {
    this(null);
  }

  private RegexpCountFunction(@Nullable Pattern compiledPattern) {
    super(
        "REGEXP_COUNT",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.STRING_STRING
            .or(OperandTypes.STRING_STRING_INTEGER)
            .or(OperandTypes.STRING_STRING_INTEGER_STRING),
        OperatorCategory.STRING,
        "/docs/hop-user-manual/expression/regexp_count.html");
    this.compiledPattern = compiledPattern;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    if (compiledPattern == null) {
      Pattern pattern = Regexp.compileConstantPattern(call, 1, 3);
      if (pattern != null) {
        return new Call(new RegexpCountFunction(pattern), call.getOperands());
      }
    }
    return call;
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {

    long count = 0L;

    String source = operands[0].getValue(String.class);
    if (source == null) {
      return null;
    }

    String pattern = operands[1].getValue(String.class);
    if (pattern == null) {
      return null;
    }

    long start = 1;
    int parameters = Pattern.UNICODE_CASE;
    if (operands.length >= 3) {
      Long v2 = operands[2].getValue(Long.class);
      if (v2 != null) {
        start = v2;
      }

      if (operands.length == 4) {
        String v3 = operands[3].getValue(String.class);
        parameters = Regexp.parseFlags(v3);
      }
    }
    if (start < 1) {
      throw new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 3, start);
    }
    // An empty pattern matches nothing and a search starting after the end of the string finds
    // nothing, checked after the arguments so they are still validated
    if (pattern.isEmpty() || start - 1 > Characters.length(source)) {
      return count;
    }

    try {
      Pattern p =
          (compiledPattern != null) ? compiledPattern : Pattern.compile(pattern, parameters);
      Matcher matcher = p.matcher(source);
      // The position is 1-based, the matcher start index is 0-based
      // The position is in characters, a surrogate pair is a single character
      boolean found = matcher.find(Characters.offset(source, start - 1));
      while (found) {
        count++;
        found = matcher.find();
      }
    } catch (PatternSyntaxException e) {
      throw new ExpressionException(ErrorCode.INVALID_REGEXP_PATTERN, pattern);
    }

    return count;
  }
}
