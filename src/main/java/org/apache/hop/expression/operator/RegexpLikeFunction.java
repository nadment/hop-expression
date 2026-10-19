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
import org.apache.hop.expression.util.Regexp;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** The REGEXP_LIKE function. */
@FunctionPlugin
@NullMarked
public class RegexpLikeFunction extends Function {

  /** The pattern compiled ahead of evaluation when it is constant, otherwise {@code null}. */
  private final @Nullable Pattern compiledPattern;

  public RegexpLikeFunction() {
    this(null);
  }

  private RegexpLikeFunction(@Nullable Pattern compiledPattern) {
    super(
        "REGEXP_LIKE",
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.STRING_STRING.or(OperandTypes.STRING_STRING_STRING),
        OperatorCategory.COMPARISON,
        "/docs/regexp_like.html");
    this.compiledPattern = compiledPattern;
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    if (compiledPattern == null) {
      Pattern pattern = Regexp.compileConstantPattern(call, 1, 2);
      if (pattern != null) {
        return new Call(new RegexpLikeFunction(pattern), call.getOperands());
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

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 3) {
      String v2 = operands[2].getValue(String.class);
      flags = Regexp.parseFlags(v2);
    }

    // An empty pattern matches nothing, checked after the parameters so they are still validated
    if (pattern.isEmpty()) {
      return Boolean.FALSE;
    }

    try {
      Pattern p = (compiledPattern != null) ? compiledPattern : Pattern.compile(pattern, flags);
      Matcher matcher = p.matcher(input);
      return matcher.find();
    } catch (PatternSyntaxException e) {
      throw new ExpressionException(ErrorCode.INVALID_REGEXP_PATTERN, pattern);
    }
  }
}
