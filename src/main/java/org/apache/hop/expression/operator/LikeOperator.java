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

import java.io.StringWriter;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Regexp;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * An operator describing the <code>LIKE</code> operator.
 *
 * <p>Syntax of the operator:
 *
 * <ul>
 *   <li><code>field [NOT] LIKE pattern ESCAPE char</code>
 * </ul>
 *
 * <p><b>NOTE</b> If the <code>NOT</code> clause is present the parser will generate an equivalent
 * to <code>
 * NOT (field LIKE pattern ...)</code>
 */
@NullMarked
public class LikeOperator extends Operator {

  public static final LikeOperator INSTANCE = new LikeOperator();

  static final Pattern startsWith = Pattern.compile("^([^_%]+)%$");
  static final Pattern endsWith = Pattern.compile("^%([^_%]+)$");
  static final Pattern contains = Pattern.compile("^%([^_%]+)%$");
  static final Pattern equalTo = Pattern.compile("^[^_%]*$");

  /** The pattern compiled ahead of evaluation when it is constant, otherwise {@code null}. */
  private final @Nullable Pattern compiledPattern;

  public LikeOperator() {
    this(null);
  }

  private LikeOperator(@Nullable Pattern compiledPattern) {
    super(
        "LIKE",
        80,
        Associativity.LEFT,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.STRING_STRING.or(OperandTypes.STRING_STRING_STRING),
        OperatorCategory.COMPARISON,
        "/docs/like.html");
    this.compiledPattern = compiledPattern;
  }

  /**
   * Compiles the pattern of a LIKE or ILIKE call ahead of evaluation, when the pattern and the
   * escape character are constant.
   *
   * <p>An invalid pattern is not reported here, the error is raised at evaluation.
   *
   * @param call the call of a LIKE or ILIKE operator
   * @param flags the match flags of the regular expression
   * @return the compiled pattern, or {@code null} if it must be compiled at evaluation
   */
  static @Nullable Pattern createConstantPattern(Call call, int flags) {
    if (!call.getOperand(1).isConstant()) {
      return null;
    }
    try {
      String pattern = call.getOperand(1).getValue(String.class);
      if (pattern == null) {
        return null;
      }
      String escape = null;
      if (call.getOperandCount() == 3) {
        if (!call.getOperand(2).isConstant()) {
          return null;
        }
        escape = call.getOperand(2).getValue(String.class);
        if (escape == null) {
          return null;
        }
      }
      return Pattern.compile(Regexp.toRegexLike(pattern, escape), flags);
    } catch (RuntimeException e) {
      return null;
    }
  }

  private static IExpression precompile(Call call) {
    Pattern pattern = createConstantPattern(call, Pattern.DOTALL);
    if (pattern == null) {
      return call;
    }
    return new Call(new LikeOperator(pattern), call.getOperands());
  }

  /**
   * Simplifies LIKE expressions that do not need to full regular expressions to evaluate the
   * condition. For example, when the expression is just checking to see if a string starts with a
   * given pattern.
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Already compiled
    if (compiledPattern != null) return call;

    // Simplify NULL LIKE FIELD → NULL
    IExpression value = call.getOperand(0);
    if (value.isNull()) return value;

    if (call.getOperand(1).isConstant()) {
      String pattern = call.getOperand(1).getValue(String.class);

      // Simplify x LIKE NULL → NULL
      if (pattern == null) return Literal.NULL_BOOLEAN;

      char escape = '\\';
      if (call.getOperandCount() == 3) {
        String escapeString = call.getOperand(2).getValue(String.class);
        if (StringUtils.isNotEmpty(escapeString)) {
          escape = escapeString.charAt(0);
        }
      }

      // Multiple consecutive '%' in the string matched by LIKE should simplify to a single '%'
      String simplifiedPattern = Regexp.simplifyLikeString(pattern, escape, '%');
      if (!simplifiedPattern.equals(pattern)) {
        return new Call(INSTANCE, value, Literal.of(simplifiedPattern));
      }

      if (call.getOperandCount() == 3) {
        if (call.getOperand(2).isNull()) {
          return Literal.NULL_BOOLEAN;
        }

        // For now, don't optimize if special escapes char
        return precompile(call);
      }

      // Simplify x LIKE '%' → IFNULL(x,NULL,TRUE)
      if ("%".equals(pattern)) {
        return new Call(Nvl2Function.INSTANCE, value, Literal.TRUE, Literal.NULL);
      }

      // Simplify x LIKE '%foo%' → CONTAINS(x,'foo')
      if (contains.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(ContainsFunction.INSTANCE, value, Literal.of(search));
      }

      // Simplify x LIKE 'foo%' → STARTSWITH(x,'foo')
      if (startsWith.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(StartsWithFunction.INSTANCE, value, Literal.of(search));
      }

      // Simplify x LIKE '%foo' → ENDSWITH(x,'foo')
      if (endsWith.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(EndsWithFunction.INSTANCE, value, Literal.of(search));
      }

      // Simplify x LIKE 'Hello' → x='Hello'
      if (equalTo.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(EqualOperator.INSTANCE, value, Literal.of(search));
      }
    }

    return precompile(call);
  }

  @Override
  public @Nullable Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) {
      return null;
    }
    if (compiledPattern != null) {
      return compiledPattern.matcher(value).matches();
    }
    String pattern = operands[1].getValue(String.class);
    if (pattern == null) {
      return null;
    }
    String escape = null;
    if (operands.length == 3) {
      escape = operands[2].getValue(String.class);
      if (escape == null) {
        return null;
      }
    }

    String regex = Regexp.toRegexLike(pattern, escape);
    Pattern p = Pattern.compile(regex, Pattern.DOTALL);
    return p.matcher(value).matches();
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" LIKE ");
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
    if (operands.length == 3) {
      writer.append(" ESCAPE ");
      operands[2].unparse(writer, getLeftPrec(), getRightPrec());
    }
  }
}
