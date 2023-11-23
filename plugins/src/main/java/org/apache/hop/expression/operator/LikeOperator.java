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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.Category;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.Operators;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Regexp;
import java.io.StringWriter;
import java.util.regex.Pattern;

/**
 * An operator describing the <code>LIKE</code> operator.
 *
 * <p>
 * Syntax of the operator:
 *
 * <ul>
 * <li><code>field [NOT] LIKE pattern ESCAPE char</code>
 * </ul>
 *
 * <p>
 * <b>NOTE</b> If the <code>NOT</code> clause is present the parser will generate a equivalent to
 * <code>
 * NOT (field LIKE pattern ...)</code>
 */
public class LikeOperator extends Operator {

  static final Pattern startsWith = Pattern.compile("^([^_%]+)%$");
  static final Pattern endsWith = Pattern.compile("^%([^_%]+)$");
  static final Pattern contains = Pattern.compile("^%([^_%]+)%$");
  static final Pattern equalTo = Pattern.compile("^[^_%]*$");


  public LikeOperator() {
    super("LIKE", 120, true, ReturnTypes.BOOLEAN,
        OperandTypes.STRING_STRING.or(OperandTypes.STRING_STRING_STRING), Category.COMPARISON,
        "/docs/like.html");
  }

  /**
   * Simplifies LIKE expressions that do not need full regular expressions to evaluate the
   * condition. For example, when the expression is just checking to see if a string starts with a
   * given pattern.
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Optimize NULL LIKE FIELD to NULL
    IExpression value = call.getOperand(0);
    if (value.isNull())
      return value;

    if (call.getOperand(1).isConstant()) {
      String pattern = call.getOperand(1).getValue(String.class);

      // FIELD LIKE NULL → NULL
      if (pattern == null)
        return new Literal(null, call.getType());

      if (call.getOperandCount() == 3) {
        if (call.getOperand(2).isNull())
          return Literal.NULL;

        // For now don't optimize if special escape char
        return call;
      }

      // field LIKE '%' → IFNULL(field,NULL,TRUE)
      if ("%".equals(pattern)) {
        // return new Call(Operators.IS_NOT_NULL, value);
        return new Call(Operators.IFNULL, value, Literal.NULL, Literal.TRUE);
      }

      // field LIKE '%foo%' → CONTAINS(field,'foo')
      if (contains.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(ContainsFunction.ContainsString, value, Literal.of(search));
      }

      // field LIKE 'foo%' → STARTSWITH(field,'foo')
      if (startsWith.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(StartsWithFunction.StartsWithString, value, Literal.of(search));
      }

      // field LIKE '%foo' → ENDSWITH(field,'foo')
      if (endsWith.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(EndsWithFunction.EndsWithString, value, Literal.of(search));
      }

      // field LIKE 'Hello' → field='Hello'
      if (equalTo.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(Operators.EQUAL, value, Literal.of(search));
      }
    }

    return call;
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String input = operands[0].getValue(String.class);
    if (input == null) {
      return null;
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

    final String regex = Regexp.toRegexLike(pattern, escape);

    Pattern p = Pattern.compile(regex, Pattern.DOTALL);

    return p.matcher(input).matches();
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" LIKE ");
    operands[1].unparse(writer);
    if (operands.length == 3) {
      writer.append(" ESCAPE ");
      operands[2].unparse(writer);
    }
  }
}
