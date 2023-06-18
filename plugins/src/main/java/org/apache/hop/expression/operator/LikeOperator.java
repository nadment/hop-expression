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
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.Operators;
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
    super("LIKE", 120, true, ReturnTypes.BOOLEAN, OperandTypes.STRING_STRING_OPTIONAL_STRING,
        OperatorCategory.COMPARISON, "/docs/like.html");
  }
  
  /**
   * Simplifies LIKE expressions that do not need full regular expressions to evaluate the
   * condition. For example, when the expression is just checking to see if a string starts with a
   * given pattern.
   */
  @Override
  public IExpression compile(IExpressionContext context, Call call)
      throws ExpressionException {
    // Optimize NULL LIKE FIELD to NULL
    IExpression value = call.getOperand(0);
    if (value.isNull())
      return value;

    if (call.getOperand(1).isConstant()) {
      String pattern = call.getOperand(1).getValue(String.class);

      // Optimize FIELD LIKE NULL to NULL
      if (pattern == null)
        return Literal.NULL;

      if (call.getOperandCount() == 3) {
        String escape = call.getOperand(2).getValue(String.class);
        if (escape == null)
          return Literal.NULL;

        // For now don't optimize if special escape char
        return call;
      }

      // Optimize "x LIKE '%'" to "x IS NOT NULL"
      if ("%".equals(pattern)) {
        return new Call(Operators.IS_NOT_NULL, value);
      }

      // Optimize the common case of FIELD LIKE '%foo%' to CONTAINS(FIELD,'foo')
      // Try contains before starts and ends
      if (contains.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(FunctionRegistry.getFunction("CONTAINS"), value, Literal.of(search));
      }

      // Optimize the common case of FIELD LIKE 'foo%' to STARTSWITH(FIELD,'foo')
      if (startsWith.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(FunctionRegistry.getFunction("STARTSWITH"), value, Literal.of(search));
      }

      // Optimize the common case of FIELD LIKE '%foo' to ENDSWITH(FIELD,'foo')
      if (endsWith.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(FunctionRegistry.getFunction("ENDSWITH"), value, Literal.of(search));
      }

      // Optimize FIELD LIKE 'Hello' to FIELD='Hello'
      if (equalTo.matcher(pattern).find()) {
        String search = pattern.replace("%", "");
        return new Call(Operators.EQUAL, value, Literal.of(search));
      }
    }

    return call;
  }

  @Override
  public Object eval(IExpression[] operands) throws Exception {
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
      escape= operands[2].getValue(String.class);
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
