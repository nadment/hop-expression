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

import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.util.Coerse;
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
public class Like extends Operator {

  public Like() {
    super("LIKE", 120, true, true, "i18n::Operator.Category.Comparison", "/docs/like.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object input = operands[0].eval(context);
    if (input == null) {
      return null;
    }
    Object pattern = operands[1].eval(context);
    if (pattern == null) {
      return null;
    }

    String escape = null;
    if (operands.length == 3) {
      Object escapeValue = operands[2].eval(context);
      if (escapeValue == null) {
        return null;
      }
      escape = Coerse.toString(escapeValue);
    }

    final String regex = Regexp.toRegexLike(Coerse.toString(pattern), escape);

    Pattern p = Pattern.compile(regex, Pattern.DOTALL);

    return p.matcher(Coerse.toString(input)).matches();
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
