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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.ScalarFunction;
import org.apache.hop.expression.util.Coerse;
import org.apache.hop.expression.util.Regexp;
import java.io.StringWriter;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/** The RLIKE regexp operator. */
public class RLike extends Operator {

  public RLike() {
    super("RLIKE", 120, true, true, "i18n::Operator.Category.Comparison", "/docs/regexp_like.html");
  }

  @ScalarFunction(id = "REGEXP_LIKE", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.Comparison", documentationUrl = "/docs/regexp_like.html")
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    Object v0 = operands[0].eval(context);
    if (v0 == null) {
      return null;
    }
    String input = Coerse.toString(v0);

    Object v1 = operands[1].eval(context);
    if (v1 == null) {
      return null;
    }
    String regexp = Coerse.toString(v1);
    // An empty pattern matches nothing
    if (regexp.length() == 0)
      return Boolean.FALSE;

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 3) {
      Object v2 = operands[2].eval(context);
      flags = Regexp.parseFlags(Coerse.toString(v2));
    }

    try {
      Pattern pattern = Pattern.compile(regexp, flags);
      return pattern.matcher(input).find();
    } catch (PatternSyntaxException e) {      
      throw new ExpressionException(ExpressionError.INVALID_REGEXP_PATTERN, regexp);
    }
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" RLIKE ");
    operands[1].unparse(writer);
  }
}
