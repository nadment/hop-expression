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

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.Error;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.ScalarFunction;
import org.apache.hop.expression.util.RegexpUtils;
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
    String input = DataType.toString(v0);

    Object v1 = operands[1].eval(context);
    if (v1 == null) {
      return null;
    }
    String regexp = DataType.toString(v1);
    // An empty pattern matches nothing
    if (regexp.length() == 0)
      return Boolean.FALSE;

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 3) {
      Object v2 = operands[2].eval(context);
      flags = RegexpUtils.parseFlags(DataType.toString(v2));
    }

    try {
      Pattern pattern = Pattern.compile(regexp, flags);
      return pattern.matcher(input).find();
    } catch (PatternSyntaxException e) {      
      throw new ExpressionException(Error.INVALID_REGEXP_PATTERN, regexp);
    }
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    operands[0].write(writer);
    writer.append(" RLIKE ");
    operands[1].write(writer);
  }
}
