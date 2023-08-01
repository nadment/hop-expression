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

import org.apache.hop.expression.Category;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Regexp;
import java.io.StringWriter;
import java.util.regex.Pattern;

/** The ILIKE case-insensitive operator. */
public class ILikeOperator extends Operator {

  public ILikeOperator() {
    super("ILIKE", 120, true, ReturnTypes.BOOLEAN, OperandTypes.STRING_STRING_OPTIONAL_STRING,
        Category.COMPARISON, "/docs/ilike.html");
  }

  @Override
    public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) {
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

    Pattern p = Pattern.compile(regex, Pattern.DOTALL | Pattern.CASE_INSENSITIVE);

    return p.matcher(value).matches();
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" ILIKE ");
    operands[1].unparse(writer);
    if (operands.length == 3) {
      writer.append(" ESCAPE ");
      operands[2].unparse(writer);
    }
  }
}
