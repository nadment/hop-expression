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

import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Regexp;
import java.io.StringWriter;
import java.util.regex.Pattern;

/** The ILIKE case-insensitive operator. */
public class ILikeOperator extends Operator {

  public ILikeOperator() {
    super("ILIKE", 120, true, true, ReturnTypes.BOOLEAN, OperandTypes.STRING_STRING_OPTIONAL_STRING,
        OperatorCategory.COMPARISON, "/docs/ilike.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    Object v0 = operands[0].getValue(context);
    if (v0 == null) {
      return null;
    }

    Object v1 = operands[1].getValue(context);
    if (v1 == null) {
      return null;
    }

    String escape = null;
    if (operands.length == 3) {
      Object escapeValue = operands[2].getValue(context);
      if (escapeValue == null) {
        return null;
      }
      escape = Coerce.toString(escapeValue);
    }

    final String regex = Regexp.toRegexLike(Coerce.toString(v1), escape);

    Pattern p = Pattern.compile(regex, Pattern.DOTALL | Pattern.CASE_INSENSITIVE);

    return p.matcher(Coerce.toString(v0)).matches();
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
