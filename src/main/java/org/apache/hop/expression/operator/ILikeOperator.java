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

/** The ILIKE case-insensitive operator. */
public class ILikeOperator extends Operator {
  public static final ILikeOperator INSTANCE = new ILikeOperator();

  public ILikeOperator() {
    super(
        "ILIKE",
        120,
        Associativity.LEFT,
        ReturnTypes.BOOLEAN_NULLABLE,
        OperandTypes.STRING_STRING.or(OperandTypes.STRING_STRING_STRING),
        OperatorCategory.COMPARISON,
        "/docs/ilike.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    // Optimize NULL ILIKE FIELD → NULL
    IExpression value = call.getOperand(0);
    if (value.isNull()) return value;

    if (call.getOperand(1).isConstant()) {
      String pattern = call.getOperand(1).getValue(String.class);

      // Simplify x ILIKE NULL → NULL
      if (pattern == null) return Literal.NULL_BOOLEAN;

      char escape = '\\';
      if (call.getOperandCount() == 3) {
        String escapeString = call.getOperand(2).getValue(String.class);
        if (StringUtils.isNotEmpty(escapeString)) {
          escape = escapeString.charAt(0);
        }
      }

      // Multiple consecutive '%' in the string matched by ILIKE should simplify to a single '%'
      String simplifiedPattern = Regexp.simplifyLikeString(pattern, escape, '%');
      if (!simplifiedPattern.equals(pattern)) {
        return new Call(INSTANCE, value, Literal.of(simplifiedPattern));
      }

      // Simplify x LIKE '%' ESCAPE NULL → NULL
      if (call.getOperandCount() == 3 && call.getOperand(2).isNull()) {
        return Literal.NULL_BOOLEAN;
      }
    }

    return call;
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

    String regex = Regexp.toRegexLike(pattern, escape);
    Pattern p = Pattern.compile(regex, Pattern.DOTALL | Pattern.CASE_INSENSITIVE);
    return p.matcher(value).matches();
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, 0, 0);
    writer.append(" ILIKE ");
    operands[1].unparse(writer, 0, 0);
    if (operands.length == 3) {
      writer.append(" ESCAPE ");
      operands[2].unparse(writer, 0, 0);
    }
  }
}
