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

import org.apache.hop.expression.DatePart;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.ScalarFunction;
import java.io.StringWriter;
import java.time.ZonedDateTime;

/**
 * Extracts the specified date or time part from a date, time, or timestamp.
 * 
 * Date part: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
 */
public class Extract extends Operator {

  public Extract() {
    super("EXTRACT", null, 10, true, "i18n::Operator.Category.Date");
  }

  @ScalarFunction(name = "EXTRACT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object part = operands[0].eval(context);
    if (part == null)
      return null;

    Object value = operands[1].eval(context);
    if (value == null)
      return null;

    ZonedDateTime datetime = coerceToDate(value);
    DatePart datePart = DatePart.of(part.toString());
    return datePart.get(datetime);
  }

  @ScalarFunction(name = "DATE_PART", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public Object date_part(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    return eval(context, operands);
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    writer.append("EXTRACT(");
    operands[0].write(writer);
    writer.append(" FROM ");
    operands[1].write(writer);
    writer.append(')');
  }
}
