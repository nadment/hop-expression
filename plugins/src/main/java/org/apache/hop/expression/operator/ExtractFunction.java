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
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.io.StringWriter;
import java.time.ZonedDateTime;
import java.time.format.TextStyle;
import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;
import java.util.Locale;

/**
 * Extracts the specified date or time part from a date, time, or timestamp.
 * 
 * Date part: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
 */
@FunctionPlugin(id = "EXTRACT", names = "DATE_PART", category = "i18n::Operator.Category.Date", documentationUrl = "/docs/extract.html")
public class ExtractFunction extends Function {

  public ExtractFunction() {
    super("EXTRACT", true, ReturnTypes.INTEGER, OperandTypes.NO_CHECK, "i18n::Operator.Category.Date", "/docs/extract.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    // Null throw exception
    DatePart part = Coerse.toDatePart(operands[0].getValue(context));

    Object value = operands[1].getValue(context);
    if (value == null)
      return null;

    return extract(Coerse.toDateTime(value), part);
  }

  protected Object extract(ZonedDateTime datetime, DatePart part) throws ExpressionException {
    switch (part) {
      case DAY:
        return datetime.getDayOfMonth();
      case DAYOFYEAR:
        return datetime.getDayOfYear();
      case DAYOFWEEK:
        int dow = datetime.getDayOfWeek().getValue() + 1;
        if (dow == 8)
          dow = 1;
        return dow;
      case ISODAYOFWEEK:
        return datetime.getDayOfWeek().getValue();
      case WEEK:
        return datetime.get(ChronoField.ALIGNED_WEEK_OF_YEAR);
      case ISOWEEK:
        return datetime.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR);
      case WEEKOFMONTH:
        return datetime.get(ChronoField.ALIGNED_WEEK_OF_MONTH);
      case MONTH:
        return datetime.getMonthValue();
      case QUARTER:
        return datetime.get(IsoFields.QUARTER_OF_YEAR);
      case YEAR:
        return datetime.getYear();
      case ISOYEAR:
        return datetime.get(IsoFields.WEEK_BASED_YEAR);
      case DECADE:
        return decade(datetime.getYear());
      case CENTURY:
        return century(datetime.getYear());
      case MILLENNIUM:
        return millennium(datetime.getYear());
      case HOUR:
        return datetime.getHour();
      case MINUTE:
        return datetime.getMinute();
      case SECOND:
        return datetime.getSecond();
      case MILLISECOND:
        return datetime.get(ChronoField.MILLI_OF_SECOND);
      case MICROSECOND:
        return datetime.get(ChronoField.MICRO_OF_SECOND);
      case NANOSECOND:
        return datetime.getNano();
      case EPOCH:
        return datetime.toEpochSecond();
      case TIMEZONE_ABBR:
        return datetime.getZone().getDisplayName(TextStyle.SHORT_STANDALONE, Locale.ENGLISH);
      case TIMEZONE_REGION:
        return datetime.getZone().getId();
      case TIMEZONE_HOUR:
        return datetime.getOffset().getTotalSeconds() / (60 * 60);
      case TIMEZONE_MINUTE:
        return (datetime.getOffset().getTotalSeconds() / 60) % 60;
      default:
        throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, part);
    }
  }

  private static int millennium(int year) {
    return year > 0 ? (year + 999) / 1000 : year / 1000;
  }

  private static int century(int year) {
    return year > 0 ? (year + 99) / 100 : year / 100;
  }

  private static int decade(int year) {
    return year >= 0 ? year / 10 : (year - 9) / 10;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("EXTRACT(");
    operands[0].unparse(writer);
    writer.append(" FROM ");
    operands[1].unparse(writer);
    writer.append(')');
  }
}
