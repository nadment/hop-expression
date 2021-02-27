/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression;

import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.WordUtils;
import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.i18n.BaseMessages;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.DayOfWeek;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.TextStyle;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.IsoFields;
import java.time.temporal.TemporalAdjusters;
import java.util.Calendar;
import java.util.Locale;
import java.util.regex.Pattern;

/** A <code>Function</code> is a type of operator which has conventional function-call syntax. */

// TODO: implement TRY_CAST
// TODO: implement DATEDIFF
// TODO: implement DATEPART
// TODO: implement BIT_COUNT
// TODO: implement REGEXP_INSTR
// TODO: implement REGEXP_SUBSTR
// TODO: implement REGEXP_REPLACE
// TODO: implement OVERLAY
// TODO: implement NULLIFZERO
// TODO: FIRST_DAY(d, 'week') LAST_DAY(d, 'week') add format YEAR, MONTH, DAY, WEEK 
// TODO: JSON_EXTRACT()  and -> operator 

public class Function extends Operator {

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  private static final char[] HEX = "0123456789abcdef".toCharArray();

  /** The maximum size to which the padding can expand. */
  private static final int PAD_LIMIT = 8192;

  private static final char[] SOUNDEX = new char[128];
  private static final int SOUNDEX_LENGTH = 4;

  static {
    // SOUNDEX_INDEX
    String index = "7AEIOUY8HW1BFPV2CGJKQSXZ3DT4L5MN6R";
    char number = 0;
    for (int i = 0, length = index.length(); i < length; i++) {
      char c = index.charAt(i);
      if (c < '9') {
        number = c;
      } else {
        SOUNDEX[c] = number;
        SOUNDEX[Character.toLowerCase(c)] = number;
      }
    }
  }

  private final boolean isDeterministic;

  protected Function(Kind kind, String name, boolean isAlias, boolean isDeterministic) {
    super(kind, name, 10, 10, isAlias);
    this.isDeterministic = isDeterministic;
  }

  /**
   * Whether the function always returns the same result for the same parameters.
   *
   * @return true if it does
   */
  public boolean isDeterministic() {
    return isDeterministic;
  }

  @Override
  public void write(StringWriter writer, ExpressionCall call, int leftPrec, int rightPrec) {
    switch (kind) {
      case CAST:
        {
          IExpression[] operands = call.getOperands();
          writer.append(this.getName());
          writer.append('(');
          operands[0].write(writer, leftPrec, rightPrec);
          writer.append(" AS ");
          int ordinal = (int) ((Value) operands[1]).toInteger();
          writer.append(DataType.of(ordinal).name());
          writer.append(')');
          break;
        }
      case EXTRACT:
        {
          IExpression[] operands = call.getOperands();
          writer.append(this.getName());
          writer.append('(');
          operands[0].write(writer, leftPrec, rightPrec);
          writer.append(" FROM ");
          int ordinal = (int) ((Value) operands[1]).toInteger();
          writer.append(DatePart.of(ordinal).name());
          writer.append(')');
          break;
        }
      default:
        writer.append(this.getName());
        writer.append('(');
        boolean first = true;
        for (IExpression operand : call.getOperands()) {
          if (!first) writer.append(',');
          else first = false;
          operand.write(writer, leftPrec, rightPrec);
        }
        writer.append(')');
    }
  }

  @Override
  public boolean checkNumberOfArguments(int len) throws ExpressionException {

    int min = 0, max = Integer.MAX_VALUE;
    switch (kind) {
      case CURRENT_DATE:
      case PI:
        max = 0;
        break;
      case ABS:
      case ACOS:
      case ACOSH:
      case ASCII:
      case ASIN:
      case ASINH:
      case ATAN:
      case ATANH:
      case CEIL:
      case CBRT:
      case CHR:
      case COS:
      case COSH:
      case COT:
      case DEGREES:
      case EXP:
      case FIRST_DAY:
      case FLOOR:
      case HOUR:
      case INITCAP:
      case LENGTH:
      case LN:
      case LOG10:
      case LOWER:
      case RADIANS:
      case ROUND:
      case SIGN:
      case SIN:
      case SINH:
      case SQRT:
      case SOUNDEX:
      case TAN:
      case TANH:
      case UPPER:
      case REVERSE:
      case UNICODE:
      case SPACE:
      case SECOND:
      case MINUTE:
      case DAYOFWEEK_ISO:
      case DAYOFWEEK:
      case DAY:
      case DAYOFYEAR:
      case DAYNAME:
      case MONTH:
      case MONTHNAME:
      case QUARTER:
      case URLDECODE:
      case URLENCODE:
      case WEEK:
      case WEEKOFMONTH:
      case WEEK_ISO:
      case LAST_DAY:
      case YEAR:
      case MD5:
      case SHA1:
      case SHA256:
      case SHA384:
      case SHA512:
      case STRINGENCODE:
      case STRINGDECODE:
      case TO_BOOLEAN:
      case TRY_TO_BOOLEAN:
        min = 1;
        max = 1;
        break;
      case TRIM:
      case LTRIM:
      case RTRIM:
      case TO_CHAR:
      case TO_DATE:
      case TRUNCATE:
      case TRY_TO_DATE:
        min = 1;
        max = 2;
        break;

      case TO_NUMBER:
      case TRY_TO_NUMBER:
        min = 1;
        max = 3;
        break;

      case ADD:
      case ADD_DAYS:
      case ADD_HOURS:
      case ADD_MINUTES:
      case ADD_MONTHS:
      case ADD_SECONDS:
      case ADD_WEEKS:
      case ADD_YEARS:
      case ATAN2:
      case BITAND:
      case BITNOT:
      case BITOR:
      case BITXOR:
        // case BIT_GET:
      case CONTAINS:
      case DAYS_BETWEEN:
      case DIVIDE:
      case ENDSWITH:
      case EQUAL_NULL:
      case EXTRACT:
      case HOURS_BETWEEN:
      case IFNULL:
      case LOG:
      case POWER:
      case MINUTES_BETWEEN:
      case MOD:
      case MONTHS_BETWEEN:
      case MULTIPLY:
      case NEXT_DAY:
      case PREVIOUS_DAY:
      case LEFT:
      case REPEAT:
      case RIGHT:
      case NULLIF:
      case SECONDS_BETWEEN:
      case STARTSWITH:
      case SUBTRACT:
      case REGEXP_LIKE:
      case YEARS_BETWEEN:
        min = 2;
        max = 2;
        break;
      case CAST:
      case INSTR:
      case LPAD:
      case REPLACE:
      case RPAD:
      case SUBSTRING:
        min = 2;
        max = 3;
        break;
      case IF:
      case NVL2:
      case TRANSLATE:
      case DATE:
        min = 3;
        max = 3;
        break;
      case COALESCE:
      case CONCAT:
      case GREATEST:
      case LEAST:
        min = 1;
        max = Integer.MAX_VALUE;
        break;
      case DECODE:
        min = 4;
        max = Integer.MAX_VALUE;
        break;
      case RAND:
        min = 0;
        max = 1;
        break;
      default:
        throw createInternalError(kind.name());
    }
    if (len < min || len > max) {
      return false;
      // throw new
      // ExpressionException(BaseMessages.getString(PKG,"Expression.InvalidNumberOfArguments",
      // this.getName()));
    }

    return true;
  }

  @Override
  @SuppressWarnings("incomplete-switch")
  public Value eval(final IExpressionContext context, final IExpression... args)
      throws ExpressionException {
    try {
      switch (kind) {
        case ADD:
        case CAST:
        case SUBTRACT:
        case MOD:
        case MULTIPLY:
        case DIVIDE:
        case CONCAT:
        case POWER:
        case BITAND:
        case BITNOT:
        case BITOR:
        case BITXOR:
          // use operator implementation
          return super.eval(context, args);

        case CURRENT_DATE:
          return Value.of(context.getCurrentDate());

        case ADD_DAYS:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return value;
            Value days = args[1].eval(context);
            if (days.isNull()) return Value.NULL;

            ZonedDateTime dt =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone())
                    .plusDays(days.toInteger());
            return Value.of(dt.toInstant());
          }
        case ADD_HOURS:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return value;
            Value hours = args[1].eval(context);
            if (hours.isNull()) return Value.NULL;

            ZonedDateTime dt =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone())
                    .plusHours(hours.toInteger());
            return Value.of(dt.toInstant());
          }
        case ADD_MINUTES:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return value;
            Value minutes = args[1].eval(context);
            if (minutes.isNull()) return Value.NULL;

            ZonedDateTime dt =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone())
                    .plusMinutes(minutes.toInteger());
            return Value.of(dt.toInstant());
          }

        case ADD_MONTHS:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            Value months = args[1].eval(context);
            if (months.isNull()) return Value.NULL;

            ZonedDateTime dt =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone())
                    .plusMonths(months.toInteger());
            return Value.of(dt.toInstant());
          }

        case ADD_SECONDS:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            Value seconds = args[1].eval(context);
            if (seconds.isNull()) return Value.NULL;

            ZonedDateTime dt =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone())
                    .plusSeconds(seconds.toInteger());
            return Value.of(dt.toInstant());
          }
        case ADD_WEEKS:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            Value weeks = args[1].eval(context);
            if (weeks.isNull()) return Value.NULL;

            ZonedDateTime dt =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone())
                    .plusWeeks(weeks.toInteger());
            return Value.of(dt.toInstant());
          }

        case ADD_YEARS:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            Value years = args[1].eval(context);
            if (years.isNull()) return Value.NULL;

            ZonedDateTime dt =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone())
                    .plusYears(years.toInteger());
            return Value.of(dt.toInstant());
          }

        case DATE:
          {
            Value v0 = args[0].eval(context);
            if (v0.isNull()) return Value.NULL;

            Value v1 = args[1].eval(context);
            if (v1.isNull()) return Value.NULL;

            Value v2 = args[2].eval(context);
            if (v2.isNull()) return Value.NULL;

            int year = (int) v0.toInteger();
            int month = (int) v1.toInteger();
            int day = (int) v2.toInteger();

            int monthsToAdd = 0;
            if (month < 1) {
              monthsToAdd = month;
              month = 1;
            } else if (month > 12) {
              monthsToAdd = month - 1;
              month = 1;
            }

            int daysToAdd = 0;
            if (day < 1 || day > 31) {
              daysToAdd = day;
              day = 1;
            }

            LocalDate date = LocalDate.of(year, month, day);
            if (monthsToAdd != 0) date = date.plusMonths(monthsToAdd);
            if (daysToAdd != 0) date = date.plusDays(daysToAdd);

            return Value.of(date.atStartOfDay(ZoneOffset.UTC).toInstant());
          }

        case EXTRACT:
          {
            Value part = args[0].eval(context);
            if (part.isNull()) return Value.NULL;

            Value value = args[1].eval(context);
            if (value.isNull()) return Value.NULL;

            ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
            DatePart datePart = DatePart.of((int) part.toInteger());

            return Value.of(datePart.get(dt));
          }

        case FIRST_DAY:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            ZonedDateTime dt =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone())
                    .with(TemporalAdjusters.firstDayOfMonth());
            return Value.of(dt.toInstant());
          }

        case LAST_DAY:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            ZonedDateTime dt =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone())
                    .with(TemporalAdjusters.lastDayOfMonth());
            return Value.of(dt.toInstant());
          }

        case NEXT_DAY:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            Value dow = args[1].eval(context);
            if (dow.isNull()) return Value.NULL;

            DayOfWeek dayofweek = DayOfWeek.valueOf(dow.toString().toUpperCase());
            ZonedDateTime dt =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone())
                    .with(TemporalAdjusters.next(dayofweek));

            return Value.of(dt.toInstant());
          }
          
          case PREVIOUS_DAY: {
            Value value = args[0].eval(context);
            if (value.isNull())
              return Value.NULL;

            Value dow = args[1].eval(context);
            if (dow.isNull())
              return Value.NULL;

            DayOfWeek dayofweek = DayOfWeek.valueOf(dow.toString().toUpperCase());
            ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
                .with(TemporalAdjusters.previous(dayofweek));

            return Value.of(dt.toInstant());
          }

        case MONTH:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
            return Value.of(dt.getMonthValue());
          }

        case MONTHNAME:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            Month month = ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getMonth();
            return Value.of(month.getDisplayName(TextStyle.FULL, Locale.ENGLISH));
          }

        case MONTHS_BETWEEN:
          {
            Value value1 = args[0].eval(context);
            if (value1.isNull()) return Value.NULL;
            Value value2 = args[1].eval(context);
            if (value2.isNull()) return Value.NULL;

            LocalDate startDate = value1.toDate().atZone(context.getZone()).toLocalDate();
            LocalDate endDate = value2.toDate().atZone(context.getZone()).toLocalDate();
            long days = startDate.until(endDate, ChronoUnit.DAYS);
            return Value.of(days / 31d);
            //			long months = startDate.until(endDate, ChronoUnit.MONTHS);
            //			return Value.of(months);
          }

        case YEAR:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
            return Value.of(dt.getYear());
          }

        case YEARS_BETWEEN:
          {
            Value value1 = args[0].eval(context);
            if (value1.isNull()) return Value.NULL;
            Value value2 = args[1].eval(context);
            if (value2.isNull()) return Value.NULL;

            LocalDate startDate = value1.toDate().atZone(context.getZone()).toLocalDate();
            LocalDate endDate = value2.toDate().atZone(context.getZone()).toLocalDate();
            long years = startDate.until(endDate, ChronoUnit.YEARS);
            return Value.of(years);
          }

        case QUARTER:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
            return Value.of(dt.get(IsoFields.QUARTER_OF_YEAR));
          }

        case DAYOFWEEK:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            DayOfWeek dow =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getDayOfWeek();

            int result = dow.getValue() + 1;
            if (result == 8) result = 1;

            return Value.of(result);
          }

        case DAYOFWEEK_ISO:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            DayOfWeek dow =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getDayOfWeek();
            return Value.of(dow.getValue());

            //			ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
            //			return Value.of(DatePart.DAYOFWEEKISO.get(dt));
          }

        case DAY:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
            return Value.of(dt.getDayOfMonth());
          }

        case DAYOFYEAR:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
            return Value.of(dt.getDayOfYear());
          }

        case DAYNAME:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            DayOfWeek weekday =
                ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getDayOfWeek();
            return Value.of(weekday.getDisplayName(TextStyle.FULL, Locale.ENGLISH));
          }

        case DAYS_BETWEEN:
          {
            Value value1 = args[0].eval(context);
            if (value1.isNull()) return Value.NULL;
            Value value2 = args[1].eval(context);
            if (value2.isNull()) return Value.NULL;

            LocalDate startDate = value1.toDate().atZone(context.getZone()).toLocalDate();
            LocalDate endDate = value2.toDate().atZone(context.getZone()).toLocalDate();
            long days = startDate.until(endDate, ChronoUnit.DAYS);
            return Value.of(days);
          }

        case WEEK:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
            return Value.of(dt.get(ChronoField.ALIGNED_WEEK_OF_YEAR));
          }

        case WEEKOFMONTH:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
            return Value.of(dt.get(ChronoField.ALIGNED_WEEK_OF_MONTH));
          }

        case HOUR:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            LocalTime time = LocalTime.from(value.toDate().atZone(context.getZone()));
            return Value.of(time.getHour());
          }

        case HOURS_BETWEEN:
          {
            Value value1 = args[0].eval(context);
            if (value1.isNull()) return Value.NULL;
            Value value2 = args[1].eval(context);
            if (value2.isNull()) return Value.NULL;

            LocalDateTime startDateTime =
                value1.toDate().atZone(context.getZone()).toLocalDateTime();
            LocalDateTime endDateTime = value2.toDate().atZone(context.getZone()).toLocalDateTime();
            long hours = startDateTime.until(endDateTime, ChronoUnit.HOURS);
            return Value.of(hours);
          }

        case MINUTE:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            LocalTime time = LocalTime.from(value.toDate().atZone(context.getZone()));
            return Value.of(time.getMinute());
          }

        case MINUTES_BETWEEN:
          {
            Value value1 = args[0].eval(context);
            if (value1.isNull()) return Value.NULL;
            Value value2 = args[1].eval(context);
            if (value2.isNull()) return Value.NULL;

            LocalDateTime startDateTime =
                value1.toDate().atZone(context.getZone()).toLocalDateTime();
            LocalDateTime endDateTime = value2.toDate().atZone(context.getZone()).toLocalDateTime();
            long minutes = startDateTime.until(endDateTime, ChronoUnit.MINUTES);
            return Value.of(minutes);
          }

        case SECOND:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            LocalTime time = LocalTime.from(value.toDate().atZone(context.getZone()));
            return Value.of(time.getSecond());
          }

        case SECONDS_BETWEEN:
          {
            Value value1 = args[0].eval(context);
            if (value1.isNull()) return Value.NULL;
            Value value2 = args[1].eval(context);
            if (value2.isNull()) return Value.NULL;

            LocalDateTime startDateTime =
                value1.toDate().atZone(context.getZone()).toLocalDateTime();
            LocalDateTime endDateTime = value2.toDate().atZone(context.getZone()).toLocalDateTime();
            long seconds = startDateTime.until(endDateTime, ChronoUnit.SECONDS);
            return Value.of(seconds);
          }

        case ABS:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            if (value.isBigNumber()) {
              return Value.of(value.toBigNumber().abs());
            }
            if (value.isNumber()) {
              return Value.of(Math.abs(value.toNumber()));
            }
            return Value.of(Math.abs(value.toInteger()));
          }

        case ACOS:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            Double d = value.toNumber();
            if (d < -1.0 || d > 1.0) {
              throw createArgumentOutOfRangeError(value);
            }
            return Value.of(Math.acos(d));
          }

        case ACOSH:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(FastMath.acosh(value.toNumber()));
          }

        case ASIN:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Math.asin(value.toNumber()));
          }

        case ASINH:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            return Value.of(FastMath.asinh(value.toNumber()));
          }

        case ATAN:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            return Value.of(Math.atan(value.toNumber()));
          }

        case ATANH:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            return Value.of(FastMath.atanh(value.toNumber()));
          }

        case ATAN2:
          {
            Value v0 = args[0].eval(context);
            if (v0.isNull()) return Value.NULL;
            Value v1 = args[1].eval(context);
            if (v1.isNull()) return Value.NULL;

            return Value.of(Math.atan2(v0.toNumber(), v1.toNumber()));
          }

        case BITGET:
          {
            Value v0 = args[0].eval(context);
            if (v0.isNull()) return Value.NULL;
            Value v1 = args[1].eval(context);
            if (v1.isNull()) return Value.NULL;

            return Value.of((v0.toInteger() & (1L << v1.toInteger())) != 0);
          }

        case CEIL:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            if (value.isInteger()) return value;
            if (value.isBigNumber()) {
              return Value.of(value.toBigNumber().setScale(0, RoundingMode.CEILING));
            }
            return Value.of(Math.ceil(value.toNumber()));
          }

        case COS:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Math.cos(value.toNumber()));
          }

        case COSH:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Math.cosh(value.toNumber()));
          }

        case COT:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            double d = Math.tan(value.toNumber());
            if (d == 0.0) {
              throw createDivisionByZeroError();
            }
            return Value.of(1. / d);
          }

        case DEGREES:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Math.toDegrees(value.toNumber()));
          }

        case FLOOR:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            if (value.isInteger()) return value;
            if (value.isBigNumber()) {
              return Value.of(value.toBigNumber().setScale(0, RoundingMode.FLOOR));
            }
            return Value.of(Math.floor(value.toNumber()));
          }

        case EXP:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Math.exp(value.toNumber()));
          }

        case LN:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            if (value.signum() <= 0) throw createArgumentOutOfRangeError(value);
            return Value.of(Math.log(value.toNumber()));
          }

        case LOG:
          {
            Value base = args[0].eval(context);

            if (base.isNull()) return Value.NULL;

            Value value = args[1].eval(context);
            if (value.isNull()) return Value.NULL;
            if (value.signum() <= 0) throw createArgumentOutOfRangeError(value);

            return Value.of(Math.log(value.toNumber()) / Math.log(base.toNumber()));
          }

        case LOG10:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            if (value.signum() <= 0) throw createArgumentOutOfRangeError(value);
            return Value.of(Math.log10(value.toNumber()));
          }

        case PI:
          return Value.PI;

        case RADIANS:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Math.toRadians(value.toNumber()));
          }

        case RAND:
          {
            if (args.length == 1) {
              Value value = args[0].eval(context);
              context.getRandom().setSeed(value.toInteger());
            }
            return Value.of(context.getRandom().nextDouble());
          }

        case ROUND:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return value;
            return Value.of(Math.round(value.toNumber()));
          }

        case SIGN:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return value;
            return Value.of(value.signum());
          }

        case SIN:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Math.sin(value.toNumber()));
          }

        case SINH:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Math.sinh(value.toNumber()));
          }

        case CBRT:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Math.cbrt(value.toNumber()));
          }

        case SQRT:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            if (value.signum() < 0) throw createArgumentOutOfRangeError(value);
            return Value.of(Math.sqrt(value.toNumber()));
          }

        case TAN:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Math.tan(value.toNumber()));
          }

        case TANH:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Math.tanh(value.toNumber()));
          }

        case ASCII:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            String string = value.toString();
            int ascii = 0;
            if (string.length() > 0) {
              ascii = string.charAt(0);
            }
            return Value.of(ascii);
          }

        case CHR:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            int codePoint = (int) value.toInteger();

            if (!Character.isValidCodePoint(codePoint)) {
              throw new ExpressionException(
                  BaseMessages.getString(PKG, "Expression.ArgumentOutOfRange", codePoint));
            }
            return Value.of(new String(Character.toChars(codePoint)));
          }

        case IF:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            return args[(value.toBoolean()) ? 1 : 2].eval(context);
          }

        case IFNULL:
        case COALESCE:
          {
            for (IExpression operand : args) {
              Value value = operand.eval(context);
              if (!value.isNull()) return value;
            }

            return Value.NULL;
          }

        case NVL2:
          {
            Value condition = args[0].eval(context);

            if (condition.isNull()) {
              return args[2].eval(context);
            }

            return args[1].eval(context);
          }

        case DECODE:
          {
            Value value = args[0].eval(context);

            int index = -1;
            for (int i = 1, len = args.length - 1; i < len; i += 2) {
              Value search = args[i].eval(context);
              if (value.compareTo(search) == 0) {
                index = i + 1;
                break;
              }
            }
            if (index < 0 && args.length % 2 == 0) {
              index = args.length - 1;
            }
            if (index < 0) return Value.NULL;

            return args[index].eval(context);
          }

        case GREATEST:
          {
            Value result = Value.NULL;
            for (IExpression operand : args) {
              Value value = operand.eval(context);
              if (result.compareTo(value) < 0) result = value;
            }

            return result;
          }

        case LEAST:
          {
            Value result = Value.NULL;
            for (IExpression operand : args) {
              Value value = operand.eval(context);
              // null is always smaller
              if (value.isNull()) continue;
              if (result.isNull() || value.compareTo(result) < 0) {
                result = value;
              }
            }

            return result;
          }

        case LENGTH:
          {
            Value value = args[0].eval(context);
            return this.getLenght(value, 1);
          }

        case UPPER:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(value.toString().toUpperCase(context.getLocale()));
          }

        case LOWER:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(value.toString().toLowerCase(context.getLocale()));
          }

        case INITCAP:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(WordUtils.capitalizeFully(value.toString()));
          }

        case TO_BOOLEAN:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            return value.convertTo(DataType.BOOLEAN);
          }
        case TRY_TO_BOOLEAN:
        {
          Value value = args[0].eval(context);
          if (value.isNull()) return Value.NULL;
          try {
            return value.convertTo(DataType.BOOLEAN);            
          } catch (Exception e) {
            return Value.NULL;
          }
        }

        case TO_CHAR:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            String format = null;
            if (args.length > 1) {
              Value v = args[1].eval(context);
              if (!v.isNull()) format = v.toString();
            }

            switch (value.getDataType()) {
              case INTEGER:
              case NUMBER:
              case BIGNUMBER:
                return Value.of(
                    NumberFormat.format(value.toBigNumber(), format, context.getLocale()));
              case DATE:
                ZonedDateTime datetime = value.toDate().atZone(context.getZone());

                if (format == null) format = "DD-MON-YY HH.MI.SS.FF PM";

                return Value.of(DateTimeFormat.format(datetime, format, context.getLocale()));
              case STRING:
                return value;
            }
          }

        case TO_NUMBER:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            // No format and no precision/scale
            if (args.length==1) {
              return Value.of(NumberFormat.parse(value.toString(),38,0));
            }

            Value v1 = args[1].eval(context);
            if ( v1.isString() )  {
              // TODO: if args==3 error
              BigDecimal result = NumberFormat.parse(value.toString(), v1.toString());
              if ( result==null )
                throw createInternalError("NULL NumberFormat  result");
                  
               return Value.of(result);
            }                       

            
            int precision= (int) v1.toInteger();
            Value v2 = args[2].eval(context);
            int scale=(int) v2.toInteger();
            return Value.of(NumberFormat.parse(value.toString(), precision, scale));
          }

        case TO_DATE:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            String format = null;
            if (args.length > 1) {
              Value v1 = args[1].eval(context);
              if (!v1.isNull()) format = v1.toString();
            }

            switch (value.getDataType()) {
              case DATE:
                return value;
              case STRING:
                Instant instant = DateTimeFormat.parse(value.toString(), format);
                return Value.of(instant);
            }
          }

        case TRY_TO_DATE:
        {
          Value value = args[0].eval(context);
          if (value.isNull()) return Value.NULL;

          String format = null;
          if (args.length > 1) {
            Value v1 = args[1].eval(context);
            if (!v1.isNull()) format = v1.toString();
          }

          switch (value.getDataType()) {
            case DATE:
              return value;
            case STRING:
              try {
                Instant instant = DateTimeFormat.parse(value.toString(), format);
              return Value.of(instant);
              } catch (Exception e) {
                return Value.NULL;
              }
          }
        }         
          
        case SOUNDEX:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Function.soundex(value.toString()));
          }

        case LEFT:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            Value count = args[1].eval(context);
            if (count.isNull()) return Value.NULL;

            return Value.of(Function.left(value.toString(), (int) count.toInteger()));
          }

        case NULLIF:
          {
            Value value = args[0].eval(context);
            Value compare = args[1].eval(context);

            if (value.compareTo(compare) == 0) return Value.NULL;

            return value;
          }

        case RIGHT:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return value;
            Value count = args[1].eval(context);
            if (count.isNull()) return Value.NULL;

            return Value.of(Function.right(value.toString(), (int) count.toInteger()));
          }

        case TRIM:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            String string = value.toString();
            String characters = null;

            if (args.length == 2) {
              Value strip = args[1].eval(context);
              if (!strip.isNull()) characters = strip.toString();
            }

            return Value.of(StringUtils.strip(string, characters));
          }

        case LTRIM:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            String string = value.toString();
            String characters = null;

            if (args.length == 2) {
              Value stripChars = args[1].eval(context);
              if (!stripChars.isNull()) characters = stripChars.toString();
            }

            return Value.of(StringUtils.stripStart(string, characters));
          }

        case RTRIM:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            String string = value.toString();
            String characters = null;

            if (args.length == 2) {
              Value stripChars = args[1].eval(context);
              if (!stripChars.isNull()) characters = stripChars.toString();
            }

            return Value.of(StringUtils.stripEnd(string, characters));
          }

        case REPEAT:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            Value number = args[1].eval(context);

            String s = value.toString();
            int count = (int) number.toInteger();
            StringBuilder builder = new StringBuilder(s.length() * count);
            while (count-- > 0) {
              builder.append(s);
            }
            return Value.of(builder.toString());
          }

        case REPLACE:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return value;
            Value param2 = args[1].eval(context);

            if (param2.isNull()) {
              return Value.NULL;
            }

            String string = value.toString();
            String search = param2.toString();

            if (args.length == 3) {
              Value param3 = args[2].eval(context);

              String replacement = param3.toString();
              return Value.of(string.replace(search, replacement));
            }

            String result = StringUtils.remove(string, search);
            return Value.of(result);
          }

        case INSTR:
          {
            Value v0 = args[0].eval(context);
            Value v1 = args[1].eval(context);

            if (v0.isNull() || v1.isNull()) {
              return Value.NULL;
            }

            String string = v0.toString();
            String pattern = v1.toString();

            // If 3 operands
            int start = 0;
            if (args.length == 3) {
              start = (int) args[2].eval(context).toInteger();

              if (start > 0) start -= 1;
              else if (start < 0) {
                return Value.of(string.lastIndexOf(pattern, string.length() + start) + 1);
              }
            }

            return Value.of(string.indexOf(pattern, start) + 1);
          }

        case SUBSTRING:
          {
            String string = args[0].eval(context).toString();
            int length = string.length();
            int start = (int) args[1].eval(context).toInteger();

            // These compatibility conditions violate the Standard
            if (start == 0) {
              start = 1;
            } else if (start < 0) {
              start = length + start + 1;
            }

            // Only 2 operands
            if (args.length == 2) {
              return Value.of(string.substring(start - 1));
            }

            int end = start + (int) args[2].eval(context).toInteger();
            // SQL Standard requires "data exception - substring error" when
            // end < start but expression does not throw it for compatibility
            start = Math.max(start, 1);
            end = Math.min(end, length + 1);
            if (start > length || end <= start) {
              // TODO: option to treatEmptyStringsAsNull
              return Value.NULL;
            }

            return Value.of(string.substring(start - 1, end - 1));
          }

        case EQUAL_NULL:
          {
            Value v0 = args[0].eval(context);
            Value v1 = args[1].eval(context);

            // Treats NULLs as known values
            if (v0.isNull() && v1.isNull()) {
              return Value.TRUE;
            }
            if (v0.isNull() || v1.isNull()) {
              return Value.FALSE;
            }

            return Value.of(v0.compareTo(v1) == 0);
          }

        case CONTAINS:
          {
            Value v0 = args[0].eval(context);
            Value v1 = args[1].eval(context);

            if (v0.isNull() && v1.isNull()) return Value.TRUE;

            if (v0.isNull() || v1.isNull()) return Value.FALSE;

            if (v0.toString().contains(v1.toString())) return Value.TRUE;

            return Value.FALSE;
          }

        case ENDSWITH:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return value;
            Value prefix = args[1].eval(context);

            if (value.isBinary()) {
              return Value.of(Function.endsWith(value.toBinary(), prefix.toBinary()));
            }

            return Value.of(value.toString().endsWith(prefix.toString()));
          }

        case STARTSWITH:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            Value prefix = args[1].eval(context);

            if (value.isBinary()) {
              return Value.of(Function.startsWith(value.toBinary(), prefix.toBinary()));
            }

            return Value.of(value.toString().startsWith(prefix.toString()));
          }

        case REGEXP_LIKE:
          {
            Value input = args[0].eval(context);
            Value pattern = args[1].eval(context);

            if (input.isNull() || pattern.isNull()) {
              return Value.FALSE;
            }

            Pattern p = Pattern.compile(pattern.toString(), Pattern.DOTALL);

            return Value.of(p.matcher(input.toString()).find());
          }

        case TRUNCATE:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            switch (value.getDataType()) {
              case INTEGER:
              case NUMBER:
              case BIGNUMBER:
                int scale = 0;
                if ( args.length==2) { 
                  Value pattern = args[1].eval(context);
                  if (pattern.isNull()) return Value.NULL;
                  scale = (int) pattern.toInteger();
                }
                return Value.of(Function.truncate(value.toBigNumber(), scale));

              case DATE:
                Instant instant = value.toDate();
                DatePart datePart = DatePart.DAY;
                
                if ( args.length==2) { 
                  Value pattern = args[1].eval(context);
                  if (pattern.isNull()) return Value.NULL;
                  datePart = DatePart.of(pattern.toString());
                }

                switch (datePart) {
                  // First day of the year
                  case YEAR:
                    {
                      ZonedDateTime datetime =
                          ZonedDateTime.ofInstant(
                              instant.truncatedTo(ChronoUnit.DAYS), context.getZone());
                      return Value.of(datetime.withDayOfYear(1).toInstant());
                    }
                  // First day of the month
                  case MONTH:
                    {
                      ZonedDateTime datetime =
                          ZonedDateTime.ofInstant(
                              instant.truncatedTo(ChronoUnit.DAYS), context.getZone());
                      return Value.of(datetime.withDayOfMonth(1).toInstant());
                    }
                  // First day of the quarter
                  case QUARTER:
                  {
                    ZonedDateTime datetime =
                        ZonedDateTime.ofInstant(
                            instant.truncatedTo(ChronoUnit.DAYS), context.getZone());
                    int month = (datetime.getMonthValue()/3)*3+1;                    
                    return Value.of(datetime.withMonth(month).withDayOfMonth(1).toInstant());
                  }
                  // First day of the week
                  case WEEK:
                  {
                    ZonedDateTime datetime =
                        ZonedDateTime.ofInstant(
                            instant.truncatedTo(ChronoUnit.DAYS), context.getZone());
                    
                    final Calendar calendar = Calendar.getInstance(context.getLocale());                  
                    DayOfWeek dow = DayOfWeek.of(calendar.getFirstDayOfWeek());
                    
                    return Value.of(datetime.with(TemporalAdjusters.previousOrSame(dow)).toInstant());
                  }

                  case DAY:
                    return Value.of(instant.truncatedTo(ChronoUnit.DAYS));
                  case HOUR:
                    return Value.of(instant.truncatedTo(ChronoUnit.HOURS));
                  case MINUTE:
                    return Value.of(instant.truncatedTo(ChronoUnit.MINUTES));
                  case SECOND:
                    return Value.of(instant.truncatedTo(ChronoUnit.SECONDS));
                  case MILLISECOND:
                    return Value.of(instant.truncatedTo(ChronoUnit.MILLIS));
                  case MICROSECOND:
                    return Value.of(instant.truncatedTo(ChronoUnit.MICROS));
                  case NANOSECOND:
                    return Value.of(instant.truncatedTo(ChronoUnit.NANOS));                 
                }

                return value;

              case STRING:
              case BOOLEAN:
              default:
                throw new ExpressionException("Truncate data type not implemented");
            }
          }

        case TRANSLATE:
          {
            Value stringValue = args[0].eval(context);
            Value findCharsValue = args[1].eval(context);
            Value replaceCharsValue = args[2].eval(context);

            if (stringValue.isNull() || findCharsValue.isNull()) {
              return Value.NULL;
            }

            String string = stringValue.toString();
            String findChars = findCharsValue.toString();
            String replaceChars = replaceCharsValue.toString();

            // if it stays null, then no replacements have been made
            StringBuilder buffer = null;
            // if shorter than findChars, then characters are removed
            // (if null, we don't access replaceChars at all)
            int replaceSize = replaceChars == null ? 0 : replaceChars.length();

            for (int i = 0, size = string.length(); i < size; i++) {
              char ch = string.charAt(i);
              int index = findChars.indexOf(ch);
              if (index >= 0) {
                if (buffer == null) {
                  buffer = new StringBuilder(size);
                  if (i > 0) {
                    buffer.append(string, 0, i);
                  }
                }
                if (index < replaceSize) {
                  ch = replaceChars.charAt(index);
                }
              }
              if (buffer != null) {
                buffer.append(ch);
              }
            }
            return Value.of(buffer == null ? string : buffer.toString());
          }

        case STRINGENCODE:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Function.stringEncode(value.toString()));
          }

        case STRINGDECODE:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            return Value.of(Function.stringDecode(value.toString()));
          }

        case URLENCODE:
          {
            Value value = args[0].eval(context);
            try {
              return Value.of(URLEncoder.encode(value.toString(), StandardCharsets.UTF_8.name()));
            } catch (Exception e) {
              throw new ExpressionException(BaseMessages.getString(PKG, "Error encoding url"), e);
            }
          }

        case URLDECODE:
          {
            Value value = args[0].eval(context);
            try {
              return Value.of(URLDecoder.decode(value.toString(), StandardCharsets.UTF_8.name()));
            } catch (Exception e) {
              throw new ExpressionException(BaseMessages.getString(PKG, "Error decoding url"), e);
            }
          }

        case UNICODE:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            String string = value.toString();
            int codePoint = 0;
            if (string.length() > 0) {
              codePoint = string.codePointAt(0);
            }
            return Value.of(codePoint);
          }

        case LPAD:
          {
            Value v0 = args[0].eval(context);
            if (v0.isNull()) return Value.NULL;

            Value v1 = args[1].eval(context);
            final int length = (int) v1.toInteger();

            // If this parameter is omitted, the function will pad spaces
            String pad = null;
            if (args.length == 3) {
              Value v2 = args[2].eval(context);
              pad = v2.toString();
            }

            return Value.of(Function.lpad(v0.toString(), length, pad));
          }

        case RPAD:
          {
            Value v0 = args[0].eval(context);
            if (v0.isNull()) return Value.NULL;

            Value v1 = args[1].eval(context);
            final int length = (int) v1.toInteger();

            // If this parameter is omitted, the function will pad spaces
            String pad = null;
            if (args.length == 3) {
              Value v2 = args[2].eval(context);
              pad = v2.toString();
            }

            return Value.of(Function.rpad(v0.toString(), length, pad));
          }

        case SPACE:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;
            int length = Math.max(0, (int) value.toInteger());
            char[] chars = new char[length];
            for (int i = length - 1; i >= 0; i--) {
              chars[i] = ' ';
            }

            return Value.of(new String(chars));
          }

        case REVERSE:
          {
            Value value = args[0].eval(context);
            if (value.isNull()) return Value.NULL;

            if (value.isBinary()) {
              return Value.of(Function.reverse(value.toBinary()));
            }

            return Value.of(Function.reverse(value.toString()));
          }

        case MD5:
          {
            Value value = args[0].eval(context);
            return getHash(value, "MD5");
          }

        case SHA1:
          {
            Value value = args[0].eval(context);
            return getHash(value, "SHA-1");
          }

        case SHA256:
          {
            Value value = args[0].eval(context);
            return getHash(value, "SHA-256");
          }

        case SHA384:
          {
            Value value = args[0].eval(context);
            return getHash(value, "SHA-384");
          }

        case SHA512:
          {
            Value value = args[0].eval(context);
            return getHash(value, "SHA-512");
          }
      }
    } catch (Exception e) {
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Expression.FunctionError", this.getName(), e.toString()), e);
    }

    throw createInternalError(kind.name());
  }

  @Override
  public IExpression optimize(IExpressionContext context, IExpression... operands)
      throws ExpressionException {

    IExpression[] args = new IExpression[operands.length];

    boolean isAllConstant = true;
    for (int index = 0; index < args.length; index++) {
      IExpression operand = operands[index];

      if (operand instanceof ExpressionCall) {
        operand = operand.optimize(context);
      }

      if (!operand.isConstant()) isAllConstant = false;

      args[index] = operand;
    }

    if (this.isDeterministic() && isAllConstant) {
      return eval(context, args);
    }

    return new ExpressionCall(this, args);
  }

  private Value getLenght(Value value, int size) {
    if (value.isNull()) return value;
    return Value.of(value.toString().length() * size);
  }

  private Value getHash(Value value, String algorithm) {
    if (value.isNull()) return value;

    try {
      MessageDigest md = MessageDigest.getInstance(algorithm);
      md.update(value.toBinary());
      String result = Hex.encodeHexString(md.digest());
      return Value.of(result);
    } catch (NoSuchAlgorithmException e) {
      new ExpressionException("Unknow algorithm: " + algorithm);
    }
    return Value.NULL;
  }

  public static String soundex(String s) {
    int len = s.length();
    char[] chars = {'0', '0', '0', '0'};
    char lastDigit = '0';
    for (int i = 0, j = 0; i < len && j < 4; i++) {
      char c = s.charAt(i);
      char newDigit = c > SOUNDEX.length ? 0 : SOUNDEX[c];
      if (newDigit != 0) {
        if (j == 0) {
          chars[j++] = c;
          lastDigit = newDigit;
        } else if (newDigit <= '6') {
          if (newDigit != lastDigit) {
            chars[j++] = newDigit;
            lastDigit = newDigit;
          }
        } else if (newDigit == '7') {
          lastDigit = newDigit;
        }
      }
    }
    return new String(chars);
  }

  public static int difference(String s0, String s1) {
    String result0 = soundex(s0);
    String result1 = soundex(s1);
    for (int i = 0; i < SOUNDEX_LENGTH; i++) {
      if (result0.charAt(i) != result1.charAt(i)) {
        return i;
      }
    }
    return SOUNDEX_LENGTH;
  }

  public static String lpad(String str, int length, String pad) {

    if (length < 0) {
      length = 0;
    } else if (length > PAD_LIMIT) {
      new ExpressionException("Paddind length exceeds maximum limit: " + PAD_LIMIT);
    }

    // If this parameter is omitted, the function will pad spaces
    if (pad == null) {
      pad = " ";
    }

    final int size = pad.length();
    final int index = length - str.length();

    if (index <= 0) {
      return str.substring(0, length);
    } else if (size == 0) {
      // nothing to do
    } else if (index == size) {
      return pad.concat(str);
    } else if (index < size) {
      return pad.substring(0, index).concat(str);
    } else {
      final char[] padding = new char[index];
      final char[] padChars = pad.toCharArray();
      for (int i = 0; i < index; i++) {
        padding[i] = padChars[i % size];
      }
      str = new String(padding).concat(str);
    }

    return str;
  }

  public static String rpad(String str, int length, String pad) {

    if (length < 0) {
      length = 0;
    }
    if (length > PAD_LIMIT) {
      new ExpressionException("Paddind length exceeds maximum limit: " + PAD_LIMIT);
    }

    // If this parameter is omitted, the function will pad spaces
    if (pad == null) {
      pad = " ";
    }

    final int size = pad.length();
    final int index = length - str.length();

    if (index <= 0) {
      return str.substring(0, length);
    } else if (size == 0) {
      // nothing to do
    } else if (index == size) {
      return str.concat(pad);
    } else if (index < size) {
      return str.concat(pad.substring(0, index));
    } else {
      final char[] padding = new char[index];
      final char[] padChars = pad.toCharArray();
      for (int i = 0; i < index; i++) {
        padding[i] = padChars[i % size];
      }
      str = str.concat(new String(padding));
    }

    return str;
  }

  public static BigDecimal truncate(BigDecimal b0, int b1) {
    return b0.movePointRight(b1).setScale(0, RoundingMode.DOWN).movePointLeft(b1);
  }

  public static double acosh(double x) {
    // return Math.log(x + Math.sqrt(x*x - 1.0d));
    return FastMath.acosh(x);
  }

  public static double asinh(double x) {
    // return Math.log(x + Math.sqrt(1 + x * x));
    return FastMath.asinh(x);
  }

  public static double atanh(double x) {
    // return Math.log(Math.sqrt(1 + x) / Math.sqrt(1 - x));
    return FastMath.atanh(x);
  }

  private static int makeRegexpFlags(String stringFlags) {
    int flags = 0;
    if (stringFlags != null) {
      for (int i = 0; i < stringFlags.length(); ++i) {
        switch (stringFlags.charAt(i)) {
          case 'i':
            flags |= Pattern.CASE_INSENSITIVE;
            break;
          case 'c':
            flags &= ~Pattern.CASE_INSENSITIVE;
            break;
          case 'n':
            flags |= Pattern.DOTALL;
            break;
          case 'm':
            flags |= Pattern.MULTILINE;
            break;
          default:
            throw new ExpressionException("Invalid input for Regexp");
        }
      }
    }
    return flags;
  }

  public static String left(String s, int count) {
    if (count < 0) {
      count = 0;
      // throw new ExpressionException("LEFT: Length must be greater than or equal to 0");
    } else if (count > s.length()) {
      count = s.length();
    }
    return s.substring(0, count);
  }

  public static String right(String s, int count) {
    if (count < 0) {
      count = 0;
      // throw new ExpressionException("RIGHT: Length must be greater than or equal to 0");
    } else if (count > s.length()) {
      count = s.length();
    }
    return s.substring(s.length() - count);
  }

  private static ExpressionException createFormatException(String s, int i) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Bad format {0} at position {1}", s, i));
  }

  /**
   * Decode a text that is encoded as a Java string literal. The Java properties file format and
   * Java source code format is supported.
   *
   * @param s the encoded string
   * @return the string
   */
  public static String stringDecode(String s) {
    int length = s.length();
    StringBuilder buff = new StringBuilder(length);
    for (int i = 0; i < length; i++) {
      char c = s.charAt(i);
      if (c == '\\') {
        if (i + 1 >= s.length()) {
          throw createFormatException(s, i);
        }
        c = s.charAt(++i);
        switch (c) {
          case 't':
            buff.append('\t');
            break;
          case 'r':
            buff.append('\r');
            break;
          case 'n':
            buff.append('\n');
            break;
          case 'b':
            buff.append('\b');
            break;
          case 'f':
            buff.append('\f');
            break;
          case '#':
            // for properties files
            buff.append('#');
            break;
          case '=':
            // for properties files
            buff.append('=');
            break;
          case ':':
            // for properties files
            buff.append(':');
            break;
          case '"':
            buff.append('"');
            break;
          case '\\':
            buff.append('\\');
            break;
          case 'u':
            {
              try {
                c = (char) (Integer.parseInt(s.substring(i + 1, i + 5), 16));
              } catch (NumberFormatException e) {
                throw createFormatException(s, i);
              }
              i += 4;
              buff.append(c);
              break;
            }
          default:
            if (c >= '0' && c <= '9') {
              try {
                c = (char) (Integer.parseInt(s.substring(i, i + 3), 8));
              } catch (NumberFormatException e) {
                throw createFormatException(s, i);
              }
              i += 2;
              buff.append(c);
            } else {
              throw createFormatException(s, i);
            }
        }
      } else {
        buff.append(c);
      }
    }
    return buff.toString();
  }

  /**
   * Convert a string to a Java literal using the correct escape sequences. The literal is not
   * enclosed in double quotes. The result can be used in properties files or in Java source code.
   *
   * @param s the text to convert
   */
  public static String stringEncode(String s) {
    StringBuilder builder = new StringBuilder(s.length());
    int length = s.length();
    for (int i = 0; i < length; i++) {
      char c = s.charAt(i);
      switch (c) {
        case '\t':
          // HT horizontal tab
          builder.append("\\t");
          break;
        case '\n':
          // LF linefeed
          builder.append("\\n");
          break;
        case '\f':
          // FF form feed
          builder.append("\\f");
          break;
        case '\r':
          // CR carriage return
          builder.append("\\r");
          break;
        case '"':
          // double quote
          builder.append("\\\"");
          break;
        case '\'':
          builder.append('\'');
          break;
        case '\\':
          // backslash
          builder.append("\\\\");
          break;
        default:
          if (c >= ' ' && (c < 0x80)) {
            builder.append(c);
          } else {
            builder
                .append("\\u")
                .append(HEX[c >>> 12])
                .append(HEX[c >>> 8 & 0xf])
                .append(HEX[c >>> 4 & 0xf])
                .append(HEX[c & 0xf]);
          }
      }
    }

    return builder.toString();
  }

  /**
   * Checks if this has the passed prefix
   *
   * @param prefix is a Bytes object to compare to this
   * @return true or false
   */
  public static boolean startsWith(byte[] data, byte[] prefix) {

    if (prefix.length > data.length) {
      return false;
    } else {
      int end = prefix.length;
      for (int i = 0; i < end; i++) {
        if (data[i] != prefix[i]) {
          return false;
        }
      }
    }
    return true;
  }

  public static byte[] reverse(byte[] data) {
    byte[] value = new byte[data.length];
    for (int i = data.length - 1, j = 0; i >= 0; i--, j++) {
      value[j] = data[i];
    }
    return value;
  }

  public static String reverse(String s) {
    StringBuilder builder = new StringBuilder(s).reverse();
    return builder.toString();
  }

  /**
   * Checks if this has the passed suffix
   *
   * @param suffix is a Bytes object to compare to this
   * @return true or false
   * @since 1.1.0
   */
  public static boolean endsWith(byte[] data, byte[] suffix) {
    int startOffset = data.length - suffix.length;

    if (startOffset < 0) {
      return false;
    } else {
      int end = startOffset + suffix.length;
      for (int i = startOffset; i < end; i++) {
        if (data[i] != suffix[i]) {
          return false;
        }
      }
    }
    return true;
  }
}
