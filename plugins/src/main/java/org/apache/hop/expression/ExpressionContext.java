/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression;

import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.i18n.BaseMessages;
import java.security.SecureRandom;
import java.time.Clock;
import java.time.DayOfWeek;
import java.time.Instant;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;
import javax.script.ScriptContext;
import javax.script.SimpleScriptContext;

public class ExpressionContext extends SimpleScriptContext implements IExpressionContext {

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  /** The UTC time zone. */
  protected static final ZoneId UTC_ZONE = ZoneId.of("UTC");

  /**
   * This parameter prevents ambiguous dates when importing or converting data with the YY date
   * format.
   * Control Two-digit year, when set to 1980, values of 79 and 80 parsed as 2079 and 1980
   * respectively.
   */
  public static final String TWO_DIGIT_CENTURY_START = "TWO_DIGIT_CENTURY_START";

  /**
   * The date format used for conversions between dates and strings.
   */
  public static final String NLS_DATE_FORMAT = "NLS_DATE_FORMAT";

  /**
   * The timestamp format used for conversions between timestamps and strings
   */
  public static final String NLS_TIMESTAMP_FORMAT = "NLS_TIMESTAMP_FORMAT";

  /**
   * Defines the first day of a week.
   * The integer value follows the ISO-8601 standard, from 1 (Monday) to 7 (Sunday).
   */
  public static final String NLS_FIRST_DAY_OF_WEEK = "WEEK_START";

  public static final String ATTRIBUTE_TODAY = "TODAY";
  public static final String ATTRIBUTE_NOW = "NOW";
  public static final String ATTRIBUTE_RANDOM = "RANDOM";

  private IRowMeta rowMeta;

  private Object[] row;
  private ZoneId zone;
  private Locale locale;
  private int twoDigitCenturyStart = 1970;

  public ExpressionContext(IVariables variables, IRowMeta rowMeta) {
    this(variables);

    this.rowMeta = Objects.requireNonNull(rowMeta);
  }

  public ExpressionContext(IVariables variables) {
    super();

    this.locale = Locale.getDefault();
    this.zone = ZoneId.systemDefault();

    
    this.setAttribute(NLS_DATE_FORMAT, variables.getVariable(NLS_DATE_FORMAT, "YYYY-MM-DD"),
        ScriptContext.ENGINE_SCOPE);
    this.setAttribute(NLS_FIRST_DAY_OF_WEEK, variables.getVariable(NLS_FIRST_DAY_OF_WEEK, "1"),
        ScriptContext.ENGINE_SCOPE);
    this.setAttribute(TWO_DIGIT_CENTURY_START,
        variables.getVariable(TWO_DIGIT_CENTURY_START, "1970"), ScriptContext.ENGINE_SCOPE);


    final Calendar calendar = Calendar.getInstance(locale);
    DayOfWeek dow = DayOfWeek.of(calendar.getFirstDayOfWeek());


    try {
      String variable = variables.getVariable(TWO_DIGIT_CENTURY_START, "1970");
      this.twoDigitCenturyStart = Integer.parseInt(variable);
    } catch (NumberFormatException e) {
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Expression.InvalidVariable", TWO_DIGIT_CENTURY_START));
    }

    // Initialize
    
   // Clock clock = Clock.withZone(UTC_ZONE);
    this.setAttribute(ATTRIBUTE_NOW, Instant.now(), ScriptContext.ENGINE_SCOPE);
    this.setAttribute(ATTRIBUTE_TODAY, Instant.now().truncatedTo(ChronoUnit.DAYS), ScriptContext.ENGINE_SCOPE);
    this.setAttribute(ATTRIBUTE_RANDOM, new SecureRandom(), ScriptContext.ENGINE_SCOPE);
  }

  public IRowMeta getRowMeta() {
    return rowMeta;
  }

  public void setRow(Object[] row) {
    this.row = row;
  }

  @Override
  public Object resolve(String name) throws ExpressionException {

    if (rowMeta == null)
      throw new ExpressionException(BaseMessages.getString(PKG, "Expression.NoRowMeta", name));

    int index = rowMeta.indexOfValue(name);
    if (index < 0)
      throw new ExpressionException(BaseMessages.getString(PKG, "Expression.FieldNotFound", name));

    IValueMeta valueMeta = rowMeta.getValueMeta(index);
    try {
      switch (valueMeta.getType()) {
        case IValueMeta.TYPE_BOOLEAN:
          return rowMeta.getBoolean(row, index);
        case IValueMeta.TYPE_DATE:
        case IValueMeta.TYPE_TIMESTAMP:
          // No getTimestamp from RowMeta ???
          Date date = rowMeta.getDate(row, index);
          if (date == null)
            return null;
          return date.toInstant();
        case IValueMeta.TYPE_STRING:
          return rowMeta.getString(row, index);
        case IValueMeta.TYPE_INTEGER:
          return rowMeta.getInteger(row, index);
        case IValueMeta.TYPE_NUMBER:
          return rowMeta.getNumber(row, index);
        case IValueMeta.TYPE_BIGNUMBER:
          return rowMeta.getBigNumber(row, index);
        case IValueMeta.TYPE_BINARY:
          return rowMeta.getBinary(row, index);
        default:
          throw new ExpressionException(BaseMessages.getString(PKG,
              "Expression.ValueMetaTypeNotSupported", name, valueMeta.getType()));
      }
    } catch (HopValueException e) {
      throw new ExpressionException("Error resolve field value " + name + ":" + e.toString());
    }
  }
  
  public ZoneId getZone() {
    return zone;
  }

  // public void setZone(ZoneId zone) {
  // this.zone = zone;
  // }

  public int getTwoDigitCenturyStart() {
    return twoDigitCenturyStart;
  }

  public int getFirstDayOfWeek() {
    return 1;
  }
}

