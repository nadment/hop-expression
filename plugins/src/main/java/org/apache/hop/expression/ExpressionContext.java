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
import org.apache.hop.core.logging.ILogChannel;
import org.apache.hop.core.logging.LogChannel;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variable;
import org.apache.hop.i18n.BaseMessages;
import java.security.SecureRandom;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.TextStyle;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;
import java.util.TimeZone;
import javax.script.SimpleScriptContext;

public class ExpressionContext extends SimpleScriptContext implements IExpressionContext {

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes
  
  private static final ILogChannel log = new LogChannel("Expression");
  
  /**
   * This parameter prevents ambiguous dates when importing or converting data with the YY date
   * format.
   * Control Two-digit year, when set to 1980, values of 79 and 80 parsed as 2079 and 1980
   * respectively.
   */
  public static final String EXPRESSION_TWO_DIGIT_CENTURY_START = "EXPRESSION_TWO_DIGIT_CENTURY_START";

  /**
   * The date format used for conversions between dates and strings.
   */
  @Variable(value = "YYYY-MM-DD", description = "The default date format used by expression for conversions between dates and strings.")
  public static final String EXPRESSION_DATE_FORMAT = "EXPRESSION_DATE_FORMAT";

  /**
   * The timestamp format used for conversions between timestamps and strings
   */
  @Variable(value = "YYYY-MM-DD H24:MI:SS", description = "The default timestamp format used by expression for conversions between timestamps and strings.")
  public static final String EXPRESSION_TIMESTAMP_FORMAT = "EXPRESSION_TIMESTAMP_FORMAT";

  /**
   * Defines the first day of a week.
   * The integer value follows the ISO-8601 standard, from 1 (Monday) to 7 (Sunday).
   */
  public static final String EXPRESSION_FIRST_DAY_OF_WEEK = "EXPRESSION_FIRST_DAY_OF_WEEK";

  public static final String CACHED_TODAY = "__TODAY";
  public static final String CACHED_NOW = "__NOW";
  public static final String CACHED_TIMEZONE = "__TIMEZONE";
  public static final String CACHED_RANDOM = "__RANDOM";

  private IRowMeta rowMeta;
  private Object[] row;

  private int twoDigitCenturyStart = 1970;

  public ExpressionContext(IVariables variables, IRowMeta rowMeta) {
    this(variables);

    this.rowMeta = Objects.requireNonNull(rowMeta);
  }

  public ExpressionContext(IVariables variables) {
    super();
    
    this.setAttribute(EXPRESSION_DATE_FORMAT, variables.getVariable(EXPRESSION_DATE_FORMAT, "YYYY-MM-DD"), ENGINE_SCOPE);
    this.setAttribute(EXPRESSION_FIRST_DAY_OF_WEEK, variables.getVariable(EXPRESSION_FIRST_DAY_OF_WEEK, "1"), ENGINE_SCOPE);           
    this.setAttribute(EXPRESSION_TWO_DIGIT_CENTURY_START, variables.getVariable(EXPRESSION_TWO_DIGIT_CENTURY_START, "1970"), ENGINE_SCOPE);

    
//    final Calendar calendar = Calendar.getInstance(locale);
//    DayOfWeek dow = DayOfWeek.of(calendar.getFirstDayOfWeek());


    try {
      String variable = variables.getVariable(EXPRESSION_TWO_DIGIT_CENTURY_START, "1970");
      this.twoDigitCenturyStart = Integer.parseInt(variable);
    } catch (NumberFormatException e) {
      log.logError(BaseMessages.getString(PKG, "Expression.InvalidVariable", EXPRESSION_TWO_DIGIT_CENTURY_START));
    }
   
    // Initialize
    ZonedDateTime now = ZonedDateTime.now();
    this.setAttribute(CACHED_TIMEZONE, ZoneId.systemDefault().getId());
    this.setAttribute(CACHED_NOW, now, ENGINE_SCOPE);
    this.setAttribute(CACHED_TODAY, now.truncatedTo(ChronoUnit.DAYS), ENGINE_SCOPE);
    this.setAttribute(CACHED_RANDOM, new SecureRandom(), ENGINE_SCOPE);
  }

  public void setAttribute(String name, Object value) {
    this.setAttribute(name, value, ENGINE_SCOPE);
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
      throw new ExpressionException(Error.UNRESOLVED_IDENTIFIER, name);

    int index = rowMeta.indexOfValue(name);
    if (index < 0)
      throw new ExpressionException(Error.UNRESOLVED_IDENTIFIER, name);

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
          return date.toInstant().atZone(ZoneId.systemDefault());          
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
          throw new ExpressionException(Error.UNSUPPORTED_IDENTIFIER_DATATYPE, name, valueMeta.getTypeDesc());
      }
    } catch (HopValueException e) {
      throw new ExpressionException(Error.UNRESOLVED_IDENTIFIER, name);
    }
  }

  public int getTwoDigitCenturyStart() {
    return twoDigitCenturyStart;
  }
}

