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
package org.apache.hop.expression.util;

import java.text.ParseException;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Expression date/time format model for <code>TO_DATE(string, format)</code> and
 * <code>TO_CHAR(datetime, format)</code> functions..
 *
 * <p>
 *
 * <table border="1">
 * <th>
 * <td>Input</td>
 * <td>Output</td>
 * <td>Closest {@link ZonedDateTimeFormat} Equivalent</td></th>
 * <tr>
 * <td>- / , . ; : "text"</td>
 * <td>Reproduced verbatim.</td>
 * <td>'text'</td>
 * </tr>
 * <tr>
 * <td>A.D. AD B.C. BC</td>
 * <td>Era designator, with or without periods.</td>
 * <td>G</td>
 * </tr>
 * <tr>
 * <td>A.M. AM P.M. PM</td>
 * <td>AM/PM marker.</td>
 * <td>a</td>
 * </tr>
 * <tr>
 * <td>CC SCC</td>
 * <td>Century.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>D</td>
 * <td>Day of week.</td>
 * <td>u</td>
 * </tr>
 * <tr>
 * <td>DAY</td>
 * <td>Name of day.</td>
 * <td>EEEE</td>
 * </tr>
 * <tr>
 * <td>DY</td>
 * <td>Abbreviated day name.</td>
 * <td>EEE</td>
 * </tr>
 * <tr>
 * <td>DD</td>
 * <td>Day of month.</td>
 * <td>d</td>
 * </tr>
 * <tr>
 * <td>DDD</td>
 * <td>Day of year.</td>
 * <td>D</td>
 * </tr>
 * <tr>
 * <td>DL</td>
 * <td>Long date format.</td>
 * <td>EEEE, MMMM d, yyyy</td>
 * </tr>
 * <tr>
 * <td>DS</td>
 * <td>Short date format.</td>
 * <td>MM/dd/yyyy</td>
 * </tr>
 * <tr>
 * <td>E</td>
 * <td>Abbreviated era name (Japanese, Chinese, Thai)</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>EE</td>
 * <td>Full era name (Japanese, Chinese, Thai)</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>FF[1-9]</td>
 * <td>Fractional seconds.</td>
 * <td>S</td>
 * </tr>
 * <tr>
 * <td>FM</td>
 * <td>Returns values with no leading or trailing spaces.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>FX</td>
 * <td>Requires exact matches between character data and format model.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>HH HH12</td>
 * <td>Hour in AM/PM (1-12).</td>
 * <td>hh</td>
 * </tr>
 * <tr>
 * <td>HH24</td>
 * <td>Hour in day (0-23).</td>
 * <td>HH</td>
 * </tr>
 * <tr>
 * <td>IW</td>
 * <td>Week in year.</td>
 * <td>w</td>
 * </tr>
 * <tr>
 * <td>WW</td>
 * <td>Week in year.</td>
 * <td>w</td>
 * </tr>
 * <tr>
 * <td>W</td>
 * <td>Week in month.</td>
 * <td>W</td>
 * </tr>
 * <tr>
 * <td>IYYY IYY IY I</td>
 * <td>Last 4/3/2/1 digit(s) of ISO year.</td>
 * <td>yyyy yyy yy y</td>
 * </tr>
 * <tr>
 * <td>RRRR RR</td>
 * <td>Last 4/2 digits of year.</td>
 * <td>yyyy yy</td>
 * </tr>
 * <tr>
 * <td>Y,YYY</td>
 * <td>Year with comma.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>YEAR SYEAR</td>
 * <td>Year spelled out (S prefixes BC years with minus sign).</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>YYYY SYYYY</td>
 * <td>4-digit year (S prefixes BC years with minus sign).</td>
 * <td>yyyy</td>
 * </tr>
 * <tr>
 * <td>YYY YY Y</td>
 * <td>Last 3/2/1 digit(s) of year.</td>
 * <td>yyy yy y</td>
 * </tr>
 * <tr>
 * <td>J</td>
 * <td>Julian day (number of days since January 1, 4712 BC).</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>MI</td>
 * <td>Minute in hour.</td>
 * <td>mm</td>
 * </tr>
 * <tr>
 * <td>MM</td>
 * <td>Month in year.</td>
 * <td>MM</td>
 * </tr>
 * <tr>
 * <td>MON</td>
 * <td>Abbreviated name of month.</td>
 * <td>MMM</td>
 * </tr>
 * <tr>
 * <td>MONTH</td>
 * <td>Name of month, padded with spaces.</td>
 * <td>MMMM</td>
 * </tr>
 * <tr>
 * <td>RM</td>
 * <td>Roman numeral month.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>Q</td>
 * <td>Quarter of year.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>SS</td>
 * <td>Seconds in minute.</td>
 * <td>ss</td>
 * </tr>
 * <tr>
 * <td>SSSSS</td>
 * <td>Seconds in day.</td>
 * <td>None.</td>
 * </tr>
 * <tr>
 * <td>TS</td>
 * <td>Short time format.</td>
 * <td>h:mm:ss aa</td>
 * </tr>
 * <tr>
 * <td>TZD</td>
 * <td>Daylight savings time zone abbreviation.</td>
 * <td>z</td>
 * </tr>
 * <tr>
 * <td>TZR</td>
 * <td>Time zone region information.</td>
 * <td>zzzz</td>
 * </tr>
 * <tr>
 * <td>X</td>
 * <td>Local radix character.</td>
 * <td>None.</td>
 * </tr>
 * </table>
 */
public abstract class DateTimeFormat extends BaseFormat {

  private static final Map<String, DateTimeFormat> cache = new ConcurrentHashMap<>();

  public static DateTimeFormat of(String pattern) {
    if (pattern == null) {
      pattern = "YYYY-MM-DD";
    }

    return cache.computeIfAbsent(pattern, DateTimeFormat::create);
  }

  private static DateTimeFormat create(String pattern) {
    if (pattern.indexOf('|') >= 0) {
      List<DateTimeFormat> formats = new ArrayList<>();
      for (String p : pattern.split("\\|")) {
        DateTimeFormat format = new ZonedDateTimeFormat(p);
        formats.add(format);
      }
      return new CompositeDateTimeFormat(pattern, formats.toArray(new ZonedDateTimeFormat[0]));
    }

    return new ZonedDateTimeFormat(pattern);
  }

  public abstract ZonedDateTime parse(String text) throws ParseException;

  /**
   * <p>
   * See also TO_CHAR(datetime) and datetime format models.
   *
   * @param value the date-time value to format
   * @return the formatted timestamp
   */
  public abstract String format(ZonedDateTime value);

  /**
   * Specifies the century start year for 2-digit years (Default is 1970).
   * This parameter prevents year ambiguous dates when parsing date with the YY date format
   * component.
   */
  public abstract void setTwoDigitYearStart(int year);
}
