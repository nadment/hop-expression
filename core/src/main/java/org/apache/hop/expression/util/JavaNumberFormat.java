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

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class JavaNumberFormat {

  // DecimalFormat is not thread-safe!
  private static class DecimalFormatThreadLocal extends ThreadLocal<DecimalFormat> {

    private final String format;

    public DecimalFormatThreadLocal(final String format) {
      this.format = format;
    }

    @Override
    protected synchronized DecimalFormat initialValue() {

      DecimalFormat decimalFormat = new DecimalFormat(format);
      decimalFormat.setParseBigDecimal(true);

      return decimalFormat;
    }
  };

  private static final Map<String, DecimalFormatThreadLocal> cache = new ConcurrentHashMap<>();

  /**
   * Returns a cached {@link DecimalFormat} instance
   *
   * @param format the number format model.
   * @return the DecimalFormat instance
   */
  private static DecimalFormat getDecimalFormat(final String format) {
    DecimalFormatThreadLocal threadLocal = cache.computeIfAbsent(format, f -> new DecimalFormatThreadLocal(f));

    return threadLocal.get();
  }

  /**
   * Format number with Java number format model.
   *
   * @param number the number to format
   * @param locale the locale to use
   * @return the formatted number
   */
  public static String format(BigDecimal value, String format, Locale locale) {
    DecimalFormat decimalFormat = getDecimalFormat(format);
    DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale); 
    decimalFormat.setDecimalFormatSymbols(symbols);
    
    return decimalFormat.format(value);
  }

  public static final BigDecimal parse(String value, String format, Locale locale) throws ParseException {
    DecimalFormat decimalFormat = getDecimalFormat(format);
    DecimalFormatSymbols symbols = DecimalFormatSymbols.getInstance(locale); 
    decimalFormat.setDecimalFormatSymbols(symbols);
    return (BigDecimal) decimalFormat.parse(value);
  }
}
