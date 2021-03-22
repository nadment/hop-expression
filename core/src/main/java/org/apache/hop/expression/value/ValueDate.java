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
package org.apache.hop.expression.value;

import java.io.StringWriter;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Locale;
import java.util.Objects;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Value;
import org.apache.hop.expression.util.DateFormat;

public class ValueDate extends Value {

  private static final DateTimeFormatter TIMESTAMP_FORMAT =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.nnn")
          .withLocale(Locale.ROOT)
          .withZone(ZoneId.systemDefault());
  private static final DateTimeFormatter DATE_FORMAT =
      DateTimeFormatter.ofPattern("yyyy-MM-dd")
          .withLocale(Locale.ROOT)
          .withZone(ZoneId.systemDefault());

  private static final double SECONDS_BY_DAY = 24D * 60 * 60;

  private final Instant value;

  public ValueDate(Instant date) {
    this.value = Objects.requireNonNull(date);
  }

  @Override
  public DataType getType() {
    return DataType.DATE;
  }

  @Override
  public Object getObject() {
    return value;
  }

  @Override
  public int hashCode() {
    return value.hashCode();
  }

  @Override
  public boolean equals(Object other) {
    return other instanceof ValueDate && value.equals(((ValueDate) other).value);
  }

  @Override
  public int compare(Value v) {
    return value.compareTo(v.toDate());
  }

  @Override
  public Value eval(IExpressionContext context) throws ExpressionException {
    return this;
  }
  
  @Override
  public Instant toDate() {
    return value;
  }

  @Override
  public String toString() {
    if (value.getNano() > 0) return TIMESTAMP_FORMAT.format(value);

    return DATE_FORMAT.format(value);
  }

  //	/**
  //	 * Converts this date time to the number of milliseconds from the epochof
  //	 * 1970-01-01T00:00:00Z.
  //	 */
  //	public long toInteger() {
  //		long time = value.toEpochMilli();
  //
  //		return time;
  //	}
  //
  //	/**
  //	 * Converts this date time to the number of milliseconds from the epochof
  //	 * 1970-01-01 00:00:00.
  //	 */
  //	public double toNumber() {
  //		long time = value.toEpochMilli();
  //
  //		return (double) time;
  //	}

  @Override
  public Value convertTo(
      final IExpressionContext context, final DataType targetType, String format) {

    if (targetType == DataType.STRING) {
      ZonedDateTime dt = ZonedDateTime.ofInstant(value, context.getZone());
      String result = DateFormat.format(dt, format, context.getLocale());

      return new ValueString(result);
    }

    throw createUnsupportedConversionError(targetType);
  }

  @Override
  public Value add(Value v) {
    // Computes fraction of day
    if (v.isNumeric()) {
      long seconds = (long) (v.toNumber() * SECONDS_BY_DAY);
      return Value.of(value.plusSeconds(seconds));
    }

    return super.add(v);
  }

  @Override
  public Value subtract(Value v) {
    if (v.isNumeric()) {
      long seconds = (long) (v.toNumber() * SECONDS_BY_DAY);
      return Value.of(value.minusSeconds(seconds));
    }
    if (v.isDate()) {
      long seconds = v.toDate().until(value, ChronoUnit.SECONDS);
      // Date diff return fraction of day
      return Value.of(seconds / SECONDS_BY_DAY);
    }

    return super.subtract(v);
  }

  public void write(StringWriter writer, int leftPrec, int rightPrec) {
    writer.append(this.toString());
  }
}
