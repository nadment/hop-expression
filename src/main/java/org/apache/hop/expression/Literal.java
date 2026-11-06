/*
 * ' * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
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

import ch.obermuhlner.math.big.BigDecimalMath;
import com.fasterxml.jackson.databind.JsonNode;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.net.InetAddress;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Objects;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.EnumType;
import org.apache.hop.expression.type.InetType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.IntervalType;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.UnknownType;
import org.apache.hop.expression.util.BaseFormat;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.StringConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Constant value in an expression. */
@NullMarked
public final class Literal implements IExpression {

  /** Literal null value without a known data type */
  public static final Literal NULL = new Literal(null, UnknownType.UNKNOWN);

  /** Literal null with binary data type */
  public static final Literal NULL_BINARY = new Literal(null, BinaryType.BINARY);

  /** Literal null with boolean data type */
  public static final Literal NULL_BOOLEAN = new Literal(null, BooleanType.BOOLEAN);

  /** Literal null with string data type */
  public static final Literal NULL_STRING = new Literal(null, StringType.STRING);

  /** Literal null with integer data type */
  public static final Literal NULL_INTEGER = new Literal(null, IntegerType.INTEGER);

  /** Literal null with number data type */
  public static final Literal NULL_NUMBER = new Literal(null, NumberType.NUMBER);

  /** Literal null with date data type */
  public static final Literal NULL_DATE = new Literal(null, DateType.DATE);

  /** Literal null with JSON data type */
  public static final Literal NULL_JSON = new Literal(null, JsonType.JSON);

  /** Literal null with INET data type */
  public static final Literal NULL_INET = new Literal(null, InetType.INET);

  /** Literal null with interval data type */
  public static final Literal NULL_INTERVAL = new Literal(null, IntervalType.INTERVAL);

  /** Literal true value with boolean data type */
  public static final Literal TRUE = new Literal(Boolean.TRUE, BooleanType.BOOLEAN_NOT_NULL);

  /** Literal false value with boolean data type */
  public static final Literal FALSE = new Literal(Boolean.FALSE, BooleanType.BOOLEAN_NOT_NULL);

  /** Literal 0 value with integer data type */
  public static final Literal ZERO = new Literal(0L, IntegerType.of(1, false));

  /** Literal 1 value with integer data type */
  public static final Literal ONE = new Literal(1L, IntegerType.of(1, false));

  /** The data type of this literal. */
  private final Type type;

  /** The value of this literal. */
  private final @Nullable Object value;

  /**
   * Create a typed literal value
   *
   * @param value the constant value
   * @param type the type of the value
   */
  public Literal(@Nullable Object value, Type type) {
    this.value = value;
    this.type = type;
  }

  public static Literal of(@Nullable Boolean value) {
    if (value == null) return NULL_BOOLEAN;
    return value ? TRUE : FALSE;
  }

  public static Literal of(byte @Nullable [] value) {
    if (value == null) return NULL_BINARY;
    return new Literal(value, BinaryType.from(value));
  }

  public static Literal of(@Nullable String value) {
    if (value == null) return NULL_STRING;
    return new Literal(value, StringType.from(value));
  }

  public static Literal of(@Nullable Integer value) {
    if (value == null) return NULL_INTEGER;
    if (value == 0) return ZERO;
    if (value == 1) return ONE;
    Long longValue = value.longValue();
    return new Literal(longValue, IntegerType.from(longValue));
  }

  public static Literal of(@Nullable Long value) {
    if (value == null) return NULL_INTEGER;
    if (value == 0L) return ZERO;
    if (value == 1L) return ONE;
    return new Literal(value, IntegerType.from(value));
  }

  public static Literal of(@Nullable BigDecimal number) {
    if (number == null) return NULL_NUMBER;

    if (BigDecimal.ZERO.compareTo(number) == 0) {
      return ZERO;
    }
    if (BigDecimal.ONE.compareTo(number) == 0) {
      return ONE;
    }
    if (BigDecimalMath.isLongValue(number)) {
      Long longValue = number.longValueExact();
      return new Literal(longValue, IntegerType.from(longValue));
    }

    return new Literal(number, NumberType.from(number));
  }

  public static Literal of(@Nullable Interval value) {
    if (value == null) return NULL_INTERVAL;
    return new Literal(value, IntervalType.INTERVAL_NOT_NULL);
  }

  public static Literal of(@Nullable ZonedDateTime value) {
    if (value == null) return NULL_DATE;
    return new Literal(value, DateType.DATE_NOT_NULL);
  }

  public static Literal of(@Nullable JsonNode value) {
    if (value == null) return NULL_JSON;
    return new Literal(value, JsonType.JSON_NOT_NULL);
  }

  public static Literal of(@Nullable InetAddress value) {
    if (value == null) return NULL_INET;
    return new Literal(value, InetType.INET_NOT_NULL);
  }

  /**
   * Creates a data type literal, as used by CAST or JSON_VALUE.
   *
   * <p>The data type is both the value and the type of the literal: the value keeps the literal
   * distinct from a typed NULL, and {@link #getType()} gives the data type without conversion.
   *
   * @param type the data type
   * @return the literal
   */
  public static Literal of(@Nullable Type type) {
    if (type == null) return NULL;
    return new Literal(type, type);
  }

  public static Literal of(@Nullable TimeUnit value) {
    if (value == null) return NULL;
    return new Literal(value, EnumType.TIMEUNIT);
  }

  public static Literal of(@Nullable BaseFormat value) {
    if (value == null) return NULL;
    return new Literal(value, UnknownType.UNKNOWN);
  }

  @Override
  public @Nullable Object getValue() {
    return value;
  }

  @Override
  public @Nullable <T> T getValue(final Class<T> clazz) {
    return type.convert(value, clazz);
  }

  @Override
  public Kind getKind() {
    return Kind.LITERAL;
  }

  @Override
  public int getCost() {
    return 1;
  }

  @Override
  public Type getType() {
    return type;
  }

  @Override
  public int hashCode() {
    // Consistent with equals, which compares only the value
    return Objects.hashCode(value);
  }

  @Override
  public boolean isNull() {
    return value == null;
  }

  @Override
  public boolean equals(Object other) {
    if (other instanceof Literal o) {
      if (value == null) {
        return (o.value == null);
      }
      return value.equals(o.value);
    }
    return false;
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    unparse(writer, 0, 0);
    return writer.toString();
  }

  @Override
  public void unparse(final StringWriter writer, int leftPrec, int rightPrec) {

    if (value == null) {
      writer.append("NULL");
    } else if (value instanceof Type) {
      // Data type literal
      writer.append(String.valueOf(value));
    } else
      switch (type.getName()) {
        case STRING:
          unparseString(writer, (String) value);
          break;
        case BOOLEAN:
          writer.append(StringConversion.convert((boolean) value));
          break;
        case INTEGER:
          writer.append(StringConversion.convert((Long) value));
          break;
        case NUMBER:
          writer.append(StringConversion.convert((BigDecimal) value));
          break;
        case BINARY:
          unparseBinary(writer, (byte[]) value);
          break;
        case DATE:
          unparseDate(writer, (ZonedDateTime) value, false);
          break;
        case TIMESTAMP:
          unparseDate(writer, (ZonedDateTime) value, true);
          break;
        case INTERVAL:
          unparseInterval(writer, (Interval) value);
          break;
        case JSON:
          writer.append("JSON '");
          writer.append(StringConversion.convert((JsonNode) value));
          writer.append('\'');
          break;
        case INET:
          writer.append("INET '");
          writer.append(StringConversion.convert((InetAddress) value));
          writer.append('\'');
          break;
        case UNKNOWN:
        default:
          writer.append(String.valueOf(value));
      }
  }

  /** Unparses a string literal, a single quote is doubled. */
  private static void unparseString(final StringWriter writer, final String str) {
    writer.append('\'');
    for (int i = 0; i < str.length(); i++) {
      char ch = str.charAt(i);
      writer.append(ch);
      if (ch == '\'') {
        writer.append(ch);
      }
    }
    writer.append('\'');
  }

  /** Unparses a binary literal in hexadecimal. */
  private static void unparseBinary(final StringWriter writer, final byte[] bytes) {
    writer.append("BINARY '");
    for (byte b : bytes) {
      writer.append(byteToHex((b >> 4) & 0xF));
      writer.append(byteToHex(b & 0xF));
    }
    writer.append('\'');
  }

  private static char byteToHex(int digit) {
    if (digit < 10) {
      return (char) ('0' + digit);
    }
    return (char) ('A' - 10 + digit);
  }

  /**
   * Unparses a date literal, or a timestamp literal if it has a time, with its time zone if it is
   * not the system time zone.
   *
   * @param writer the writer
   * @param datetime the date-time
   * @param withTimeZone true to always write the time zone, for a TIMESTAMP
   */
  private static void unparseDate(
      final StringWriter writer, final ZonedDateTime datetime, final boolean withTimeZone) {
    int nano = datetime.getNano();
    // The date literals are in the system time zone, another time zone is written
    boolean isSystemZone = !withTimeZone && datetime.getZone().equals(ZoneId.systemDefault());
    if (isSystemZone
        && nano == 0
        && datetime.getHour() == 0
        && datetime.getMinute() == 0
        && datetime.getSecond() == 0) {
      writer.append("DATE '");
      writer.append(DateTimeFormat.of("YYYY-MM-DD").format(datetime));
    } else {
      writer.append("TIMESTAMP '");
      writer.append(DateTimeFormat.of(timestampPattern(nano)).format(datetime));
      if (!isSystemZone) {
        if (datetime.getZone() instanceof ZoneOffset offset) {
          writer.append(' ');
          // The offset of UTC is written +00:00 and not Z
          writer.append(offset.getTotalSeconds() == 0 ? "+00:00" : offset.toString());
        } else {
          writer.append("' AT TIME ZONE '");
          writer.append(datetime.getZone().getId());
        }
      }
    }
    writer.append('\'');
  }

  /** Returns the pattern of a timestamp, with the fewest fractional digits for its nanoseconds. */
  private static String timestampPattern(int nano) {
    if (nano == 0) return "YYYY-MM-DD HH24:MI:SS";
    if (nano % 1000000 == 0) return "YYYY-MM-DD HH24:MI:SS.FF3";
    if (nano % 1000 == 0) return "YYYY-MM-DD HH24:MI:SS.FF6";
    return "YYYY-MM-DD HH24:MI:SS.FF9";
  }

  /** Unparses an interval literal, with its qualifier if it has one. */
  private static void unparseInterval(final StringWriter writer, final Interval interval) {
    IntervalQualifier qualifier = IntervalQualifier.of(interval);

    if (qualifier == null) {
      writer.append("INTERVAL '");
      writer.append(interval.toString());
      writer.append("'");
    } else if (qualifier.getStartUnit() == qualifier.getEndUnit()) {
      writer.append("INTERVAL ");
      writer.append(interval.toString(qualifier));
      writer.append(' ');
      writer.append(qualifier.toString());
    } else {
      writer.append("INTERVAL '");
      writer.append(interval.toString(qualifier));
      writer.append("' ");
      writer.append(qualifier.toString());
    }
  }

  /**
   * Validate a literal.
   *
   * @param context The context against which the expression will be validated.
   */
  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {
    // Nothing to validate
  }

  @Override
  public <E> E accept(final IExpressionVisitor<E> visitor) {
    return visitor.visitLiteral(this);
  }

  @Override
  public boolean isConstant() {
    return true;
  }
}
