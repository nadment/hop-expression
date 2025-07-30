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
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Objects;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.util.BaseFormat;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.StringConversion;

/** Constant value in a expression. */
public class Literal implements IExpression {

  /** Literal null value without known data type */
  public static final Literal NULL = new Literal(null, Types.UNKNOWN);

  /** Literal null with binary data type */
  public static final Literal NULL_BINARY = new Literal(null, Types.BINARY);

  /** Literal null with boolean data type */
  public static final Literal NULL_BOOLEAN = new Literal(null, Types.BOOLEAN);

  /** Literal null with string data type */
  public static final Literal NULL_STRING = new Literal(null, Types.STRING);

  /** Literal null with integer data type */
  public static final Literal NULL_INTEGER = new Literal(null, Types.INTEGER);

  /** Literal null with number data type */
  public static final Literal NULL_NUMBER = new Literal(null, Types.NUMBER);

  /** Literal null with date data type */
  public static final Literal NULL_DATE = new Literal(null, Types.DATE);

  /** Literal null with json data type */
  public static final Literal NULL_JSON = new Literal(null, Types.JSON);

  /** Literal null with inet data type */
  public static final Literal NULL_INET = new Literal(null, Types.INET);

  /** Literal null with interval data type */
  public static final Literal NULL_INTERVAL = new Literal(null, Types.INTERVAL);

  /** Literal true value with boolean data type */
  public static final Literal TRUE = new Literal(Boolean.TRUE, Types.BOOLEAN_NOT_NULL);

  /** Literal false value with boolean data type */
  public static final Literal FALSE = new Literal(Boolean.FALSE, Types.BOOLEAN_NOT_NULL);

  /** Literal 0 value with integer data type */
  public static final Literal ZERO = new Literal(0L, IntegerType.of(1, false));

  /** Literal 1 value with integer data type */
  public static final Literal ONE = new Literal(1L, IntegerType.of(1, false));

  public static Literal of(final Boolean value) {
    if (value == null) return NULL_BOOLEAN;
    return value ? TRUE : FALSE;
  }

  public static Literal of(final byte[] value) {
    if (value == null) return NULL_BINARY;
    return new Literal(value, BinaryType.from(value));
  }

  public static Literal of(final String value) {
    if (value == null) return NULL_STRING;
    return new Literal(value, StringType.from(value));
  }

  public static Literal of(final Integer value) {
    if (value == null) return NULL_INTEGER;
    if (value == 0) return ZERO;
    if (value == 1) return ONE;
    Long longValue = value.longValue();
    return new Literal(longValue, IntegerType.from(longValue));
  }

  public static Literal of(final Long value) {
    if (value == null) return NULL_INTEGER;
    if (value == 0L) return ZERO;
    if (value == 1L) return ONE;
    return new Literal(value, IntegerType.from(value));
  }

  public static Literal of(final BigDecimal number) {
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

  public static Literal of(final Interval value) {
    if (value == null) return NULL_INTERVAL;
    return new Literal(value, Types.INTERVAL_NOT_NULL);
  }

  public static Literal of(final ZonedDateTime value) {
    if (value == null) return NULL_DATE;
    return new Literal(value, Types.DATE_NOT_NULL);
  }

  public static Literal of(final JsonNode value) {
    if (value == null) return NULL_JSON;
    return new Literal(value, Types.JSON_NOT_NULL);
  }

  public static Literal of(final InetAddress value) {
    if (value == null) return NULL_INET;
    return new Literal(value, Types.INET_NOT_NULL);
  }

  public static Literal of(final Type value) {
    if (value == null) return NULL;
    return new Literal(value, Types.TIMEUNIT);
  }

  public static Literal of(final TimeUnit value) {
    if (value == null) return NULL;
    return new Literal(value, Types.TIMEUNIT);
  }

  public static Literal of(final BaseFormat value) {
    if (value == null) return NULL;
    return new Literal(value, Types.UNKNOWN);
  }

  /** The data type of this literal. */
  private final Type type;

  /** The value of this literal. */
  private final Object value;

  /**
   * Create a typed literal value
   *
   * @param value the constant value
   * @param type the type of the value
   */
  public Literal(final Object value, final Type type) {
    this.value = value;
    this.type = type;
  }

  @Override
  public Object getValue() {
    return value;
  }

  @Override
  public <T> T getValue(final Class<T> clazz) {
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
    return Objects.hash(value, type);
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
    } else
      switch (type.getName()) {
        case STRING:
          writer.append('\'');
          String str = (String) value;
          for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);
            writer.append(ch);
            if (ch == '\'') {
              writer.append(ch);
            }
          }
          writer.append('\'');
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
          writer.append("BINARY '");
          for (byte b : (byte[]) value) {
            writer.append(byteToHex((b >> 4) & 0xF));
            writer.append(byteToHex(b & 0xF));
          }
          writer.append('\'');
          break;
        case DATE:
          {
            ZonedDateTime datetime = (ZonedDateTime) value;
            int nano = datetime.getNano();
            if (nano > 0) {
              writer.append("TIMESTAMP '");

              if (nano % 1000000 == 0)
                writer.append(DateTimeFormat.of("YYYY-MM-DD HH24:MI:SS.FF3").format(datetime));
              else if (nano % 1000 == 0)
                writer.append(DateTimeFormat.of("YYYY-MM-DD HH24:MI:SS.FF6").format(datetime));
              else writer.append(DateTimeFormat.of("YYYY-MM-DD HH24:MI:SS.FF9").format(datetime));
              if (datetime.getOffset().getTotalSeconds() != 0) {
                if (datetime.getZone() instanceof ZoneOffset) {
                  writer.append(' ');
                  writer.append(datetime.getZone().toString());

                } else {
                  writer.append("' AT TIME ZONE '");
                  writer.append(datetime.getZone().getId());
                }
              }

            } else if (datetime.getHour() == 0
                && datetime.getMinute() == 0
                && datetime.getSecond() == 0) {
              writer.append("DATE '");
              writer.append(DateTimeFormat.of("YYYY-MM-DD").format(datetime));
            } else {
              writer.append("TIMESTAMP '");
              writer.append(DateTimeFormat.of("YYYY-MM-DD HH24:MI:SS").format(datetime));
              if (datetime.getOffset().getTotalSeconds() != 0) {
                if (datetime.getZone() instanceof ZoneOffset) {
                  writer.append(' ');
                  writer.append(datetime.getZone().toString());

                } else {
                  writer.append("' AT TIME ZONE '");
                  writer.append(datetime.getZone().getId());
                }
              }
            }
            writer.append('\'');
            break;
          }
        case INTERVAL:
          {
            Interval interval = (Interval) value;
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
            break;
          }
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

  private char byteToHex(int digit) {
    if (digit < 10) {
      return (char) ('0' + digit);
    }
    return (char) ('A' - 10 + digit);
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
