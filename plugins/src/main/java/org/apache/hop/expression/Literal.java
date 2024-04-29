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
import org.apache.hop.expression.type.Interval;
import org.apache.hop.expression.type.IntervalQualifier;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;
import org.apache.hop.expression.util.DateTimeFormat;

/** Constant value in a expression. */
public final class Literal implements IExpression {

  /** Literal null value without known data type */
  public static final Literal NULL = new Literal(null, Types.UNKNOWN);

  /** Literal true value with boolean data type */
  public static final Literal TRUE = new Literal(Boolean.TRUE, Types.BOOLEAN_NOT_NULL);

  /** Literal false value with boolean data type */
  public static final Literal FALSE = new Literal(Boolean.FALSE, Types.BOOLEAN_NOT_NULL);

  /** Literal 0 value with integer data type */
  public static final Literal ZERO = new Literal(0L, IntegerType.of(1).withNullability(false));

  /** Literal 1 value with integer data type */
  public static final Literal ONE = new Literal(1L, IntegerType.of(1).withNullability(false));

  public static Literal of(final Object value) {
    if (value == null) return NULL;

    if (value instanceof Boolean) {
      return ((boolean) value) ? TRUE : FALSE;
    }

    if (value instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) value;
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

    if (value instanceof Long) {
      Long number = (Long) value;
      if (number == 0L) return ZERO;
      if (number == 1L) return ONE;
      return new Literal(number, IntegerType.from(number));
    }

    if (value instanceof Integer) {
      Integer number = (Integer) value;
      if (number == 0) return ZERO;
      if (number == 1) return ONE;
      Long longValue = number.longValue();
      return new Literal(longValue, IntegerType.from(longValue));
    }

    if (value instanceof String) {
      return new Literal(value, StringType.from((String) value));
    }

    if (value instanceof byte[]) {
      return new Literal(value, BinaryType.from((byte[]) value));
    }

    if (value instanceof ZonedDateTime) {
      return new Literal(value, Types.DATE_NOT_NULL);
    }

    if (value instanceof Interval) {
      return new Literal(value, Types.INTERVAL_NOT_NULL);
    }

    if (value instanceof JsonNode) {
      return new Literal(value, Types.JSON_NOT_NULL);
    }

    if (value instanceof InetAddress) {
      return new Literal(value, Types.INET_NOT_NULL);
    }

    // Special internal case TimeUnit, DataType
    return new Literal(value, Types.UNKNOWN);
  }

  /** The data type of this literal. */
  private final Type type;

  /** The value of this literal. */
  private final Object value;

  /**
   * Create a typed literal value
   *
   * @param type
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

  public Class<?> getJavaClass() {
    if (value == null) return Void.class;
    return value.getClass();
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
    if (other instanceof Literal) {
      Literal o = (Literal) other;
      if (value == null) {
        return (o.value == null);
      }
      return value.equals(o.value);
    }
    return false;
  }

  @Override
  public Literal asLiteral() {
    return this;
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    unparse(writer);
    return writer.toString();
  }

  @Override
  public void unparse(final StringWriter writer) {

    if (value == null) {
      writer.append("NULL");
    } else
      switch (type.getId()) {
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
          writer.append(StringType.convertToString((boolean) value));
          break;
        case INTEGER:
          writer.append(((Long) value).toString());
          break;
        case NUMBER:
          writer.append(StringType.convertToString((BigDecimal) value));
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
          writer.append(StringType.convertToString((JsonNode) value));
          writer.append('\'');
          break;
        case INET:
          writer.append("INET '");
          writer.append(StringType.convertToString((InetAddress) value));
          writer.append('\'');
          break;
        case UNKNOWN:
          writer.append(String.valueOf(value));
          break;
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
