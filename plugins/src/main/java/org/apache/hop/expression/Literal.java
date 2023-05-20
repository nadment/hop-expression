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

import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.DataName;
import org.apache.hop.expression.type.DataType;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Objects;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Constant value in a expression.
 */
public final class Literal implements IExpression {

  private static final ObjectMapper MAPPER = new ObjectMapper();

  public static final Literal NULL = new Literal(null, DataType.UNKNOWN);
  public static final Literal TRUE = new Literal(Boolean.TRUE, DataType.BOOLEAN);
  public static final Literal FALSE = new Literal(Boolean.FALSE, DataType.BOOLEAN);
  public static final Literal ZERO = new Literal(0L, DataType.INTEGER);
  public static final Literal ONE = new Literal(1L, DataType.INTEGER);
  public static final Literal PI = new Literal(Math.PI, DataType.NUMBER);

  public static Literal of(final Object value) {
    if (value == null)
      return NULL;

    if (value instanceof Boolean) {
      return ((boolean) value) ? TRUE : FALSE;
    }

    if (value instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) value;
      if (BigDecimal.ZERO.compareTo(number) == 0)
        return ZERO;
      if (BigDecimal.ONE.compareTo(number) == 0)
        return ONE;
      return new Literal(number, DataType.BIGNUMBER);
    }

    if (value instanceof Double) {
      Double number = (Double) value;
      if (number == 0D)
        return ZERO;
      if (number == 1D)
        return ONE;
      return new Literal(number, DataType.NUMBER);
    }

    if (value instanceof Long) {
      Long number = (Long) value;
      if (number == 0L)
        return ZERO;
      if (number == 1L)
        return ONE;
      return new Literal(number, DataType.INTEGER);
    }

    if (value instanceof Integer) {
      Integer number = (Integer) value;
      if (number == 0)
        return ZERO;
      if (number == 1)
        return ONE;
      return new Literal(number.longValue(), DataType.INTEGER);
    }

    if (value instanceof String) {
      return new Literal(value, DataType.STRING);
    }

    if (value instanceof byte[]) {
      return new Literal(value, DataType.BINARY);
    }

    if (value instanceof ZonedDateTime) {
      return new Literal(value, DataType.DATE);
    }

    if (value instanceof JsonNode) {
      return new Literal(value, DataType.JSON);
    }

    if (value instanceof TimeUnit) {
      return new Literal(value, DataType.TIMEUNIT);
    }
    
    // Special case for optimization
    return new Literal(value, DataType.UNKNOWN);
  }

  /**
   * The data type of this literal.
   */
  private final DataType type;

  /**
   * The value of this literal.
   */
  private final Object value;
  
  /**
   * For internal use only
   */
  public Literal(final Object value, final DataType type) {
    this.value = value;
    this.type = type;
  }

  @Override
  public Object getValue(final IExpressionContext context) throws ExpressionException {
    return value;
  }

  @Override
  public <T> T getValue(final IExpressionContext context, final Class<T> clazz)
      throws ExpressionException {

    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }

    if (value == null)
      return null;

    switch (type.getName()) {

      case BOOLEAN:
        if (clazz == String.class) {
          return clazz.cast(String.valueOf(value));
        }
        if (clazz == Long.class) {
          return clazz.cast(((boolean) value) ? 1L : 0L);
        }
        if (clazz == Double.class) {
          return clazz.cast(((boolean) value) ? 1D : 0D);
        }
        if (clazz == BigDecimal.class) {
          return clazz.cast(((boolean) value) ? BigDecimal.ONE : BigDecimal.ZERO);
        }
        break;

      case STRING:
        if (clazz == Boolean.class) {
          return clazz.cast(Converter.parseBoolean((String) value));
        }
        if (clazz == Long.class) {
          return clazz.cast(Converter.parseInteger((String) value));
        }
        if (clazz == Double.class) {
          return clazz.cast(Converter.parseNumber((String) value));
        }
        if (clazz == BigDecimal.class) {
          return clazz.cast(Converter.parseBigNumber((String) value));
        }
        if (clazz == byte[].class) {
          return clazz.cast(((String) value).getBytes(StandardCharsets.UTF_8));
        }
        if (clazz == JsonNode.class) {
          return clazz.cast(Converter.parseJson((String) value));
        }
        break;

      case INTEGER:
        if (clazz == Boolean.class) {
          return clazz.cast(((Long) value) != 0);
        }
        if (clazz == Double.class) {
          return clazz.cast(Double.valueOf((Long) value));
        }
        if (clazz == BigDecimal.class) {
          return clazz.cast(BigDecimal.valueOf((Long) value));
        }
        if (clazz == String.class) {
          return clazz.cast(String.valueOf(value));
        }
        break;

      case NUMBER:
        if (clazz == Boolean.class) {
          return clazz.cast(((Double) value) != 0);
        }
        if (clazz == Long.class) {
          return clazz.cast(((Double) value).longValue());
        }
        if (clazz == BigDecimal.class) {
          return clazz.cast(BigDecimal.valueOf((Double) value));
        }
        if (clazz == String.class) {
          return clazz.cast(String.valueOf(value));
        }
        break;

      case BIGNUMBER:
        if (clazz == Boolean.class) {
          return clazz.cast(((BigDecimal) value).unscaledValue() != BigInteger.ZERO);
        }
        if (clazz == Long.class) {
          return clazz.cast(((BigDecimal) value).longValue());
        }
        if (clazz == Double.class) {
          return clazz.cast(((BigDecimal) value).doubleValue());
        }
        if (clazz == String.class) {
          return clazz.cast(NumberFormat.of("TM").format((BigDecimal) value));
        }
        break;

      case BINARY:
        if (clazz == String.class) {
          return clazz.cast(new String((byte[]) value, StandardCharsets.UTF_8));
        }
        break;

      case JSON:
        if (clazz == String.class) {
          return clazz.cast(Converter.parseJson((String) value));
        }
        break;

      case DATE:
      case TIMEUNIT:
      case ANY:
      case UNKNOWN:
        break;
    }

    throw new ExpressionException(ExpressionError.UNSUPPORTED_COERCION, value,
        DataName.from(value), DataName.from(clazz));
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
  public DataType getType() {
    return type;
  }

  public Class<?> getJavaClass() {
    if (value == null)
      return Void.class;
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
  public void unparse(StringWriter writer) {
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
        writer.append(((boolean) value) ? "TRUE" : "FALSE");
        break;
      case BINARY:
        writer.append("0x");
        for (byte b : (byte[]) value) {
          writer.append(byteToHex((b >> 4) & 0xF));
          writer.append(byteToHex(b & 0xF));
        }
        break;
      case DATE: {
        //ZonedDateTime datetime = ((LocalDateTime) value).atZone(ZoneId.systemDefault());
        ZonedDateTime datetime = (ZonedDateTime) value;
        int nano = datetime.getNano();
        if (nano > 0) {
          writer.append("TIMESTAMP '");

          if (nano % 1000000 == 0)
            writer.append(DateTimeFormat.of("YYYY-MM-DD HH24:MI:SS.FF3").format(datetime));
          else if (nano % 1000 == 0)
            writer.append(DateTimeFormat.of("YYYY-MM-DD HH24:MI:SS.FF6").format(datetime));
          else
            writer.append(DateTimeFormat.of("YYYY-MM-DD HH24:MI:SS.FF9").format(datetime));
          if (datetime.getOffset().getTotalSeconds() != 0) {
            if (datetime.getZone() instanceof ZoneOffset) {
              writer.append(' ');
              writer.append(datetime.getZone().toString());

            } else {
              writer.append("' AT TIME ZONE '");
              writer.append(datetime.getZone().getId());
            }
          }
          
        } else if (datetime.getHour() == 0 && datetime.getMinute() == 0 && datetime.getSecond() == 0) {
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
      case JSON:
        try {
          writer.append("JSON '");
          writer.append(MAPPER.writeValueAsString((JsonNode) value));
          writer.append('\'');
        } catch (JsonProcessingException e) {
          throw new RuntimeException("Unable to unparse json object ", e);
        }
        break;
      case UNKNOWN:
        if (value == null) {
          writer.append("NULL");
        } else {
          writer.append(String.valueOf(value));
        }
        break;
      default:
        writer.append(Converter.coerceToString(value));
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
  public IExpression compile(final IExpressionContext context) throws ExpressionException {   
    return this;
  }
  
  @Override
  public <E> E accept(IExpressionContext context, IExpressionVisitor<E> visitor) {
    return visitor.apply(context, this);
  }

//  @Override
//  public boolean isAlwaysTrue() {
//    return Boolean.TRUE.equals(value);
//  }
//
//  @Override
//  public boolean isAlwaysFalse() {
//    return Boolean.FALSE.equals(value);
//  }

  @Override
  public boolean isConstant() {
    return true;
  }
}
