/*
' * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
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

import org.apache.hop.expression.type.DataTypeName;
import org.apache.hop.expression.util.Coerse;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.i18n.BaseMessages;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.Objects;

/**
 * Constant value in a expression.
 */
public class Literal implements IExpression {

  private static final Class<?> PKG = IExpression.class; // for i18n purposes

  public static final Literal NULL = new Literal(null, DataTypeName.UNKNOWN);
  public static final Literal TRUE = new Literal(Boolean.TRUE, DataTypeName.BOOLEAN);
  public static final Literal FALSE = new Literal(Boolean.FALSE, DataTypeName.BOOLEAN);
  public static final Literal ZERO = new Literal(0L, DataTypeName.INTEGER);
  public static final Literal ONE = new Literal(1L, DataTypeName.INTEGER);

  public static Literal of(Object value) {
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
      return new Literal(number, DataTypeName.BIGNUMBER);
    }

    if (value instanceof Double) {
      Double number = (Double) value;
      if (number == 0D)
        return ZERO;
      if (number == 1D)
        return ONE;
      return new Literal(number, DataTypeName.NUMBER);
    }

    if (value instanceof Long) {
      Long number = (Long) value;
      if (number == 0L)
        return ZERO;
      if (number == 1L)
        return ONE;
      return new Literal(number, DataTypeName.INTEGER);
    }

    if (value instanceof Integer) {
      Integer number = (Integer) value;
      if (number == 0)
        return ZERO;
      if (number == 1)
        return ONE;
      return new Literal(number.longValue(), DataTypeName.INTEGER);
    }

    if (value instanceof String ) {
      return new Literal(value, DataTypeName.STRING);
    }

    if (value instanceof  byte[] ) {
      return new Literal(value, DataTypeName.BINARY);
    }
    if (value instanceof  ZonedDateTime ) {
      return new Literal(value, DataTypeName.DATE);
    }
        
    // Special case for optimization
    if (value instanceof DatePart || value instanceof DataTypeName || value instanceof NumberFormat || value instanceof DateTimeFormat) {
      return new Literal(value, DataTypeName.UNKNOWN);
    }

    throw new IllegalArgumentException(
        BaseMessages.getString(PKG, "Expression.UnsupportedLiteralType", value.getClass(), value));
  }

  /**
   * The data type of this literal, as reported by {@link #getDataType}.
   */
  private final DataTypeName type;

  private Object value;

  private Literal(final Object value, final DataTypeName type) {
    this.value = value;
    this.type = type;
  }

  @Override
  public Object getValue(final IExpressionContext context) throws ExpressionException {
    return value;
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
  public DataTypeName getType() {
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
    if (other == null)
      return false;

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
  public String toString() {
    StringWriter writer = new StringWriter();
    unparse(writer);
    return writer.toString();
  }

  @Override
  public void unparse(StringWriter writer) {
    if (value == null) {
      writer.append("NULL");
    } else if (value instanceof String) {
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
    } else if (value instanceof Boolean) {
      writer.append(((boolean) value) ? "TRUE" : "FALSE");
    } else if (value instanceof byte[]) {
      writer.append("0x");
      for (byte b : (byte[]) value) {
        writer.append(byteToHex((b >> 4) & 0xF));
        writer.append(byteToHex(b & 0xF));
      }
    } else if (value instanceof ZonedDateTime) {
      ZonedDateTime datetime = (ZonedDateTime) value;
      if (datetime.getNano() > 0) {
        writer.append("TIMESTAMP '");
        writer.append(DateTimeFormat.of("YYYY-MM-DD HH24:MI:SS.FF").format(datetime));
      } else {
        writer.append("DATE '");
        writer.append(DateTimeFormat.of("YYYY-MM-DD").format(datetime));
      }
      writer.append('\'');
    } else {
      writer.append(Coerse.toString(value));
    }
  }

  private char byteToHex(int digit) {
    if (digit < 10) {
      return (char) ('0' + digit);
    }
    return (char) ('A' - 10 + digit);
  }

  @Override
  public <E> E accept(IExpressionContext context, IExpressionVisitor<E> visitor) {
    return visitor.apply(context, this);
  }
}
