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

import org.apache.hop.expression.util.DateTimeFormat;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.time.ZonedDateTime;

/**
 * Expression representing a literal value.
 */
public class Literal implements IExpression {
  public static final Literal NULL = new Literal(null);
  public static final Literal TRUE = new Literal(Boolean.TRUE);
  public static final Literal FALSE = new Literal(Boolean.FALSE);
  public static final Literal ZERO = new Literal(0L);
  public static final Literal ONE = new Literal(1L);
  public static final Literal PI = new Literal(Math.PI);

  public static Literal of(Object value) {
    if (value == null)
      return NULL;

    if (value instanceof Boolean) {
      return ((boolean) value) ? TRUE : FALSE;
    }

    if (value instanceof BigDecimal) {
      BigDecimal number = (BigDecimal) value;
      if (number.equals(BigDecimal.ZERO))
        return ZERO;
      if (number.equals(BigDecimal.ONE))
        return ONE;
      return new Literal(number);
    }

    if (value instanceof Double) {
      Double number = (Double) value;
      if (number == 0D)
        return ZERO;
      if (number == 1D)
        return ONE;
      return new Literal(number);
    }

    if (value instanceof Long) {
      Long number = (Long) value;
      if (number == 0L)
        return ZERO;
      if (number == 1L)
        return ONE;
      return new Literal(number);
    }

    if (value instanceof Integer) {
      Integer number = (Integer) value;
      if (number == 0)
        return ZERO;
      if (number == 1)
        return ONE;
      return new Literal(number.longValue());
    }

    if (value instanceof String || value instanceof ZonedDateTime || value instanceof byte[] || value instanceof DatePart || value instanceof DataType) {
      return new Literal(value);
    }

    throw ExpressionException.create("Expression.InvalidLiteral", value);
  }

  private Object value;

  private Literal(Object value) {
    this.value = value;
  }

  @Override
  public int getCost() {
    return 1;
  }

  @Override
  public Object eval(IExpressionContext context) throws ExpressionException {
    return value;
  }

  @Override
  public Kind getKind() {
    return Kind.LITERAL;
  }

  @Override
  public int hashCode() {
    if (value == null)
      return 0;

    return value.hashCode();
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
    write(writer);
    return writer.toString();
  }

  @Override
  public void write(StringWriter writer) {
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
    } 
    else if (value instanceof Boolean) {
      writer.append(((boolean) value) ? "TRUE" : "FALSE");
    }    
    else if (value instanceof byte[]) {
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
      writer.append(Operator.coerceToString(value));
    }
  }

  private static char byteToHex(int digit) {
    if (digit < 10) {
      return (char) ('0' + digit);
    }
    return (char) ('A' - 10 + digit);
  }
}
