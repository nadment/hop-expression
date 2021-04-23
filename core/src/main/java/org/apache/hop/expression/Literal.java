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

import java.io.StringWriter;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

/**
 * Expression representing a literal value.
 */
public class Literal implements IExpression {
  private static final DateTimeFormatter TIMESTAMP_FORMAT =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.nnn").withLocale(Locale.ROOT)
          .withZone(ZoneId.systemDefault());
  private static final DateTimeFormatter DATE_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      .withLocale(Locale.ROOT).withZone(ZoneId.systemDefault());

  public static final Literal UNKNOWN = new Literal(null);
  public static final Literal TRUE = new Literal(Boolean.TRUE);
  public static final Literal FALSE = new Literal(Boolean.FALSE);
  public static final Literal ZERO = new Literal(0L);
  public static final Literal ONE = new Literal(1L);
  public static final Literal PI = new Literal(Math.PI);

  public static Literal of(Object value) {
    if (value == null)
      return UNKNOWN;

    if (value instanceof Boolean) {
      return ((boolean) value) ? TRUE : FALSE;
    }

    if (value instanceof String) {     
      return new Literal((String) value);
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
    
    if (value instanceof Instant) {      
      return new Literal((Instant) value);
    }
    
    throw new IllegalArgumentException("Invalid literal: "+value);
  }

  private Object value;

  public Literal(Object value) {
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
      if (value == null ) {
        return (o.value == null);
      }
      return value.equals(o.value);
    }
    return false;
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    write(writer,0,0);    
    return writer.toString();
  }
  
  @Override
  public void write(StringWriter writer, int leftPrec, int rightPrec) {
    if (value == null) {
      writer.append("NULL");
    } else if (value instanceof String) {
      writer.append('\'');
      writer.append((String) value);
      writer.append('\'');
    } else if (value instanceof Instant) {
      writer.append("DATE '");
      Instant instant = (Instant) value;
      if (instant.getNano() > 0)
        writer.append(TIMESTAMP_FORMAT.format(instant));
      else
        writer.append(DATE_FORMAT.format(instant));
      writer.append('\'');
    } else {
      writer.append(Operator.coerceToString(value));
    }
  }
}
