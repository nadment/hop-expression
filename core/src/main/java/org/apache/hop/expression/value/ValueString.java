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
package org.apache.hop.expression.value;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.time.Instant;
import java.util.Locale;
import java.util.Objects;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Value;
import org.apache.hop.expression.util.DateFormat;
import org.apache.hop.i18n.BaseMessages;

public class ValueString extends Value {
  /** The string data. */
  private final String value;

  public ValueString(String value) {
    this.value = Objects.requireNonNull(value);
  }

  @Override
  public DataType getType() {
    return DataType.STRING;
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
    return other instanceof ValueString && value.equals(((ValueString) other).value);
  }

  @Override
  public int compare(Value v) {
    return value.compareTo(v.toString());
  }

  @Override
  public Value convertTo(final IExpressionContext context, final DataType targetType,
      String format) {

    if (targetType == DataType.DATE) {
      try {
        Instant result = DateFormat.parse(value, format, Locale.ENGLISH);
        return new ValueDate(result);
      } catch (RuntimeException | ParseException e) {
        throw new ExpressionException(BaseMessages.getString(PKG, "Expression.InvalidDate", value));
      }
    }

    throw createUnsupportedConversionError(targetType);
  }

  @Override
  public boolean toBoolean() {
    switch (value.length()) {
      case 1:
        if (value.equals("1") || value.equalsIgnoreCase("t") || value.equalsIgnoreCase("y")) {
          return true;
        }
        if (value.equals("0") || value.equalsIgnoreCase("f") || value.equalsIgnoreCase("n")) {
          return false;
        }
        break;
      case 2:
        if (value.equalsIgnoreCase("on")) {
          return true;
        }

        if (value.equalsIgnoreCase("no")) {
          return false;
        }
        break;
      case 3:
        if (value.equalsIgnoreCase("yes")) {
          return true;
        }
        if (value.equalsIgnoreCase("off")) {
          return false;
        }
        break;
      case 4:
        if (value.equalsIgnoreCase("true")) {
          return true;
        }
        break;
      case 5:
        if (value.equalsIgnoreCase("false")) {
          return false;
        }
        break;
      default:
       break;
    }
    throw new ExpressionException(
        BaseMessages.getString(PKG, "Expression.InvalidBoolean", value));
  }

  @Override
  public Value eval(IExpressionContext context) throws ExpressionException {
    return this;
  }

  public void write(StringWriter writer, int leftPrec, int rightPrec) {
    writer.append('\'');
    writer.append(value);
    writer.append('\'');
  }

  @Override
  public String toString() {
    return value;
  }

  @Override
  public byte[] toBinary() {
    return value.getBytes(StandardCharsets.UTF_8);
  }

  @Override
  public long toInteger() {
    try {

      if (value.indexOf('.') < 0)
        return Long.parseLong(value);

      return (long) Double.parseDouble(value);
    } catch (NumberFormatException e) {
      throw new ExpressionException(BaseMessages.getString(PKG, "Expression.InvalidNumber", value));
    }
  }

  @Override
  public double toNumber() {
    try {
      return Double.parseDouble(value);
    } catch (NumberFormatException e) {
      throw new ExpressionException(BaseMessages.getString(PKG, "Expression.InvalidNumber", value));
    }
  }

  @Override
  public BigDecimal toBigNumber() {
    try {
      return new BigDecimal(value.trim());
    } catch (NumberFormatException e) {
      throw new ExpressionException(BaseMessages.getString(PKG, "Expression.InvalidNumber", value));
    }
  }
}
