/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.hop.expression.type;

import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.net.InetAddress;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.util.BinaryConversion;
import org.apache.hop.expression.util.BooleanConversion;
import org.apache.hop.expression.util.DateTimeConversion;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.InetConversion;
import org.apache.hop.expression.util.IntegerConversion;
import org.apache.hop.expression.util.JsonConversion;
import org.apache.hop.expression.util.NumberConversion;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.expression.util.StringConversion;

public final class StringType extends Type {

    /** Default STRING type with maximum precision. */
    public static final StringType STRING = new StringType(TypeName.STRING.getMaxPrecision(), true);

    /** Default STRING NOT NULL type with maximum precision. */
    public static final StringType STRING_NOT_NULL =
            new StringType(TypeName.STRING.getMaxPrecision(), false);
    
  /* Package */ StringType(int precision, boolean nullable) {
    super(precision, 0, nullable);
    this.signature = generateSignature();
    this.checkPrecisionAndScale();
  }

  public static StringType of(int precision) {
    return of(precision, true);
  }

  public static StringType of(int precision, boolean nullable) {
    if (precision == PRECISION_NOT_SPECIFIED) precision = TypeName.STRING.getMaxPrecision();

    if (precision == TypeName.STRING.getMaxPrecision() && nullable) return StringType.STRING;

    return new StringType(precision, nullable);
  }

  public static StringType from(final String value) {
    int precision = value.length();
    // Empty string should return 1
    if (precision < 1) precision = 1;
    return StringType.of(precision, false);
  }

  public StringType withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new StringType(precision, nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.STRING;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }

  @Override
  public <T> T convert(Object value, Class<T> clazz) throws ExpressionException {
    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == Boolean.class) {
      return clazz.cast(BooleanConversion.convert((String) value));
    }
    if (clazz == Long.class) {
      return clazz.cast(IntegerConversion.convert((String) value));
    }
    if (clazz == BigDecimal.class) {
      return clazz.cast(NumberConversion.convert((String) value));
    }
    if (clazz == byte[].class) {
      return clazz.cast(BinaryConversion.convert((String) value));
    }
    if (clazz == ZonedDateTime.class) {
      return clazz.cast(DateTimeConversion.convert((String) value));
    }
    if (clazz == JsonNode.class) {
      return clazz.cast(JsonConversion.convert((String) value));
    }
    if (clazz == InetAddress.class) {
      return clazz.cast(InetConversion.convert((String) value));
    }
    return super.convert(value, clazz);
  }

  @Override
  public String cast(final Object value) throws ExpressionException {
    return cast(value, null);
  }

  /**
   * Convert a value to the specified type {@link StringType} with optional pattern and adjust to
   * precision.
   *
   * @param value the value to convert
   * @param pattern the optional pattern to use for conversion to string when value is date or
   *     numeric, or null if none
   * @return the converted value
   */
  @Override
  public String cast(final Object value, String pattern) throws ExpressionException {

    if (value == null) {
      return null;
    }

    String result = null;

    if (value instanceof String str) {
      result = str;
    } else if (value instanceof Boolean bool) {
      result = StringConversion.convert(bool);
    } else if (value instanceof Number) {
      if (pattern == null) {
        pattern = "TM";
      }
      BigDecimal number;
      if (value instanceof Long l) {
        number = BigDecimal.valueOf(l);
      } else {
        number = (BigDecimal) value;
      }
      result = NumberFormat.of(pattern).format(number);
    } else if (value instanceof ZonedDateTime datetime) {
      if (pattern == null) pattern = "YYYY-MM-DD";
      result = DateTimeFormat.of(pattern).format(datetime);
    } else if (value instanceof byte[] bytes) {
      result = new String(bytes, StandardCharsets.UTF_8);
    } else if (value instanceof JsonNode json) {
      return StringConversion.convert(json);
    } else if (value instanceof InetAddress inet) {
      return StringConversion.convert(inet);
    }

    if (result == null) {
      throw new ExpressionException(
          ErrorCode.CONVERSION_ERROR, TypeName.fromValue(value), this, value);
    }

    // Adjust length
    if (precision < result.length()) {
      result = result.substring(0, precision);
    }

    return result;
  }

  private boolean checkPrecision(final String result) {
    if (result == null) return true;

    return this.precision < 0 || this.precision >= result.length();
  }

  @Override
  public boolean compareEqual(Object left, Object right) {
    if (left instanceof String l && right instanceof String r) {
      return l.compareTo(r) == 0;
    }
    return super.compareEqual(left, right);
  }

  @Override
  public int compare(Object left, Object right) {
    if (left instanceof String l && right instanceof String r) {
      return l.compareTo(r);
    }
    return super.compare(left, right);
  }
}
