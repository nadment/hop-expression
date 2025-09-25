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

import java.math.BigDecimal;
import java.time.ZonedDateTime;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.util.IntegerConversion;

public final class IntegerType extends Type {
  /** Default INTEGER type with maximum precision. */
  public static final IntegerType INTEGER =
      new IntegerType(TypeName.INTEGER.getMaxPrecision(), true);

  /** Default INTEGER NOT NULL type with maximum precision. */
  public static final IntegerType INTEGER_NOT_NULL =
      new IntegerType(TypeName.INTEGER.getMaxPrecision(), false);

  IntegerType(int precision, boolean nullable) {
    super(precision, 0, nullable);
    this.signature = generateSignature();
    this.checkPrecisionAndScale();
  }

  public static IntegerType from(final Long value) {
    return of(numberOfDigit(value), false);
  }

  public static IntegerType from(final Integer value) {
    return of(numberOfDigit(value), false);
  }

  public static IntegerType of(int precision) {
    return of(precision, true);
  }

  public static IntegerType of(int precision, boolean nullable) {
    if (precision == PRECISION_NOT_SPECIFIED) precision = TypeName.INTEGER.getMaxPrecision();

    if (precision == TypeName.INTEGER.getMaxPrecision() && nullable) return IntegerType.INTEGER;

    return new IntegerType(precision, nullable);
  }

  private static int numberOfDigit(int number) {
    if (number < 100000) {
      if (number < 100) {
        if (number < 10) {
          return 1;
        } else {
          return 2;
        }
      } else {
        if (number < 1000) {
          return 3;
        } else {
          if (number < 10000) {
            return 4;
          } else {
            return 5;
          }
        }
      }
    } else {
      if (number < 10000000) {
        if (number < 1000000) {
          return 6;
        } else {
          return 7;
        }
      } else {
        if (number < 100000000) {
          return 8;
        } else {
          if (number < 1000000000) {
            return 9;
          } else {
            return 10;
          }
        }
      }
    }
  }

  private static int numberOfDigit(long number) {
    int count = 0;
    while (number != 0) {
      number = number / 10;
      ++count;
    }
    return count;
  }

  @Override
  public Type withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new IntegerType(precision, nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.INTEGER;
  }

  @Override
  public TypeComparability getComparability() {
    return TypeComparability.ALL;
  }

  @Override
  public <T> T convert(final Object value, final Class<T> clazz) throws ExpressionException {

    if (value == null) {
      return null;
    }
    if (clazz.isInstance(value)) {
      return clazz.cast(value);
    }
    if (clazz == Boolean.class) {
      return clazz.cast(((Long) value) != 0);
    }
    if (clazz == BigDecimal.class) {
      return clazz.cast(BigDecimal.valueOf((Long) value));
    }
    if (clazz == String.class) {
      return clazz.cast(String.valueOf(value));
    }

    return super.convert(value, clazz);
  }

  @Override
  public Long cast(final Object value) throws ExpressionException {
    return cast(value, null);
  }

  @Override
  public Long cast(final Object value, String pattern) throws ExpressionException {

    if (value == null) {
      return null;
    }
    if (value instanceof Long integer) {
      return integer;
    }
    if (value instanceof BigDecimal number) {
      return IntegerConversion.convert(number);
    }
    if (value instanceof Boolean bool) {
      return (bool) ? 1L : 0L;
    }
    if (value instanceof String str) {
      return IntegerConversion.convert(str);
    }
//    if (value instanceof byte[] bytes) {
//      return IntegerConversion.convert(bytes);
//    }
    if (value instanceof ZonedDateTime datetime) {
      return IntegerConversion.convert(datetime);
    }

    throw new ExpressionException(
        ErrorCode.UNSUPPORTED_CONVERSION, value, TypeName.fromValue(value), this);
  }

  @Override
  public boolean compareEqual(Object left, Object right) {
    if (left instanceof Long l && right instanceof Long r) {
      return l.equals(r);
    }
    return super.compareEqual(left, right);
  }

  @Override
  public int compare(Object left, Object right) {
    if (left instanceof Long l && right instanceof Long r) {
      return l.compareTo(r);
    }
    return super.compare(left, right);
  }
}
