/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression.value;

import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.i18n.BaseMessages;
import java.io.StringWriter;
import java.lang.ref.SoftReference;
import java.math.BigDecimal;
import java.util.Objects;

public class ValueBigNumber extends Value {
  
  /** The value 'zero'. */
  public static final ValueBigNumber ZERO = new ValueBigNumber(BigDecimal.ZERO);

  /** The value 'one'. */
  public static final ValueBigNumber ONE = new ValueBigNumber(BigDecimal.ONE);

  /** The default precision for a decimal value. */
  static final int DEFAULT_PRECISION = 20;

  /** The default scale for a decimal value. */
  static final int DEFAULT_SCALE = 18;
 
  /**
   * The maximum allowed precision of numeric data types.
   */
  public static final int MAX_NUMERIC_PRECISION = 38;
  
  private static final int OBJECT_CACHE_SIZE = 1024;

  private static SoftReference<ValueBigNumber[]> softCache;
  
  /** Returns an big number Value that equals to {@code value}. */
  public static Value of(BigDecimal value) {
    if (value == null)
      return Value.NULL;
    if (BigDecimal.ZERO.equals(value))
      return ValueBigNumber.ZERO;
    if (BigDecimal.ONE.equals(value))
      return ValueBigNumber.ONE;

    return cache(new ValueBigNumber(value));
  }

  public static Value of(long value) {
    return of(BigDecimal.valueOf(value));
  }

  public static Value of(double value) {
    return of(BigDecimal.valueOf(value));
  }
  
  /**
   * Check if a value is in the cache that is equal to this value. If yes, this value should be used
   * to save memory. If the value is not in the cache yet, it is added.
   *
   * @param v the value to look for
   * @return the value in the cache or the value passed
   */
  static ValueBigNumber cache(ValueBigNumber v) {
    int hash = v.hashCode();
    ValueBigNumber[] cache;
    if (softCache == null || (cache = softCache.get()) == null) {
      cache = new ValueBigNumber[OBJECT_CACHE_SIZE];
      softCache = new SoftReference<>(cache);
    }
    int index = hash & (OBJECT_CACHE_SIZE - 1); //
    ValueBigNumber cached = cache[index];
    if (cached != null  && v.equals(cached)) {
      return cached;
    }
    cache[index] = v;

    return v;
  }

  /**
   * Clear the value cache. Used for testing.
   */
  public static void clearCache() {
    softCache = null;
  }
  
  private final BigDecimal value;

  protected ValueBigNumber(BigDecimal value) {
    this.value = Objects.requireNonNull(value);
    
    int length = value.precision();
    if (length > MAX_NUMERIC_PRECISION) {
        throw new IllegalArgumentException(BaseMessages.getString(PKG, "Expression.ValueTooLong", value.toString(), length));
    }
  }

  @Override
  public DataType getType() {
    return DataType.BIGNUMBER;
  }

  @Override
  public Object getObject() {
    return value;
  }

  @Override
  public Value convertTo(
      final IExpressionContext context, final DataType type, String format) {

    if (type == DataType.STRING) {
      String result = NumberFormat.format(value, format);
      return ValueString.of(result);
    }

    throw createUnsupportedConversionError(type);
  }

  @Override
  public BigDecimal toBigNumber() {
    return value;
  }

  @Override
  public int signum() {
    return value.signum();
  }

  @Override
  public Value negate() {
    return new ValueBigNumber(value.negate());
  }

  @Override
  public String toString() {
    final String s = value.toString();

    if (s.startsWith("0.")) {
      // we want ".1" not "0.1"
      return s.substring(1);
    } else if (s.startsWith("-0.")) {
      // we want "-.1" not "-0.1"
      return "-" + s.substring(2);
    } else {
      return s;
    }
  }

  @Override
  public long toInteger() {
    return value.setScale(0, BigDecimal.ROUND_HALF_UP).longValue();
  }

  @Override
  public double toNumber() {
    return value.doubleValue();
  }

  public void write(StringWriter writer, int leftPrec, int rightPrec) {
    writer.append(this.toString());
  }

  @Override
  public int hashCode() {
    return value.hashCode();
  }

  @Override
  public boolean equals(Object other) {
    // Two BigDecimal objects are considered equal only if they are equal in
    // value and scale (thus 2.0 is not equal to 2.00 when using equals
    // however -0.0 and 0.0 are). Can not use compareTo because 2.0 and 2.00
    // have different hash codes
    return other instanceof ValueBigNumber && value.equals(((ValueBigNumber) other).value);
  }

  @Override
  public int compare(Value v) {
    return value.stripTrailingZeros().compareTo(v.toBigNumber().stripTrailingZeros());
  }

  @Override
  public Value eval(IExpressionContext context) throws ExpressionException {
    return this;
  }

  @Override
  public boolean toBoolean() {
    return Boolean.valueOf(value.signum() != 0);
  }

  @Override
  public Value add(Value v) {
    if (v.isNull())
      return NULL;
    return ValueBigNumber.of(value.add(v.toBigNumber()));
  }

  @Override
  public Value subtract(Value v) {
    if (v.isNull())
      return NULL;
    return ValueBigNumber.of(value.subtract(v.toBigNumber()));
  }

  @Override
  public Value multiply(Value v) {
    if (v.isNull())
      return NULL;
    return ValueBigNumber.of(value.multiply(v.toBigNumber()));
  }

  @Override
  public Value divide(Value v) {
    if (v.isNull())
      return NULL;
    // prevent a division by zero ..
    if (v.signum() == 0)
      throw createDivisionByZeroError();    
    return ValueBigNumber.of(value.divide(v.toBigNumber(), DEFAULT_SCALE, BigDecimal.ROUND_HALF_UP));
  }

  @Override
  public Value remainder(Value v) {
    if (v.isNull())
      return NULL;
    // prevent a division by zero ..
    if (v.signum() == 0)
      throw createDivisionByZeroError();
    return ValueBigNumber.of(value.remainder(v.toBigNumber()));
  }

  @Override
  public Value power(Value v) {
    if (v.isNull())
      return NULL;
    if (v.signum() == 0)
      return ONE;
    if (v.signum() < 0)
      throw new ArithmeticException("Cannot power negative " + v);
    
    // BigDecimal doesn't support for fractional, so we use double
    double result = FastMath.pow(value.doubleValue(), v.toNumber());
    return ValueNumber.of(result);
  }
}
