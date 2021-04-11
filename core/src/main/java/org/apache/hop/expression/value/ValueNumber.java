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

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.util.NumberFormat;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.util.Objects;

public class ValueNumber extends Value {

  private final double value;

  /** Numeric literal value of 0. */
  public static final Value ZERO = new ValueNumber(0.0);

  /** Numeric literal value of 1. */
  public static final Value ONE = new ValueNumber(1.0);

  /** Numeric literal value of PI. */
  public static final Value PI = new ValueNumber(Math.PI);



  public static Value of(Double value) {
    if (value == null || value.isNaN())
      return NULL;
    if (value == 0.0)
      return ZERO;
    if (value == 1.0)
      return ONE;

    return new ValueNumber(value);
  }
  
  protected ValueNumber(double value) {
    this.value = value;
  }

  protected ValueNumber(Double value) {
    this.value = Objects.requireNonNull(value);
  }

  @Override
  public DataType getType() {
    return DataType.NUMBER;
  }

  @Override
  public Object getObject() {
    return value;
  }

  @Override
  public int hashCode() {
    /*
     * NaNs are normalized in Value.of() method, so it's safe to use
     * doubleToRawLongBits() instead of doubleToLongBits() here.
     */
    long hash = Double.doubleToRawLongBits(value);
    return (int) (hash ^ (hash >>> 32));
  }

  @Override
  public boolean equals(Object other) {
    return other instanceof ValueNumber && value == ((ValueNumber) other).value;
  }

  @Override
  public int compare(Value v) {
    return Double.compare(value, v.toNumber());
  }

  @Override
  public Value eval(IExpressionContext context) throws ExpressionException {
    return this;
  }
  
  @Override
  public void write(StringWriter writer, int leftPrec, int rightPrec) {
    writer.append(this.toString());
  }

  @Override
  public int signum() {
    if ( value== 0 ) return 0;    
    return (value < 0) ? -1 : 1;
  }

  @Override
  public Value negate() throws ExpressionException {
    if (value == Double.MIN_VALUE) {
      throw createOverflowError();
    }
    return new ValueNumber(-value);
  }

  @Override
  public boolean toBoolean() {
    return value != 0.d;
  }

  @Override
  public double toNumber() {
    return value;
  }

  @Override
  public BigDecimal toBigNumber() {
    return BigDecimal.valueOf(value);
  }

  @Override
  public long toInteger() {
    return (long) value;
  }

  @Override
  public String toString() {
    return Double.toString(value);
  }

  @Override
  public Value convertTo(
      final IExpressionContext context, final DataType targetType, String format) {

    if (targetType == DataType.STRING) {
      String result = NumberFormat.format(this.toBigNumber(), format);
      return ValueString.of(result);
    }

    throw createUnsupportedConversionError(targetType);
  }

  @Override
  public Value add(Value v) {
    if (v.isNull())
      return NULL;
    if (this.getType().compareTo(v.getType()) >= 0) {
      return ValueNumber.of(value + v.toNumber());
    }

    return v.add(this);
  }

  @Override
  public Value subtract(Value v) {
    if (v.isNull())
      return NULL;
    if (this.getType().compareTo(v.getType()) >= 0) {
      return ValueNumber.of(value - v.toNumber());
    }

    return this.convertTo(v.getType()).subtract(v);
  }

  @Override
  public Value multiply(Value v) {
    if (v.isBigNumber()) {
      return v.multiply(this);
    }
    if (v.isNull())
      return NULL;
    return ValueNumber.of(value * v.toNumber());
  }

  @Override
  public Value divide(Value v) {
    if (v.isNull())
      return NULL;
    // prevent a division by zero ..
    if (v.signum() == 0)
      throw createDivisionByZeroError();    
    if (v.isBigNumber()) {
      return ValueBigNumber.of(
          this.toBigNumber().divide(v.toBigNumber(), ValueBigNumber.DEFAULT_SCALE, BigDecimal.ROUND_HALF_UP));
    }

    return ValueNumber.of(value / v.toNumber());
  }

  @Override
  public Value remainder(Value v) {
    if (v.isNull())
      return NULL;
    // prevent a division by zero ..
    if (v.signum() == 0)
      throw createDivisionByZeroError();    
    if (v.isBigNumber()) {
      return ValueBigNumber.of(this.toBigNumber().remainder(v.toBigNumber()));
    }

    return ValueNumber.of(value % v.toNumber());
  }

  @Override
  public Value power(Value v) {
    if (v.isNull())
      return NULL;
    if (v.signum() == 0)
      return ONE;
    if (v.signum() < 0)
      throw new ArithmeticException("Cannot power negative " + v);
    return ValueNumber.of(Math.pow(value, v.toNumber()));
  }
}
