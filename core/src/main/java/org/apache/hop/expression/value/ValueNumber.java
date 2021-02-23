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
import org.apache.hop.expression.Value;
import org.apache.hop.expression.util.NumberFormat;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.util.Objects;

public class ValueNumber extends Value {

  private final double value;

  /** Number value of 0. */
  public static final Value ZERO = new ValueNumber(0.0);

  /** Number value of 1. */
  public static final Value ONE = new ValueNumber(1.0);


  public ValueNumber(double value) {
    this.value = value;
  }

  protected ValueNumber(Double value) {
    this.value = Objects.requireNonNull(value);
  }

  @Override
  public DataType getDataType() {
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

  public void write(StringWriter writer, int leftPrec, int rightPrec) {
    writer.append(this.toString());
  }

  @Override
  public int signum() {
    return value == 0 ? 0 : (value < 0 ? -1 : 1);
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
      String result = NumberFormat.format(this.toBigNumber(), format, context.getLocale());
      return new ValueString(result);
    }

    throw createUnsupportedConversionError(targetType);
  }

  @Override
  public Value add(Value v) {
    if (this.getDataType().compareTo(v.getDataType()) >= 0) {
      return Value.of(value + v.toNumber());
    }

    return v.add(this);
  }

  @Override
  public Value subtract(Value v) {
    if (this.getDataType().compareTo(v.getDataType()) >= 0) {
      return Value.of(value - v.toNumber());
    }

    return this.convertTo(v.getDataType()).subtract(v);
  }

  @Override
  public Value multiply(Value v) {
    if (v.isBigNumber()) {
      return v.multiply(this);
    }
    return Value.of(value * v.toNumber());
  }

  @Override
  public Value divide(Value v) {
    if (v.isBigNumber()) {
      return Value.of(
          this.toBigNumber().divide(v.toBigNumber(), MAX_SCALE, BigDecimal.ROUND_HALF_UP));
    }

    return Value.of(value / v.toNumber());
  }

  @Override
  public Value remainder(Value v) {
    if (v.isBigNumber()) {
      return Value.of(this.toBigNumber().remainder(v.toBigNumber()));
    }

    return Value.of(value % v.toNumber());
  }

  @Override
  public Value power(Value v) {
    return Value.of(Math.pow(value, v.toNumber()));
  }
}
