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

public class ValueInteger extends Value {

  private final long value;
  
  /** Integer value of 0. */
  public static final Value ZERO = new ValueInteger(0L);

  /** Integer value of 1. */
  public static final Value ONE = new ValueInteger(1L);


  public ValueInteger(long value) {
    this.value = Objects.requireNonNull(value);
  }

  @Override
  public DataType getDataType() {
    return DataType.INTEGER;
  }

  @Override
  public Object getObject() {
    return value;
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
  public int signum() {
    return Long.signum(value);
  }

  @Override
  public Value negate() throws ExpressionException {
    if (value == Long.MIN_VALUE) {
      throw createOverflowError();
    }
    return new ValueInteger(-value);
  }

  @Override
  public long toInteger() {
    return value;
  }

  @Override
  public double toNumber() {
    return (double) value;
  }

  @Override
  public BigDecimal toBigNumber() {
    return BigDecimal.valueOf(value);
  }

  @Override
  public byte[] toBinary() {
    byte[] buffer = new byte[8];
    int v = (int) (value >> 32);

    buffer[0] = (byte) v;
    buffer[1] = (byte) (v >> 8);
    buffer[2] = (byte) (v >> 16);
    buffer[3] = (byte) (v >> 24);
    buffer[4] = (byte) (value >> 32);
    buffer[5] = (byte) (value >> 40);
    buffer[6] = (byte) (value >> 48);
    buffer[7] = (byte) (value >> 56);

    return buffer;
  }

  @Override
  public String toString() {
    return Long.toString(value);
  }

  @Override
  public boolean toBoolean() {
    return value != 0 ? true : false;
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
    return other instanceof ValueInteger && value == ((ValueInteger) other).value;
  }

  @Override
  public int compare(Value v) {
    return Long.compare(value, v.toInteger());
  }

  @Override
  public Value eval(IExpressionContext context) throws ExpressionException {
    return this;
  }

  public void write(StringWriter writer, int leftPrec, int rightPrec) {
    writer.append(Long.toString(value));
  }

  @Override
  public Value add(Value v) {
    if (this.getDataType().compareTo(v.getDataType()) >= 0) {
      return Value.of(value + v.toInteger());
    }

    return v.add(this);
  }

  @Override
  public Value subtract(Value v) {
    if (this.getDataType().compareTo(v.getDataType()) >= 0) {
      return Value.of(value - v.toInteger());
    }

    return this.convertTo(v.getDataType()).subtract(v);
  }

  @Override
  public Value multiply(Value v) {
    if (v.isBigNumber()) {
      return v.multiply(this);
    }

    return Value.of(this.toNumber() * v.toNumber());
  }

  @Override
  public Value divide(Value v) {
    if (v.isBigNumber()) {
      return Value.of(
          this.toBigNumber().divide(v.toBigNumber(), MAX_SCALE, BigDecimal.ROUND_HALF_UP));
    }

    return Value.of(this.toNumber() / v.toNumber());
  }

  @Override
  public Value remainder(Value v) {
    if (v.isBigNumber()) {
      return Value.of(this.toBigNumber().remainder(v.toBigNumber()));
    }

    return Value.of(this.toNumber() % v.toNumber());
  }

  @Override
  public Value power(Value v) {
    return Value.of(Math.pow(value, v.toNumber()));
  }
}
