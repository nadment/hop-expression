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
import org.apache.hop.expression.Value;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.i18n.BaseMessages;
import java.io.StringWriter;
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
  
  private final BigDecimal value;

  public ValueBigNumber(BigDecimal value) {
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
      String result = NumberFormat.format(value, format, context.getLocale());
      return new ValueString(result);
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
    // value and scale (thus 2.0 is not equal to 2.00 when using equals;
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
    return Value.of(value.add(v.toBigNumber()));
  }

  @Override
  public Value subtract(Value v) {
    return Value.of(value.subtract(v.toBigNumber()));
  }

  @Override
  public Value multiply(Value v) {
    return Value.of(value.multiply(v.toBigNumber()));
  }

  @Override
  public Value divide(Value v) {
    return Value.of(value.divide(v.toBigNumber(), DEFAULT_SCALE, BigDecimal.ROUND_HALF_UP));
  }

  public Value remainder(Value v) {
    return Value.of(value.remainder(v.toBigNumber()));
  }

  @Override
  public Value power(Value v) {
    // TODO: BigDecimal doesn't support for fractional, so we use double
    double result = FastMath.pow(value.doubleValue(), v.toNumber());
    return Value.of(result);
  }
}
