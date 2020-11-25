package org.apache.hop.expression.value;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.time.ZonedDateTime;
import java.util.Objects;

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Value;
import org.apache.hop.expression.util.ToChar;

public class ValueNumber extends Value {

  private final double value;

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

  public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
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
      String result = ToChar.toChar(this.toBigNumber(), format, context.getLocale());
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
