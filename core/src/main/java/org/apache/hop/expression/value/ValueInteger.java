package org.apache.hop.expression.value;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.util.Objects;

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Value;

public class ValueInteger extends Value {

	private final long value;

	public ValueInteger(Long value) {
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

	public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
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
		if ( v.isBigNumber() ) {
			return v.multiply(this);
		}
		
		 return Value.of(this.toNumber() * v.toNumber());
	}

	@Override
	public Value divide(Value v) {
		if ( v.isBigNumber() ) {			
			return Value.of(this.toBigNumber().divide(v.toBigNumber(),MAX_SCALE, BigDecimal.ROUND_HALF_UP));
		}
		
		return Value.of(this.toNumber() / v.toNumber());
	}

	@Override
	public Value remainder(Value v) {	
		if ( v.isBigNumber() ) {
			return Value.of(this.toBigNumber().remainder(v.toBigNumber()));
		}
		
		return Value.of(this.toNumber() % v.toNumber());
	}

	@Override
	public Value power(Value v) {
		return Value.of(Math.pow(value, v.toNumber()));
	}
}
