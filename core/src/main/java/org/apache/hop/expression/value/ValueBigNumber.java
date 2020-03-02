package org.apache.hop.expression.value;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.util.Objects;

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Value;
import org.apache.hop.expression.util.ToChar;

public class ValueBigNumber extends Value {

	/**
	 * The value 'zero'.
	 */
	public static final ValueBigNumber ZERO = new ValueBigNumber(BigDecimal.ZERO);

	/**
	 * The value 'one'.
	 */
	public static final ValueBigNumber ONE = new ValueBigNumber(BigDecimal.ONE);

	/**
	 * The default precision for a decimal value.
	 */
	static final int DEFAULT_PRECISION = 65535;

	/**
	 * The default scale for a decimal value.
	 */
	static final int DEFAULT_SCALE = 32767;

	private final BigDecimal value;

	public ValueBigNumber(BigDecimal value) {
		this.value = Objects.requireNonNull(value);
	}

	@Override
	public DataType getDataType() {
		return DataType.BIGNUMBER;
	}

	@Override
	public Object getObject() {
		return value;
	}

	@Override
	public Value convertTo(final IExpressionContext context, final DataType targetType, String format) {

		if (targetType==DataType.STRING) {			
			String result = ToChar.toChar(value, format, context.getLocale());			
			return new ValueString(result);			
		}
		
		throw createUnsupportedConversionError(targetType);
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
		if (s.equals("0")) {
			return s;
		} else if (s.startsWith("0.")) {
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
		return value.longValue();
	}

	@Override
	public double toNumber() {
		return value.doubleValue();
	}

	public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
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
		return Value.of(value.divide(v.toBigNumber(), MAX_SCALE, BigDecimal.ROUND_HALF_UP));
	}

	public Value remainder(Value v) {
		return Value.of(value.remainder(v.toBigNumber()));
	}

	@Override
	public Value power(Value v) {
		return Value.of(value.pow((int) v.toInteger()));
	}

}
