package org.apache.hop.expression.value;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Locale;
import java.util.Objects;

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Value;
import org.apache.hop.expression.util.ToChar;

public class ValueDate extends Value {

	// FIXME: Format
	private static final DateTimeFormatter TIMESTAMP_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.nnn")
			.withLocale(Locale.ROOT).withZone(ZoneId.systemDefault());
	private static final DateTimeFormatter DATE_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd")
			.withLocale(Locale.ROOT).withZone(ZoneId.systemDefault());

	private static final BigDecimal SECONDS_BY_DAY = new BigDecimal(24 * 60 * 60);
	//private static final BigDecimal NANOS_BY_DAY = new BigDecimal(24 * 60 * 60 * 1000000000);

	private final Instant value;

	public ValueDate(Instant date) {
		this.value = Objects.requireNonNull(date);
	}

	@Override
	public DataType getDataType() {
		return DataType.DATE;
	}

	@Override
	public Object getObject() {
		return value;
	}

	@Override
	public int hashCode() {
		return value.hashCode();
	}

	@Override
	public boolean equals(Object other) {
		return other instanceof ValueDate && value.equals(((ValueDate) other).value);
	}

	@Override
	public int compare(Value v) {
		return value.compareTo(v.toDate());
	}

	@Override
	public Value eval(IExpressionContext context) throws ExpressionException {
		return this;
	}

	public Instant toDate() {
		return value;
	}

	@Override
	public String toString() {
		if (value.getNano() > 0)
			return TIMESTAMP_FORMAT.format(value);

		return DATE_FORMAT.format(value);
	}

	/**
	 * Converts this date time to the number of milliseconds from the epochof
	 * 1970-01-01T00:00:00Z.
	 */
	public long toInteger() {
		long time = value.toEpochMilli();

		return time;
	}

	/**
	 * Converts this date time to the number of milliseconds from the epochof
	 * 1970-01-01 00:00:00.
	 */
	public double toNumber() {
		long time = value.toEpochMilli();

		return (double) time;
	}

	
	@Override
	public Value convertTo(final IExpressionContext context, final DataType targetType, String format) {

		if (targetType==DataType.STRING) {			
			ZonedDateTime dt = ZonedDateTime.ofInstant(value,context.getZone());			
			String result = ToChar.toChar(dt, format, context.getLocale());
			
			return new ValueString(result);			
		}
		
		throw createUnsupportedConversionError(targetType);
	}	
	
	
	@Override
	public Value add(Value v) {
		// Computes fraction of day
		if (v.isNumeric()) {

			BigDecimal number = v.toBigNumber();
			long seconds = number.multiply(SECONDS_BY_DAY).setScale( 0, BigDecimal.ROUND_HALF_UP ).longValue();

			return Value.of(value.plusSeconds(seconds));
		}

		return super.add(v);
	}


	@Override
	public Value subtract(Value v) {
		if (v.isNumeric()) {

			BigDecimal number = v.toBigNumber();
			long seconds = number.multiply(SECONDS_BY_DAY).setScale( 0, BigDecimal.ROUND_HALF_UP ).longValueExact();

			return Value.of(value.minusSeconds(seconds));
		}

		return super.add(v);
	}

	public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
		writer.append(this.toString());
	}
}
