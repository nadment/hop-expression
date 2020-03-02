package org.apache.hop.expression.value;

import java.io.StringWriter;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Objects;

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Value;

public class ValueDate extends Value {

	private final LocalDateTime value;

	public ValueDate(LocalDateTime date) {
		this.value = Objects.requireNonNull(date);
	}

	@Override
	public DataType getType() {
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
	public Value eval(ExpressionContext context) throws ExpressionException {
		return this;
	}

	public LocalDateTime toDate() {
		return value;
	}


	@Override
	// TODO: fix me
	public String toString() {
		return "NULL";
	}

	/**
	 * Converts this date time to the number of milliseconds from the epochof
	 * 1970-01-01T00:00:00Z.
	 */
	public long toInteger() {
		long time = value.toInstant(ZoneOffset.ofHours(0)).toEpochMilli();

		return time;
	}

	/**
	 * Converts this date time to the number of milliseconds from the epochof
	 * 1970-01-01T00:00:00Z.
	 */
	public double toNumber() {
		long time = value.toInstant(ZoneOffset.ofHours(0)).toEpochMilli();

		return (double) time;
	}

	public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
		writer.append(this.toString());
	}
}
