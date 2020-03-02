package org.apache.hop.expression.value;

import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Value;

public class ValueBinary extends Value {
	/**
	 * The binary data.
	 */
	private final byte[] value;

	public ValueBinary(byte[] value) {
		this.value = Objects.requireNonNull(value);
	}

	@Override
	public DataType getType() {
		return DataType.BINARY;
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
		return other instanceof ValueBinary && value.equals(((ValueBinary) other).value);
	}

	@Override
	public int compare(Value v) {
		byte[] o = v.toBinary();

		int length = value.length < o.length ? value.length : o.length;

		int compare = value.length - o.length;
		if (compare == 0) {
			for (int i = 0; i < length; i++) {
				compare = value[i] - o[i];
				if (compare != 0) {
					compare = compare < 0 ? -1 : 1;
					break;
				}
			}
		}

		return compare;
	}

	public byte[] toBinary() throws ExpressionException {
		return value;
	}

	@Override
	public Value eval(ExpressionContext context) throws ExpressionException {
		return this;
	}

	public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
		writer.append('\'');
		writer.append(toString());
		writer.append('\'');
	}

	@Override
	public String toString() {
		return new String(value, StandardCharsets.UTF_8);
	}
}
