package org.apache.hop.expression.value;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Value;
import org.apache.hop.i18n.BaseMessages;

public class ValueString extends Value {
	/**
	 * The string data.
	 */
	private final String value;

	public ValueString(String value) {
		this.value = Objects.requireNonNull(value);
	}

	@Override
	public DataType getDataType() {
		return DataType.STRING;
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
		return other instanceof ValueString && value.equals(((ValueString) other).value);
	}

	@Override
	public int compare(Value v) {
		return value.compareTo(v.toString());
	}

	@Override
	public boolean toBoolean() {
		switch (value.length()) {
		case 1:
			if (value.equals("1") || value.equalsIgnoreCase("t") || value.equalsIgnoreCase("y")) {
				return true;
			}
			if (value.equals("0") || value.equalsIgnoreCase("f") || value.equalsIgnoreCase("n")) {
				return false;
			}
			break;
		case 2:
			if (value.equalsIgnoreCase("on")) {
				return true;
			}

			if (value.equalsIgnoreCase("no")) {
				return false;
			}
			break;
		case 3:
			if (value.equalsIgnoreCase("yes")) {
				return true;
			}
			if (value.equalsIgnoreCase("off")) {
				return false;
			}
			break;
		case 4:
			if (value.equalsIgnoreCase("true")) {
				return true;
			}
			break;
		case 5:
			if (value.equalsIgnoreCase("false")) {
				return false;
			}
		}
		return false;
	}

	@Override
	public Value eval(IExpressionContext context) throws ExpressionException {
		return this;
	}

	public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
		writer.append('\'');
		writer.append(String.valueOf(value));
		writer.append('\'');
	}

	@Override
	public String toString() {
		return value;
	}

	@Override
	public byte[] toBinary() {
		return value.getBytes(StandardCharsets.UTF_8);
	}

	@Override
	public long toInteger() {
		try {

			if (value.indexOf('.') < 0)
				return Long.parseLong(value);

			return (long) Double.parseDouble(value);
		} catch (NumberFormatException e) {
			throw new ExpressionException(BaseMessages.getString(PKG, "Expression.InvalidNumeric", value));
		}
	}

	@Override
	public double toNumber() {
		try {
			return Double.parseDouble(value);
		} catch (NumberFormatException e) {
			throw new ExpressionException(BaseMessages.getString(PKG, "Expression.InvalidNumeric", value));
		}
	}

	@Override
	public BigDecimal toBigNumber() {
		try {
			return new BigDecimal(value.trim());
		} catch (NumberFormatException e) {
			throw new ExpressionException(BaseMessages.getString(PKG, "Expression.InvalidNumeric", value));
		}
	}
}
