package org.apache.hop.expression.value;

import java.io.StringWriter;

import org.apache.hop.expression.ValueType;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Value;

/**
 * Implementation of NULL. NULL is not a regular data type.
 */
public class ValueNull extends Value {

	public ValueNull() {
	}

	@Override
	public ValueType getType() {
		return ValueType.NONE;
	}
	
	@Override
	public Object getObject() {
		return null;
	}

	@Override
	public boolean toBoolean() {
		return false;
	}

	@Override
	public double toNumber() {
		return 0.0;
	}

	@Override
	public String toString() {
		return "NULL";
	}

	@Override
	public long toInteger() {
		return 0L;
	}

	@Override
	public int hashCode() {
		return 0;
	}

	@Override
	public boolean equals(Object other) {
		return other == this;
	}

	public boolean isNull() {
		return true;
	}

	@Override
	public Value negate() {
		return this;
	}

	@Override
	public Value eval(IExpressionContext context) throws ExpressionException {
		return this;
	}

	public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
		writer.append(this.toString());
	}

	@Override
	public int compare(Value o) {
		if (o.isNull())
			return 0;

		// null is always smaller
		return -1;
	}
}
