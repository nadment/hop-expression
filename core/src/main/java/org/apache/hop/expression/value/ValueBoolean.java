package org.apache.hop.expression.value;

import java.io.StringWriter;
import java.math.BigDecimal;

import org.apache.hop.expression.Type;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Value;

public class ValueBoolean extends Value {

	private final boolean value;

	public ValueBoolean(boolean value) {
		this.value = value;
	}

	@Override
	public String toString() {
		return value ? "TRUE" : "FALSE";
	}

	@Override
	public boolean toBoolean() {
		return value;
	}

	@Override
	public int hashCode() {
		return value ? 1 : 0;
	}

	@Override
	public boolean equals(Object other) {
		// there are only ever two instances, so the instance must match
		return this == other;
	}

	@Override
	public int compare(Value v) {
		if (value && v.toBoolean() || !value && !v.toBoolean()) {
			return 0; // true == true, false == false
		}
		if (value && !v.toBoolean()) {
			return 1; // true > false
		}
		return -1; // false < true
	}

	@Override
	public Value eval(IExpressionContext context) throws ExpressionException {
		return this;
	}

	@Override
	public Type getType() {
		return Type.BOOLEAN;
	}
	
	@Override
	public Object getObject() {
		return value;
	}

	public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
		writer.append(this.toString());
	}

	@Override
	public Value negate() {
		return value ? Value.FALSE : Value.TRUE;
	}

	@Override
	public BigDecimal toBigNumber() throws ExpressionException {
		return value ? BigDecimal.ONE : BigDecimal.ZERO;
	}

	@Override
	public double toNumber() throws ExpressionException {
		return value ? 1.0 : 0.0;
	}

	@Override
	public long toInteger() throws ExpressionException {
		// avoids creating new Long objects all the time.
		return value ? 1L : 0L;
	}
}
