package org.apache.hop.expression;

import java.io.StringWriter;

public class ExpressionIdentifier extends Expression {

	private final String name;

	public ExpressionIdentifier(final String name) {
		super();

		this.name = name;
	}

	public String getName() {
		return name;
	}

	@Override
	public Value eval(IExpressionContext context) throws ExpressionException {
		return context.resolve(this.name);
	}

	@Override
	public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
		writer.append(this.name);
	}

	@Override
	public String toString() {
		return this.name;
	}
}
