package org.apache.hop.expression;

import java.time.ZoneId;
import java.util.Locale;
import java.util.Random;

// OR optimization:
// If a clause consists of multiple predicates that are connected by the OR operator, optimization occurs only if the same column and comparison operator are used in every predicate, and the comparison operator is LIKE or equal to (=).

public class DefaultExpressionContext implements IExpressionContext {

	private ZoneId zone;
	private Locale locale;
	private Random random;

	public DefaultExpressionContext() {
		super();

		this.locale = Locale.ROOT;
		this.zone = ZoneId.of("UTC");
		this.random = new Random();
	}

	@Override
	public Value resolve(String name) throws ExpressionException {

		throw new ExpressionException("ExpressionException.OptimizerError", name);

	}

	public Locale getLocale() {
		return locale;
	}

	public ZoneId getZone() {
		return zone;
	}

	public Random getRandom() {
		return random;
	}

}
