package org.apache.hop.expression;

import java.time.Instant;
import java.time.ZoneId;
import java.util.Locale;
import java.util.Random;

import org.apache.hop.i18n.BaseMessages;

// OR optimization:
// If a clause consists of multiple predicates that are connected by the OR operator, optimization occurs only if the same column and comparison operator are used in every predicate, and the comparison operator is LIKE or equal to (=).

public class DefaultExpressionContext implements IExpressionContext {
	
	protected static final Class<?> PKG = Expression.class; // for i18n purposes
	
	private ZoneId zone;
	private Locale locale;
	private Random random;
	private Instant currentDate;
	
	public DefaultExpressionContext() {
		super();

		this.locale = Locale.ROOT;
		this.zone = ZoneId.of("UTC");
		this.random = new Random();
		this.currentDate = Instant.now();
	}

	@Override
	public Value resolve(String name) throws ExpressionException {
		throw new ExpressionException(BaseMessages.getString(PKG,"Expression.OptimizerError", name));
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
	
	public Instant getCurrentDate() {
		return currentDate;
	}
}
