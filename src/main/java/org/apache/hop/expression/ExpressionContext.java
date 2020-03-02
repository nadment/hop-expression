package org.apache.hop.expression;

import java.time.ZoneId;
import java.util.Locale;
import java.util.Random;

public interface ExpressionContext {

	/**
	 * Resolves the given reference. How the name is interpreted by the outside
	 * system is an implementation detail. Resolve is case sensitive.
	 *
	 * @param name the name that identifies the reference.
	 * @return the resolved value.
	 * @throws ExpressionException if an error occurs.
	 */
	public Value resolve(String name) throws ExpressionException;

	public Locale getLocale();

	public ZoneId getZone();

//	/**
//	 * Returns the current date.
//	 *
//	 * @return the date.
//	 */
//	public LocalDateTime getCurrentDate();

	public Random getRandom();
}
