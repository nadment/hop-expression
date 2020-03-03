package org.apache.hop.expression;

import java.io.StringWriter;

/**
 * An expression is a combination of one or more values, operators and functions
 * that evaluate to a value.
 * 
 * @author Nicolas ADMENT
 *
 */
public abstract class Expression implements Cloneable {

	public static Expression parse(String source) throws ExpressionException {
		ExpressionParser parser = new ExpressionParser(source);

		return parser.parse();
	}

	public Expression clone() {
		Expression expression = null;
		try {
			expression = (Expression) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return expression;
	}

	/**
	 * Check if this expression will always return the same value.
	 *
	 * @return if the expression is constant
	 */
	public boolean isConstant() {
		return false;
	}

	/**
	 * Return the resulting value for the current context.
	 *
	 * @param context the context
	 * @return the result
	 */
	public abstract Value eval(ExpressionContext context) throws ExpressionException;

	/**
	 * Try to optimize the expression.
	 *
	 * @param context the context
	 * @return the optimized expression
	 */
	public Expression optimize(ExpressionContext context) throws ExpressionException {
		return this;
	}

	/**
	 * Appends this expression statement to the specified writer. This may not
	 * always be the original expression statement, specially after optimization.
	 */
	public abstract void unparse(StringWriter writer, int leftPrec, int rightPrec);

	public String toString() {
		StringWriter writer = new StringWriter();
		this.unparse(writer, 0, 0);
		return writer.toString();
	}
}
