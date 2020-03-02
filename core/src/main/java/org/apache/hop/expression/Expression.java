package org.apache.hop.expression;

import java.io.StringWriter;

/**
 * An expression is a combination of one or more values, resolvables identifiers, operators and functions
 * that evaluate to a value.
 * 
 * @author Nicolas ADMENT
 *
 */
public abstract class Expression implements IExpression {

	protected static final Class<?> PKG = Expression.class; // for i18n purposes

	public abstract Kind getKind();
	
	public boolean is(Kind kind) {
		return this.getKind()==kind;
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
     * Check if this expression will always return the NULL value.
     *
     * @return if the expression is constant NULL value
     */
	public boolean isNull() {
		return false;
	}
	
	/**
	 * Return the resulting value for the current context.
	 *
	 * @param context the context
	 * @return the result
	 */
	public abstract Value eval(IExpressionContext context) throws ExpressionException;

    /**
     * Estimate the cost to process the expression.
     * Used when optimizing the query, to optimize the expression with the lowest estimated cost.
     *
     * @return the estimated cost
     */
    public abstract int getCost();
	
	/**
	 * Try to optimize the expression.
	 *
	 * @param context the context
	 * @return the optimized expression
	 */
	public Expression optimize(IExpressionContext context) throws ExpressionException {
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
