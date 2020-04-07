package org.apache.hop.expression;

public interface IExpression {
	
	//	/**
//	 * Check if this expression will always return the same value.
//	 *
//	 * @return if the expression is constant
//	 */
//	public boolean isConstant();

	/**
	 * Return the resulting value for the current context.
	 *
	 * @param context the context
	 * @return the result
	 */
	public abstract Value eval(IExpressionContext context) throws ExpressionException;

//	/**
//	 * Try to optimize the expression.
//	 *
//	 * @param context the context
//	 * @return the optimized expression
//	 */
//	public IExpression optimize(IExpressionContext context) throws ExpressionException;

}
