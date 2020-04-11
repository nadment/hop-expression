package org.apache.hop.expression;

import java.io.StringWriter;

/**
 * An expression is a combination of one or more values, operators and functions
 * that evaluate to a value.
 * 
 * @author Nicolas ADMENT
 *
 */
public abstract class Expression implements IExpression {


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
	 * Return the resulting value for the current context.
	 *
	 * @param context the context
	 * @return the result
	 */
	public abstract Value eval(IExpressionContext context) throws ExpressionException;

	/**
	 * Try to optimize the expression.
	 *
	 * @param context the context
	 * @return the optimized expression
	 */
	public Expression optimize(IExpressionContext context) throws ExpressionException {
		return this;
	}

    /* If leftOperand is an OrNode, then we modify the tree from:
    *
    *                              Or1 
    *                           /      \
    *                      Or2              Nodex
    *                   /      \                ...
    *              left2        right2
    *
    *      to:
    *
    *                                    Or1 
    *                                 /      \
    *   changeToCNF(left2)          Or2
    *                           /        \
    *              changeToCNF(right2)  changeToCNF(Nodex)
    *
    *  NOTE: We could easily switch places between changeToCNF(left2) and 
    *  changeToCNF(right2).
    */	
	
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
