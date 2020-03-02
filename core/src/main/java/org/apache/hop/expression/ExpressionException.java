package org.apache.hop.expression;

import org.apache.hop.i18n.BaseMessages;

public class ExpressionException extends RuntimeException {

	private static final Class<?> PKG = Expression.class; // for i18n purposes

	private static final long serialVersionUID = 8634955627375465878L;

	private Expression expression;

	public ExpressionException(String message, Expression expression, Throwable cause) {
		super(message, cause);
		this.expression = expression;
	}

	public ExpressionException(String messageID, Object... args) {
		super(BaseMessages.getString(PKG, messageID, args));
	}

	public Expression getExpression() {
		return expression;
	}
}
