package org.apache.hop.expression;

public class ExpressionException extends RuntimeException {

	private static final long serialVersionUID = 8634955627375465878L;

	public ExpressionException(String message) {
		super(message);
	}

	public ExpressionException(String message, Throwable exception) {
		super(message, exception);
	}
		
	
//	public ExpressionException(String messageID, Object... args) {
//		super(BaseMessages.getString(PKG, messageID, args));
//	}
}
