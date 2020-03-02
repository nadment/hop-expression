package org.apache.hop.expression;

public class ExpressionParserException extends ExpressionException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8634955627375465878L;

	private final String source;
	private final int position;

	public ExpressionParserException(String message, String source, int position) {
		super(message, source, position);
		this.source = source;
		this.position = position;
	}

	public String getSource() {
		return source;
	}

	public int getPosition() {
		return position;
	}
}
