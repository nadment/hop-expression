package org.apache.hop.expression;

import org.apache.hop.i18n.BaseMessages;

public class ExpressionParserException extends ExpressionException {

  private static final Class<?> PKG = Expression.class; // for i18n purposes

  /** */
  private static final long serialVersionUID = 8634955627375465878L;

  private final String source;
  private final int position;

  public ExpressionParserException(String message, String source, int position) {
    super(message);
    this.source = source;
    this.position = position;
  }

  public String getSource() {
    return source;
  }

  /**
   * Returns the position where the error was found.
   *
   * @return the position where the error was found
   */
  public int getPosition() {
    return position;
  }

  public String toString() {

    String message = this.getMessage();
    message = (message != null) ? ": " + message : "";

    return BaseMessages.getString(PKG, "ExpressionParser.SyntaxError", position, message);
  }
}
