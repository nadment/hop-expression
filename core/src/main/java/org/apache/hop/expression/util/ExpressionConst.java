package org.apache.hop.expression.util;

public class ExpressionConst {
  
  private ExpressionConst() {
    // utility class
  }
    
  /** TODO: Control Two-digit year, when set to 1980, values of 79 and 80 parsed as 2079 and 1980 respectively. */  
  public static final String TWO_DIGIT_CENTURY_START = "TWO_DIGIT_CENTURY_START";
  
  public static final String WEEK_START = "WEEK_START";
}
