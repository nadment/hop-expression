package org.apache.hop.expression.util;

import org.apache.commons.lang.StringUtils;

/** Represents a capitalization strategy. */
public enum Capitalization {

  /** All letters are upper case. */
  UPPER,

  /** All letters are lower case. */
  LOWER,

  /** The string is capitalized (first letter upper case, subsequent letters lower case). */
  CAPITALIZE;

  /**
   * Returns the capitalization strategy which should be used when the first and second
   * letters have the specified casing.
   *
   * @param up1 whether or not the first letter is upper case 
   * @param up2 whether or not the second letter is upper case
   * @return the capitalization strategy
   */
  static Capitalization of(Boolean up1, Boolean up2) {
    if (up1 == null) {
      return Capitalization.CAPITALIZE;
    } else if (up2 == null) {
      return up1 ? Capitalization.UPPER : Capitalization.LOWER;
    } else if (up1) {
      return up2 ? Capitalization.UPPER : Capitalization.CAPITALIZE;
    } else {
      return Capitalization.LOWER;
    }
  }

  /**
   * Applies this capitalization strategy to the specified string.
   *
   * @param str the string to apply this strategy to
   * @return the resultant string
   */
  public String apply(String str) {
    if (str == null || str.isEmpty()) {
      return str;
    }
    switch (this) {
      case UPPER:
        return StringUtils.upperCase(str);
      case LOWER:
        return StringUtils.lowerCase(str);
      case CAPITALIZE:
        return Character.toUpperCase(str.charAt(0))
            + (str.length() > 1 ? StringUtils.lowerCase(str).substring(1) : "");
      default:
        throw new IllegalArgumentException("Unknown capitalization strategy: " + this);
    }
  }
}