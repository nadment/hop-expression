/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression;


import org.apache.commons.io.IOUtils;
import org.apache.hop.core.logging.LogChannel;
import org.apache.hop.core.util.TranslateUtil;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.time.ZonedDateTime;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Operators have the precedence levels. An operator on higher levels is evaluated before an
 * operator on a lower level
 */
public abstract class Operator implements Comparable<Operator> {

  protected static final Class<?> PKG = Operator.class; // for i18n purposes

  private static final ConcurrentHashMap<String, String> docs = new ConcurrentHashMap<>();
  
  /** The name of the operator/function. Ex. "COS" or "TRIM" */
  protected final String name;

  /** The alias of the function. Ex. "TRUNCATE" alias "TRUNC"    */
  protected final String alias;

  /**
   * The precedence with which this operator binds to the expression to the left. This is less than
   * the right precedence if the operator is left-associative.
   */
  private final int leftPrecedence;

  /**
   * The precedence with which this operator binds to the expression to the right. This is more than
   * the left precedence if the operator is left-associative.
   */
  private final int rightPrecedence;

  private final boolean isDeterministic;

  private final String category;

  private final String description;

  /**
   * Create a new operator for use in expressions.
   *
   * @param name The name of operator
   * @param alias The symbol of the operator or alias of function
   * @param precedence The precedence value of the operator
   * @param isLeftAssociative Set to true if the operator is left associative, false if it is right associative
   * @param isDeterministic Set to true if the operator always returns the same result for the same parameters
   * @param category The category to group operator
   */
  protected Operator(String name, String alias, int precedence, boolean isLeftAssociative, boolean isDeterministic,
      String category) {
    this.name = name;
    this.alias = alias;
    this.leftPrecedence = leftPrecedence(precedence, isLeftAssociative);
    this.rightPrecedence = rightPrecedence(precedence, isLeftAssociative);
    this.isDeterministic = true;
    this.category = TranslateUtil.translate(category, Operator.class);
    this.description = findDescription(name);
  }
  
  protected Operator(String name, int precedence, boolean isLeftAssociative, boolean isDeterministic,
      String category) {
    this(name, name,  precedence, isLeftAssociative, isDeterministic, category);    
  }
  
  protected static int leftPrecedence(int precedence, boolean isLeftAssociative) {
    assert (precedence % 2) == 0;
    if (isLeftAssociative) {
      ++precedence;
    }
    return precedence;
  }

  protected static int rightPrecedence(int precedence, boolean isLeftAssociative) {
    assert (precedence % 2) == 0;
    if (!isLeftAssociative) {
      ++precedence;
    }
    return precedence;
  }

  /** 
   * The unique name of the operator
   */
  public String getName() {
    return name;
  }

  public String getAlias() {
    return alias;
  }

  public int getLeftPrecedence() {
    return leftPrecedence;
  }

  public int getRightPrecedence() {
    return rightPrecedence;
  }

  /**
   * Whether the operator always returns the same result for the same parameters.
   *
   * @return true if it does
   */
  public boolean isDeterministic() {
    return isDeterministic;
  }
  
  public URL getDocumentationUrl() {
    return getClass().getResource("/docs/" + name.toLowerCase() + ".html");
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof Operator)) {
      return false;
    }
    if (!obj.getClass().equals(this.getClass())) {
      return false;
    }
    Operator other = (Operator) obj;
    return name.equals(other.name) && alias.equals(other.alias);
  }

  public boolean isAlias(Operator other) {
    if (other==null)
      return false;    
    return name.equals(other.name);
  }
  
  @Override
  public int hashCode() {
    return Objects.hash(name,alias);
  }

  /**
   * Get the category of operator
   * 
   * @return
   */
  public String getCategory() {
    return category;
  }

  /**
   * Get the description of operator
   * 
   * @return
   */
  public String getDescription() {
    return description;
  }

  public abstract Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException;

  public abstract void write(StringWriter writer, IExpression[] operands);

  @Override
  public int compareTo(Operator o) {
    // Compare with name
    int compare =  this.name.compareTo(o.name);
    if ( compare!=0 )
      return compare;
    
    // Primary operator first and alias last
    return this.alias==null ? 1:0; 
  }

  private static final String JAVA_REGEX_SPECIALS = "\\.[]{}()<>*+-=!?^$|";


  /**
   * Compare this value against another value. If values need to be converted to match the other
   * operands data type, the value with the lower order is converted to the value with the higher
   * order.
   *
   * @param value the other value
   * @return 0 if both values are equal, -1 if this value is smaller, and 1 otherwise
   */
  public static int compareTo(Object left, Object right) {

    if (left == null && right == null)
      return 0;
    if (left == null)
      return -1;
    if (right == null)
      return 1;

    // The lower order data type is converted
    if (left instanceof byte[] || right instanceof byte[]) {
      return compare(coerceToBinary(left), coerceToBinary(right));
    }
    if (left instanceof ZonedDateTime || right instanceof ZonedDateTime) {
      return coerceToDate(left).compareTo(coerceToDate(right));
    }
    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return coerceToBigNumber(left).compareTo(coerceToBigNumber(right));
    }
    if (left instanceof Double || right instanceof Double) {
      return coerceToNumber(left).compareTo(coerceToNumber(right));
    }
    if (left instanceof Long || right instanceof Long) {
      return coerceToInteger(left).compareTo(coerceToInteger(right));
    }
    if (left instanceof Boolean || right instanceof Boolean) {
      return coerceToBoolean(left).compareTo(coerceToBoolean(right));
    }

    return coerceToString(left).compareTo(coerceToString(right));
  }

  protected static int compare(byte[] left, byte[] right) {
    int length = left.length < right.length ? left.length : right.length;

    int compare = left.length - right.length;
    if (compare == 0) {
      for (int i = 0; i < length; i++) {
        compare = left[i] - right[i];
        if (compare != 0) {
          compare = compare < 0 ? -1 : 1;
          break;
        }
      }
    }

    return compare;
  }

  /**
   * Convert a value to the specified type.
   *
   * @param value the value to convert
   * @param type the data type of the returned value
   * @return the converted value
   */
  public static Object convertTo(Object value, final DataType type) {
    return convertTo(value, type, null);
  }

  public static Object convertTo(Object value, final DataType type, String pattern) {
    if (value == null) {
      return null;
    }    

    if ( type.isInstance(value) )
      return value;
    
    switch (type) {
      case BOOLEAN:
        if (value instanceof Number) {
          return ((Number) value).intValue() != 0;
        }
        if (value instanceof String) {
          return convertStringToBoolean( (String) value);         
        }
        break;
      case INTEGER:
        if (value instanceof Number) {
          return ((Number) value).longValue();
        }
        if (value instanceof Boolean) {
          return ((boolean) value) ? 1L:0L;
        }
        if (value instanceof String) {
          return convertStringToInteger((String) value);
        }
        if (value instanceof byte[]) {
          return convertBinaryToInteger((byte[]) value);
        }
        break;
      case NUMBER:
        if (value instanceof Boolean) {
          return ((boolean) value) ? 1D:0D;
        }
        if (value instanceof Number) {
          return Double.valueOf(((Number) value).doubleValue());
        }
        if (value instanceof String) {
          return convertStringToNumber((String) value);
        }
        if (value instanceof byte[]) {
          return convertBinaryToNumber((byte[]) value);
        }
        break;
      case BIGNUMBER:
        if (value instanceof Boolean) {
          return ((boolean) value) ? BigDecimal.ONE:BigDecimal.ZERO;
        }
        if (value instanceof Long) {
          long v = (long) value;
          if (v==0L)
            return BigDecimal.ZERO;
          if (v==1L)
            return BigDecimal.ONE;      
          return BigDecimal.valueOf(v);
        }
        if (value instanceof Double) {
          double v = (double) value;
          if (v==0D)
            return BigDecimal.ZERO;
          if (v==1D)
            return BigDecimal.ONE;      
          return BigDecimal.valueOf(v);
        }
        if (value instanceof String) {
          return convertStringToBigNumber((String) value);
        }
        break;
      case STRING:
        if (value instanceof Boolean) {
          return ((boolean) value) ? "TRUE" : "FALSE";
        }
        if (value instanceof Number) {
          return NumberFormat.of(pattern).format(coerceToBigNumber(value));
        } 
        if (value instanceof ZonedDateTime) {          
          if ( pattern==null) pattern="YYYY-MM-DD";
          return DateTimeFormat.of(pattern).format((ZonedDateTime) value);
        }
        return coerceToString(value);
      case DATE:
        if (value instanceof String) {
          try {
            return DateTimeFormat.of(pattern).parse((String) value);
          } catch (RuntimeException | ParseException e) {
            throw ExpressionException.create("Expression.InvalidDate", value);
          }
        }
        break;
      case BINARY:
        if (value instanceof String) {
          return ((String) value).getBytes(StandardCharsets.UTF_8);
        }
        if (value instanceof Long) {
          return convertIntegerToBinary((Long) value);
        }
        break;
      case NONE:
        return null;
      default:    
    }
    throw errorUnsupportedConversion(value, type);
  }

  private static Long convertStringToInteger(String str) throws ExpressionException {
    try {
      BigDecimal number = NumberFormat.parse(str, 38, 0);
      return number.longValue();
    } catch (ParseException | NumberFormatException e) {
      throw ExpressionException.create("Expression.InvalidNumber", str);
    }
  }

  private static Double convertStringToNumber(String str) throws ExpressionException {
    try {
      return Double.parseDouble(str);
    } catch (NumberFormatException e) {
      throw ExpressionException.create("Expression.InvalidNumber", str);
    }
  }

  private static BigDecimal convertStringToBigNumber(String str) throws ExpressionException {
    try {
      return new BigDecimal(str);
    } catch (NumberFormatException e) {
      throw ExpressionException.create("Expression.InvalidNumber", str);
    }
  }

  private static byte[] convertIntegerToBinary(Long number) throws ExpressionException {
    byte[] buffer = new byte[8];
    int v = (int) (number >> 32);

    buffer[0] = (byte) v;
    buffer[1] = (byte) (v >> 8);
    buffer[2] = (byte) (v >> 16);
    buffer[3] = (byte) (v >> 24);
    buffer[4] = (byte) (number >> 32);
    buffer[5] = (byte) (number >> 40);
    buffer[6] = (byte) (number >> 48);
    buffer[7] = (byte) (number >> 56);

    return buffer;
  }
  
  private static Long convertBinaryToInteger(byte[] bytes) throws ExpressionException {   
    if (bytes.length > 8)
      throw new ExpressionException("Binary too big to fit in integer");
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result = result << 8;
      result = result | (bytes[i] & 0xFF);
    }
    return result;
  }

  private static Double convertBinaryToNumber(byte[] bytes) throws ExpressionException {   
    if (bytes.length > 8)
      throw new ExpressionException("Binary too big to fit in number");
    long result = 0;
    for (int i = 0; i < bytes.length; i++) {
      result = result << 8;
      result = result | (bytes[i] & 0xFF);
    }

    return Double.valueOf(result);
  }
  
  private static boolean convertStringToBoolean(String str) {
    switch (str.length()) {
      case 1:
        if (str.equals("1") || str.equalsIgnoreCase("t") || str.equalsIgnoreCase("y")) {
          return true;
        }
        if (str.equals("0") || str.equalsIgnoreCase("f") || str.equalsIgnoreCase("n")) {
          return false;
        }
        break;
      case 2:
        if (str.equalsIgnoreCase("on")) {
          return true;
        }
        if (str.equalsIgnoreCase("no")) {
          return false;
        }
        break;
      case 3:
        if (str.equalsIgnoreCase("yes")) {
          return true;
        }
        if (str.equalsIgnoreCase("off")) {
          return false;
        }
        break;
      case 4:
        if (str.equalsIgnoreCase("true")) {
          return true;
        }
        break;
      case 5:
        if (str.equalsIgnoreCase("false")) {
          return false;
        }
        break;
      default:
        break;
    }
    throw errorUnsupportedConversion(str, DataType.BOOLEAN);
  }

  public static Boolean coerceToBoolean(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof Boolean) {
      return (Boolean) value;
    }
    if (value instanceof Number) {
      return ((Number) value).intValue() != 0;
    }

    throw errorUnsupportedConversion(value, DataType.BOOLEAN);
  }

  public static byte[] coerceToBinary(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof byte[]) {
      return (byte[]) value;
    }
    if (value instanceof String) {
      return ((String) value).getBytes(StandardCharsets.UTF_8);
    }

    throw errorUnsupportedConversion(value, DataType.BINARY);
  }

  public static String coerceToString(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof String) {
      return (String) value;
    }
    if (value instanceof BigDecimal) {
      return NumberFormat.of("TM").format((BigDecimal) value);
    }
    if (value instanceof byte[]) {
      return new String((byte[]) value, StandardCharsets.UTF_8);
    }

    return String.valueOf(value);
  }

  public static Long coerceToInteger(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof Long) {
      return (Long) value;
    }
    if (value instanceof Number) {
      return ((Number) value).longValue();
    }
    if (value instanceof String) {
      return convertStringToInteger((String) value);
    }
    if (value instanceof Boolean) {
      return ((boolean) value) ? 1L : 0L;
    }
    if (value instanceof byte[]) {
      return convertBinaryToInteger((byte[]) value);
    }

    throw errorUnsupportedConversion(value, DataType.INTEGER);
  }

  public static Double coerceToNumber(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof Double) {
      return (Double) value;
    }
    if (value instanceof Number) {
      return Double.valueOf(((Number) value).doubleValue());
    }
    if (value instanceof String) {
      return convertStringToNumber((String) value);
    }
    if (value instanceof byte[]) {
      return convertBinaryToNumber((byte[]) value);
    }

    throw errorUnsupportedConversion(value, DataType.NUMBER);
  }

  public static BigDecimal coerceToBigNumber(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof BigDecimal) {
      return (BigDecimal) value;
    }
    if (value instanceof Long) {
      long v = (long) value;
      if (v==0L)
        return BigDecimal.ZERO;
      if (v==1L)
        return BigDecimal.ONE;      
      return BigDecimal.valueOf(v);
    }
    if (value instanceof Double) {
      double v = (double) value;
      if (v==0D)
        return BigDecimal.ZERO;
      if (v==1D)
        return BigDecimal.ONE;      
      return BigDecimal.valueOf(v);
    }
    if (value instanceof String) {
      return convertStringToBigNumber((String) value);
    }
    throw errorUnsupportedConversion(value, DataType.BIGNUMBER);
  }

  public static ZonedDateTime coerceToDate(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof ZonedDateTime) {
      return (ZonedDateTime) value;
    }
    throw errorUnsupportedConversion(value, DataType.DATE);
  }

  /** Translates a LIKE pattern to Java regex pattern, with optional escape string. */
  protected static String toRegexLike(String sqlPattern, CharSequence escapeStr) {
    final char escapeChar;
    if (escapeStr != null) {

      if (escapeStr.length() != 1) {
        throw ExpressionException.create("Expression.InvalidEscapeCharacter", escapeStr.toString());
      }

      escapeChar = escapeStr.charAt(0);
    } else {
      escapeChar = 0;
    }
    return toRegexLike(sqlPattern, escapeChar);
  }

  /** Translates a LIKE pattern to Java regex pattern. */
  protected static String toRegexLike(String sqlPattern, char escapeChar) {
    int i;
    final int len = sqlPattern.length();
    final StringBuilder javaPattern = new StringBuilder(len + len);
    for (i = 0; i < len; i++) {
      char c = sqlPattern.charAt(i);
      if (JAVA_REGEX_SPECIALS.indexOf(c) >= 0) {
        javaPattern.append('\\');
      }

      if (c == escapeChar) {
        if (i == (sqlPattern.length() - 1)) {
          throw ExpressionException.create("Expression.InvalidEscape", sqlPattern, i);
        }
        char nextChar = sqlPattern.charAt(i + 1);
        if ((nextChar == '_') || (nextChar == '%') || (nextChar == escapeChar)) {
          if (JAVA_REGEX_SPECIALS.indexOf(nextChar) >= 0) {
            javaPattern.append('\\');
          }
          javaPattern.append(nextChar);
          i++;
        } else {
          throw ExpressionException.create("Expression.InvalidEscape", sqlPattern, i);
        }
      } else if (c == '_') {
        javaPattern.append('.');
      } else if (c == '%') {
        javaPattern.append("(?s:.*)");
      } else {
        javaPattern.append(c);
      }
    }
    return javaPattern.toString();
  }

  public static final ExpressionException errorArgumentOutOfRange(Object arg) {
    return ExpressionException.create("Expression.ArgumentOutOfRange", arg);
  }

  public static final ExpressionException errorOverflow(String message) {
    return ExpressionException.create("Expression.Overflow", message);
  }

  public static final ExpressionException errorDivisionByZero() {
    return ExpressionException.create("Expression.DivisionByZero");
  }

  public static final ExpressionException errorUnsupportedConversion(Object value, DataType type) {
    return ExpressionException.create("Expression.UnsupportedConversion", value, DataType.fromJava(value), type);
  }
  
  public static ExpressionException errorFormatPattern(String s, int i) {
    return ExpressionException.create("Bad format {0} at position {1}", s, i);
  }

  public static ExpressionException errorRegexpPattern(String s) {
    return ExpressionException.create("Bad regexp {0}", s);
  }
  
  public static ExpressionException errorUnexpectedDataType(String name, DataType type) {
    return ExpressionException.create("Expression.UnexpectedDataType", name, type);
  }

  public static ExpressionException errorUnexpectedDatePart(String name, DatePart part) {
    return ExpressionException.create("Expression.UnexpectedDatePart", name, part);
  }
  
  public static String getHtml(String name) {
    String doc = docs.get(name);
    if (doc != null) {
      return doc;
    }

    doc = readAsciidoc(name);
    docs.put(name, doc);

    return doc;
  }

  private static String readAsciidoc(String name) {
    String file = "/docs/" + name.toLowerCase() + ".html";

    StringWriter writer = new StringWriter();

    try (
        InputStreamReader is = new InputStreamReader(IExpression.class.getResourceAsStream(file))) {
      IOUtils.copy(is, writer);
    } catch (Exception e) {
      writer.append(e.getMessage());
      LogChannel.GENERAL.logDebug("Warning no documentation : " + name);
    }

    return writer.toString();
  }

  private static String findDescription(String name) {
    String doc = getHtml(name);

    if (doc == null)
      return "";

    int beginIndex = doc.indexOf("id=\"preamble\"");
    beginIndex = doc.indexOf("<p>", beginIndex);

    if (beginIndex > 0) {
      int endIndex = doc.indexOf("</p>", beginIndex);

      return doc.substring(beginIndex + 3, endIndex);
    }

    return "";
  }
}
