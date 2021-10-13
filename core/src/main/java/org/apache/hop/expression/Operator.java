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
import org.apache.hop.expression.operator.Add;
import org.apache.hop.expression.operator.Between;
import org.apache.hop.expression.operator.BitAnd;
import org.apache.hop.expression.operator.BitNot;
import org.apache.hop.expression.operator.BitOr;
import org.apache.hop.expression.operator.BitXor;
import org.apache.hop.expression.operator.BoolAnd;
import org.apache.hop.expression.operator.BoolNot;
import org.apache.hop.expression.operator.BoolOr;
import org.apache.hop.expression.operator.BoolXor;
import org.apache.hop.expression.operator.Case;
import org.apache.hop.expression.operator.Cast;
import org.apache.hop.expression.operator.Concat;
import org.apache.hop.expression.operator.Divide;
import org.apache.hop.expression.operator.Equal;
import org.apache.hop.expression.operator.Extract;
import org.apache.hop.expression.operator.GreaterThan;
import org.apache.hop.expression.operator.GreaterThanOrEqual;
import org.apache.hop.expression.operator.ILike;
import org.apache.hop.expression.operator.In;
import org.apache.hop.expression.operator.Is;
import org.apache.hop.expression.operator.LessThan;
import org.apache.hop.expression.operator.LessThanOrEqual;
import org.apache.hop.expression.operator.LessThanOrGreaterThan;
import org.apache.hop.expression.operator.Like;
import org.apache.hop.expression.operator.Mod;
import org.apache.hop.expression.operator.Multiply;
import org.apache.hop.expression.operator.Negative;
import org.apache.hop.expression.operator.NotEqual;
import org.apache.hop.expression.operator.Subtract;
import org.apache.hop.expression.operator.TryCast;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.i18n.BaseMessages;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Operators have the precedence levels. An operator on higher levels is evaluated before an
 * operator on a lower level
 *
 * @author Nicolas ADMENT
 */
public abstract class Operator implements Comparable<Operator> {

  protected static final Class<?> PKG = Operator.class; // for i18n purposes

  private static final ConcurrentHashMap<String, String> docs = new ConcurrentHashMap<>();
  
  // -------------------------------------------------------------
  // BITWISE OPERATORS
  // -------------------------------------------------------------
  public static final Operator BITAND = new BitAnd();
  public static final Operator BITOR = new BitOr();
  public static final Operator BITNOT = new BitNot();
  public static final Operator BITXOR = new BitXor();

  // -------------------------------------------------------------
  // LOGICAL OPERATORS
  // -------------------------------------------------------------
  public static final Operator BOOLNOT = new BoolNot();
  public static final Operator BOOLOR = new BoolOr();
  public static final Operator BOOLAND = new BoolAnd();
  public static final Operator BOOLXOR = new BoolXor();

  // -------------------------------------------------------------
  // COMPARISON OPERATORS
  // -------------------------------------------------------------
  public static final Operator IS = new Is();
  public static final Operator IN = new In();
  public static final Operator LIKE = new Like();
  public static final Operator ILIKE = new ILike();
  public static final Operator BETWEEN = new Between();
  public static final Operator EQUAL = new Equal();
  public static final Operator NOT_EQUAL = new NotEqual();
  public static final Operator LESS_THAN_OR_GREATER_THAN = new LessThanOrGreaterThan();
  public static final Operator LESS_THAN = new LessThan();
  public static final Operator LESS_THAN_OR_EQUAL = new LessThanOrEqual();
  public static final Operator GREATER_THAN = new GreaterThan();
  public static final Operator GREATER_THAN_OR_EQUAL = new GreaterThanOrEqual();

  // -------------------------------------------------------------
  // ARITHMETIC OPERATORS
  // -------------------------------------------------------------
  public static final Operator NEGATIVE = new Negative();
  public static final Operator MULTIPLY = new Multiply();
  public static final Operator DIVIDE = new Divide();
  public static final Operator MODULUS = new Mod();
  public static final Operator ADD = new Add();
  public static final Operator SUBTRACT = new Subtract();

  // -------------------------------------------------------------
  // SPECIAL OPERATORS
  // -------------------------------------------------------------

  public static final Operator CAST = new Cast();
  public static final Operator TRY_CAST = new TryCast();
  public static final Operator CASE = new Case();
  public static final Operator CONCAT = new Concat();
  public static final Operator EXTRACT = new Extract();

  // -------------------------------------------------------------
  // Hidden operator available as function but used by optimizer
  // -------------------------------------------------------------

  // public static final Operator STARTSWITH =
  // new Operator(Kind.STARTSWITH, "STARTSWITH", 10, true, "i18n::Operator.Category.Comparison");
  // public static final Operator ENDSWITH =
  // new Operator(Kind.ENDSWITH, "ENDSWITH", 10, true, "i18n::Operator.Category.Comparison");
  // public static final Operator CONTAINS =
  // new Operator(Kind.CONTAINS, "CONTAINS", 10, true, "i18n::Operator.Category.Comparison");

  /** The name of the operator/function. Ex. "COS" or "TRIM" */
  private final String name;

  private final String alias;

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
   * Creates an function operator.
   *
   * @param name Name of function
   * @param alias Alias of function
   */
  protected Operator(String name, String alias, boolean isDeterministic, String category) {
    // Some operator has syntax of function CAST, TRY_CAST, CONCAT, CONTAINS, EXTRACT, STARTSWITH,
    // ENDSWITH
    this.name = name;
    this.alias = alias;
    this.leftPrecedence = 11;
    this.rightPrecedence = 10;
    this.isDeterministic = isDeterministic;
    this.category = TranslateUtil.translate(category, Operator.class);
    this.description = findDescription(name);
  }

  /**
   * Creates an operator specifying precedence and associativity.
   *
   * @param kind Kind of operator
   * @param name Name of operator
   * @param precedence precedence
   * @param leftAssociativity left associativity
   */
  protected Operator(String name, int precedence, boolean leftAssociativity,
      String category) {
    this(name, name,  precedence, leftAssociativity, category);    
  }

  protected Operator(String name, String alias, int precedence, boolean leftAssociativity,
      String category) {
    this.name = name;
    this.alias = alias;
    this.leftPrecedence = leftPrec(precedence, leftAssociativity);
    this.rightPrecedence = rightPrec(precedence, leftAssociativity);
    this.isDeterministic = true;
    this.category = TranslateUtil.translate(category, Operator.class);
    this.description = findDescription(name);
  }
  
  protected static int leftPrec(int precedence, boolean leftAssociativity) {
    assert (precedence % 2) == 0;
    if (leftAssociativity) {
      ++precedence;
    }
    return precedence;
  }

  protected static int rightPrec(int precedence, boolean leftAssociativity) {
    assert (precedence % 2) == 0;
    if (!leftAssociativity) {
      ++precedence;
    }
    return precedence;
  }

  /** The unique name of the operator/function */
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
  
  protected URL getUrl(Kind kind) {
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

    // Compare kind if same type
    if (this.getClass().equals(o.getClass()))
      return this.name.compareTo(o.name);

    // Operator first and function last
    return (o instanceof Operator) ? 1 : 0;
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
    if (left instanceof Instant || right instanceof Instant) {
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
  public static Object convertTo(Object value, final Type type) {

    if (Type.fromJava(value) == type)
      return value;

    switch (type) {
      case BOOLEAN:
        return coerceToBoolean(value);
      case INTEGER:
        return coerceToInteger(value);
      case NUMBER:
        return coerceToNumber(value);
      case BIGNUMBER:
        return coerceToBigNumber(value);
      case STRING:
        if (value instanceof Instant) {
          ZonedDateTime dt = ZonedDateTime.ofInstant((Instant) value, ZoneId.of("UTC"));
          DateTimeFormat format = DateTimeFormat.ofPattern("YYYY-MM-DD");
          return format.format(dt);
        }
        return coerceToString(value);
      case DATE:
        return coerceToDate(value);
      case BINARY:
        return coerceToBinary(value);
      case NONE:
        return null;
      default:
        throw errorUnsupportedConversion(value, type);
    }
  }

  public static Object convertTo(Object value, final Type type, String pattern) {

    Type sourceType = Type.fromJava(value);
    if (sourceType == type)
      return value;

    switch (type) {
      case BOOLEAN:
        return coerceToBoolean(value);
      case INTEGER:
        return coerceToInteger(value);
      case NUMBER:
        return coerceToNumber(value);
      case BIGNUMBER:
        return coerceToBigNumber(value);
      case STRING:
        if (value instanceof Number) {
          return NumberFormat.ofPattern(pattern).format(coerceToBigNumber(value));
        } else if (value instanceof Instant) {
          ZonedDateTime dt = ZonedDateTime.ofInstant((Instant) value, ZoneId.of("UTC"));
          DateTimeFormat formatter = DateTimeFormat.ofPattern(pattern);
          return formatter.format(dt);
        }
        return coerceToString(value);
      case DATE:
        if (sourceType == Type.STRING) {
          try {
            DateTimeFormat format = DateTimeFormat.ofPattern(pattern);
            return format.parse((String) value);
          } catch (RuntimeException | ParseException e) {
            throw new ExpressionException(
                BaseMessages.getString(PKG, "Expression.InvalidDate", value));
          }
        }

        return coerceToDate(value);
      case BINARY:
        return coerceToBinary(value);
      case NONE:
        return null;
      default:
        throw errorUnsupportedConversion(value, type);
    }
  }


  public static Boolean coerceToBoolean(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof Boolean) {
      return (Boolean) value;
    }
    if (value instanceof Number) {
      return ((Number) value).doubleValue() != 0;
    }
    if (value instanceof String) {
      String str = (String) value;
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

    }
    throw new ExpressionException(BaseMessages.getString(PKG, "Expression.InvalidBoolean", value));
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
    if (value instanceof Long) {
      Long number = (Long) value;
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

    throw errorUnsupportedConversion(value, Type.BINARY);
  }

  public static String coerceToString(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof String) {
      return (String) value;
    }
    if (value instanceof Boolean) {
      return ((Boolean) value) ? "TRUE" : "FALSE";
    }
    if (value instanceof BigDecimal) {
      final String s = ((BigDecimal) value).toString();

      if (s.startsWith("0.")) {
        // we want ".1" not "0.1"
        return s.substring(1);
      } else if (s.startsWith("-0.")) {
        // we want "-.1" not "-0.1"
        return "-" + s.substring(2);
      } else {
        return s;
      }
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
      try {
        BigDecimal number = NumberFormat.parse((String) value, 38, 0);
        return number.longValue();
      } catch (ParseException | NumberFormatException e) {
        throw errorInvalidNumber(value);
      }
    }

    if (value instanceof Boolean) {
      return ((Boolean) value) ? 1L : 0L;
    }

    if (value instanceof byte[]) {
      byte[] b = (byte[]) value;
      if (b.length > 8)
        throw new ExpressionException("Binary too big to fit in integer");

      long result = 0;

      for (int i = 0; i < b.length; i++) {
        result = result << 8;
        result = result | (b[i] & 0xFF);
      }

      return result;
    }

    throw errorUnsupportedConversion(value, Type.INTEGER);
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
      try {
        return Double.parseDouble((String) value);
      } catch (NumberFormatException e) {
        throw errorInvalidNumber(value);
      }
    }
    // if (value instanceof Boolean) {
    // return ((Boolean) value) ? 1D:0D;
    // }

    if (value instanceof byte[]) {
      byte[] b = (byte[]) value;
      if (b.length > 8)
        throw new ExpressionException("Binary too big to fit in number");

      long result = 0;

      for (int i = 0; i < b.length; i++) {
        result = result << 8;
        result = result | (b[i] & 0xFF);
      }

      return Double.valueOf(result);
    }

    throw errorUnsupportedConversion(value, Type.NUMBER);
  }

  public static BigDecimal coerceToBigNumber(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof BigDecimal) {
      return (BigDecimal) value;
    }
    if (value instanceof Long) {
      return BigDecimal.valueOf((Long) value);
    }
    if (value instanceof Double) {
      return BigDecimal.valueOf((Double) value);
    }
    if (value instanceof String) {
      try {
        return new BigDecimal(((String) value).trim());
      } catch (NumberFormatException e) {
        throw errorInvalidNumber(value);
      }
    }
    // if (value instanceof Boolean) {
    // return ((Boolean) value) ? BigDecimal.ONE:BigDecimal.ZERO;
    // }

    throw errorUnsupportedConversion(value, Type.BIGNUMBER);
  }

  public static Instant coerceToDate(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof Instant) {
      return (Instant) value;
    }

    throw errorUnsupportedConversion(value, Type.DATE);
  }

  /** Translates a LIKE pattern to Java regex pattern, with optional escape string. */
  protected static String toRegexLike(String sqlPattern, CharSequence escapeStr) {
    final char escapeChar;
    if (escapeStr != null) {

      if (escapeStr.length() != 1) {
        throw errorInvalidEscapeCharacter(escapeStr.toString());
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
          throw errorInvalidEscapeSequence(sqlPattern, i);
        }
        char nextChar = sqlPattern.charAt(i + 1);
        if ((nextChar == '_') || (nextChar == '%') || (nextChar == escapeChar)) {
          if (JAVA_REGEX_SPECIALS.indexOf(nextChar) >= 0) {
            javaPattern.append('\\');
          }
          javaPattern.append(nextChar);
          i++;
        } else {
          throw errorInvalidEscapeSequence(sqlPattern, i);
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

  protected static final ExpressionException errorArgumentOutOfRange(Object arg) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Expression.ArgumentOutOfRange", arg));
  }

  protected static final ExpressionException errorInternal(final String error) {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.InternalError", error));
  }

  protected static ExpressionException errorInvalidEscapeCharacter(String s) {
    return new ExpressionException("Invalid escape character '" + s + "'");
  }

  protected static ExpressionException errorInvalidEscapeSequence(String s, int i) {
    return new ExpressionException("Invalid escape sequence '" + s + "', " + i);
  }

  protected static final ExpressionException errorInvalidNumber(Object value) {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.InvalidNumber", value));
  }

  protected static final ExpressionException errorOverflow(String message) {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.Overflow", message));
  }

  protected static final ExpressionException errorDivisionByZero() {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.DivisionByZero"));
  }

  protected static final ExpressionException errorUnsupportedConversion(Object value, Type type) {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.UnsupportedConversion",
        value, Type.fromJava(value), type));
  }

  protected static ExpressionException createFormatPatternException(String s, int i) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Bad format {0} at position {1}", s, i));
  }

  protected static ExpressionException createRegexpPatternException(String s) {
    return new ExpressionException(BaseMessages.getString(PKG, "Bad regexp {0}", s));
  }

  protected static ExpressionException createUnexpectedDataTypeException(Operator operator,
      Type type) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Expression.UnexpectedDataType", operator.getName(), type));
  }

  protected static ExpressionException createUnexpectedDatePartException(Operator operator,
      DatePart part) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Expression.UnexpectedDatePart", operator.getName(), part));
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
