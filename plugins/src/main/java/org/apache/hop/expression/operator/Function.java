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
package org.apache.hop.expression.operator;

import org.apache.commons.codec.EncoderException;
import org.apache.commons.codec.binary.Hex;
import org.apache.commons.codec.language.Soundex;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.DatePart;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.ScalarFunction;
import org.apache.hop.expression.Type;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.i18n.BaseMessages;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.ParseException;
import java.time.DayOfWeek;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.TextStyle;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.IsoFields;
import java.time.temporal.TemporalAdjuster;
import java.time.temporal.TemporalAdjusters;
import java.util.List;
import java.util.Locale;
import java.util.Random;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/** A <code>Function</code> is a type of operator which has conventional function-call syntax. */

public class Function extends Operator {

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  private static final char[] HEX = "0123456789ABCDEF".toCharArray();

  /** The maximum size to which the padding can expand. */
  private static final int PAD_LIMIT = 8192;

  private static final Soundex SOUNDEX = new Soundex();

  private final Method method;
  private final int minArgs;
  private final int maxArgs;

  public Function(String name, String alias, boolean isDeterministic, Method method, int min,
      int max, String category) throws ExpressionException {
    super(name, alias, isDeterministic, category);

    this.method = method;
    this.minArgs = min;
    this.maxArgs = max;
  }

  /**
   * Check if the number of arguments is correct.
   *
   * @param len the number of arguments set
   * @throws error if not enough or too many arguments
   */
  public void checkNumberOfArguments(List<IExpression> operands) throws ExpressionException {

    if (operands.size() < minArgs) {
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Expression.NotEnoughArguments", this.getName()));
    }

    if (operands.size() > maxArgs) {
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Expression.TooManyNumberOfArguments", this.getName()));
    }
  }

  @Override
  public final Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    try {
      return method.invoke(null, context, operands);
    } catch (Exception e) {
      if ( e.getCause() instanceof ExpressionException ) {
        throw (ExpressionException) e.getCause();
      }
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Expression.FunctionError", this.getName(), e.getMessage()),
          e);
    }
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
    boolean first = true;
    for (IExpression operand : operands) {
      if (!first)
        writer.append(',');
      else
        first = false;
      operand.write(writer);
    }
    writer.append(')');
  }

  @ScalarFunction(name = "CURRENT_DATE", alias = {"NOW"}, deterministic = false, minArgs = 0,
      maxArgs = 0, category = "i18n::Operator.Category.Date")
  public static Object current_date(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    return context.getAttribute(ExpressionContext.ATTRIBUTE_CURRENT_DATE);
  }

  // -------------------------------------------------------------
  // CRYPTOGRAPHIC
  // -------------------------------------------------------------

  /**
   * The function calculate the MD5 hash of a data value. The hash will be returned as a 32
   * characters hex-encoded string.
   *
   * @see {@link #sha1}, {@link #sha256}, {@link #sha384}, {@link #sha512}
   */
  @ScalarFunction(name = "MD5", category = "i18n::Operator.Category.Cryptographic")
  public static Object md5(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    return getHash(operands[0].eval(context), "MD5");
  }

  /**
   * The function calculate the SHA-1 hash of a data value. The hash will be returned as a 40
   * characters hex-encoded string.
   *
   * @see {@link #md5}, {@link #sha256}, {@link #sha384}, {@link #sha512}
   */
  @ScalarFunction(name = "SHA1", category = "i18n::Operator.Category.Cryptographic")
  public static Object sha1(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    return getHash(operands[0].eval(context), "SHA-1");
  }

  /**
   * The function calculate the SHA-256 hash of a data value. The hash will be returned as a 64
   * characters hex-encoded string.
   *
   * @see {@link #md5}, {@link #sha1}, {@link #sha384}, {@link #sha512}
   */
  @ScalarFunction(name = "SHA256", category = "i18n::Operator.Category.Cryptographic")
  public static Object sha256(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    return getHash(operands[0].eval(context), "SHA-256");
  }

  /**
   * The function calculate the SHA-384 hash of a data value. The hash will be returned as a 96
   * characters hex-encoded string.
   *
   * @see {@link #md5}, {@link #sha1}, {@link #sha256}, {@link #sha512}
   */
  @ScalarFunction(name = "SHA384", category = "i18n::Operator.Category.Cryptographic")
  public static Object sha384(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    return getHash(operands[0].eval(context), "SHA-384");
  }

  /**
   * The function calculate the SHA-512 hash of a data value. The hash will be returned as a 128
   * characters hex-encoded string.
   *
   * @see {@link #md5}, {@link #sha1}, {@link #sha256}, {@link #sha384}
   */
  @ScalarFunction(name = "SHA512", category = "i18n::Operator.Category.Cryptographic")
  public static Object sha512(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    return getHash(operands[0].eval(context), "SHA-512");
  }

  @ScalarFunction(name = "BITGET", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Bitwise")
  public static Object bitget(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    return (coerceToInteger(v0) & (1L << coerceToInteger(v1))) != 0;
  }

  /**
   * Returns the number of PI.
   */
  @ScalarFunction(name = "PI", minArgs = 0, maxArgs = 0,
      category = "i18n::Operator.Category.Mathematical")
  public static Object pi(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    return Math.PI;
  }

  /** Returns the absolute (positive) value of the numeric value. */
  @ScalarFunction(name = "ABS", category = "i18n::Operator.Category.Mathematical")
  public static Object abs(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;

    if (value instanceof BigDecimal) {
      return ((BigDecimal) value).abs();
    }
    if (value instanceof Double) {
      return FastMath.abs((double) value);
    }
    if (value instanceof Long) {
      return FastMath.abs((long) value);
    }

    return coerceToBigNumber(value).abs();
  }

  /** Returns the sign of a number. */
  @ScalarFunction(name = "SIGN", category = "i18n::Operator.Category.Mathematical")
  public static Object sign(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;
    Double number = coerceToNumber(value);
    if (number == 0)
      return 0L;
    return (number > 0) ? 1L : -1L;
  }

  /** Function to converts radians to degrees. */
  @ScalarFunction(name = "DEGREES", category = "i18n::Operator.Category.Mathematical")
  public static Object degrees(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.toDegrees(coerceToNumber(value));
  }

  /** The function converts degrees to radians. */
  @ScalarFunction(name = "RADIANS", category = "i18n::Operator.Category.Mathematical")
  public static Object radians(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.toRadians(coerceToNumber(value));
  }

  /** Returns the exponential value of a numeric expression. */
  @ScalarFunction(name = "EXP", category = "i18n::Operator.Category.Mathematical")
  public static Object exp(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.exp(coerceToNumber(value));
  }

  /** Returns the values rounded to the nearest equal or larger integer. */
  @ScalarFunction(name = "CEIL", alias = "CEILING",
      category = "i18n::Operator.Category.Mathematical")
  public static Object ceil(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    if (value instanceof Long)
      return value;
    if (value instanceof BigDecimal) {
      return coerceToBigNumber(value).setScale(0, RoundingMode.CEILING);
    }
    return FastMath.ceil(coerceToNumber(value));
  }

  /** Returns the values rounded to the nearest equal or smaller integer. */
  @ScalarFunction(name = "FLOOR", category = "i18n::Operator.Category.Mathematical")
  public static Object floor(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    if (value instanceof Long)
      return value;
    if (value instanceof BigDecimal) {
      return coerceToBigNumber(value).setScale(0, RoundingMode.FLOOR);
    }
    return FastMath.floor(coerceToNumber(value));
  }

  /** Returns the values rounded to the nearest integer. */
  @ScalarFunction(name = "ROUND", category = "i18n::Operator.Category.Mathematical")
  public static Object round(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;
    return FastMath.round(coerceToNumber(value));
  }

  @ScalarFunction(name = "RANDOM", alias = "RAND", deterministic = false, minArgs = 0, maxArgs = 1,
      category = "i18n::Operator.Category.Mathematical")
  public static Object random(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    Random random = (Random) context.getAttribute(ExpressionContext.ATTRIBUTE_RANDOM);

    if (operands.length == 1) {
      Object value = operands[0].eval(context);
      // FIXME: What if multi random with different SEED ?
      random.setSeed(coerceToInteger(value));
    }
    return random.nextDouble();
  }

  /** Returns a randomly generated UUID (Universal Unique Identifier defined by RFC 4122) */
  @ScalarFunction(name = "UUID", minArgs = 0, maxArgs = 0, deterministic = false,
      category = "i18n::Operator.Category.String")
  public static Object uuid(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    return UUID.randomUUID().toString();
  }

  /**
   * Returns the arc cosine, the angle in radians whose cosine is the specified float expression.
   */
  @ScalarFunction(name = "ACOS", category = "i18n::Operator.Category.Trigonometry")
  public static Object acos(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Double d = coerceToNumber(value);
    if (d < -1.0 || d > 1.0) {
      throw errorArgumentOutOfRange(value);
    }
    return FastMath.acos(d);
  }

  @ScalarFunction(name = "ACOSH", category = "i18n::Operator.Category.Trigonometry")
  public static Object acosh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.acosh(coerceToNumber(value));
  }

  @ScalarFunction(name = "ASINH", category = "i18n::Operator.Category.Trigonometry")
  public static Object asinh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    return FastMath.asinh(coerceToNumber(value));
  }

  @ScalarFunction(name = "ATAN", category = "i18n::Operator.Category.Trigonometry")
  public static Object atan(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    return FastMath.atan(coerceToNumber(value));
  }

  @ScalarFunction(name = "ATANH", category = "i18n::Operator.Category.Trigonometry")
  public static Object atanh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    return FastMath.atanh(coerceToNumber(value));
  }

  @ScalarFunction(name = "ATAN2", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Trigonometry")
  public static Object atan2(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    return FastMath.atan2(coerceToNumber(v0), coerceToNumber(v1));
  }

  /** Returns the trigonometric cosine of the specified angle in radians in the specified number. */
  @ScalarFunction(name = "COS", category = "i18n::Operator.Category.Trigonometry")
  public static Object cos(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.cos(coerceToNumber(value));
  }

  /** Returns the trigonometric cosine of the specified angle in radians in the specified number. */
  @ScalarFunction(name = "COSH", category = "i18n::Operator.Category.Trigonometry")
  public static Object cosh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.cosh(coerceToNumber(value));
  }

  /** Returns the trigonometric cotangent of the angle in radians specified by float expression. */
  @ScalarFunction(name = "COT", category = "i18n::Operator.Category.Trigonometry")
  public static Object cot(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    double number = coerceToNumber(value);
    if (number == 0)
      throw errorArgumentOutOfRange(value);

    return FastMath.cos(number) / FastMath.sin(number);
  }

  @ScalarFunction(name = "ASIN", category = "i18n::Operator.Category.Trigonometry")
  public static Object asin(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.asin(coerceToNumber(value));
  }

  /**
   * Calculates the trigonometric sine of the angle in radians.
   */
  @ScalarFunction(name = "SIN", category = "i18n::Operator.Category.Trigonometry")
  public static Object sin(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;

    return FastMath.sin(coerceToNumber(value));
  }

  /**
   * Calculates the hyperbolic sine of its argument.
   */
  @ScalarFunction(name = "SINH", category = "i18n::Operator.Category.Trigonometry")
  public static Object sinh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.sinh(coerceToNumber(value));
  }

  /**
   * Calculates the tangent of its argument, the argument should be expressed in radians.
   */
  @ScalarFunction(name = "TAN", category = "i18n::Operator.Category.Trigonometry")
  public static Object tan(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.tan(coerceToNumber(value));
  }

  /**
   * Calculates the hyperbolic tangent of its argument.
   */
  @ScalarFunction(name = "TANH", category = "i18n::Operator.Category.Trigonometry")
  public static Object tanh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.tanh(coerceToNumber(value));
  }

  /**
   * Returns the natural logarithm of a numeric value.
   */
  @ScalarFunction(name = "LN", category = "i18n::Operator.Category.Trigonometry")
  public static Object ln(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Double number = coerceToNumber(value);
    if (number <= 0)
      throw errorArgumentOutOfRange(value);
    return FastMath.log(number);
  }

  /**
   * Returns the specified base logarithm of a numeric value.
   */
  @ScalarFunction(name = "LOG", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Trigonometry")
  public static Object log(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object base = operands[0].eval(context);
    if (base == null)
      return null;

    Object value = operands[1].eval(context);
    if (value == null)
      return null;
    Double number = coerceToNumber(value);
    if (number <= 0)
      throw errorArgumentOutOfRange(value);

    return FastMath.log(number) / FastMath.log(coerceToNumber(base));
  }

  /**
   * Returns the base 10 logarithm of a numeric value.
   */
  @ScalarFunction(name = "LOG10", category = "i18n::Operator.Category.Trigonometry")
  public static Object log10(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Double number = coerceToNumber(value);
    if (number <= 0)
      throw errorArgumentOutOfRange(value);
    return FastMath.log10(number);
  }

  /**
   * Returns the cubic root of a numeric expression.
   * 
   * @See {@link #SQRT}
   */
  @ScalarFunction(name = "CBRT", category = "i18n::Operator.Category.Mathematical")
  public static Object cbrt(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.cbrt(coerceToNumber(value));
  }

  /**
   * Returns the square-root of a non-negative numeric expression.
   * 
   * @See {@link #CBRT}
   */
  @ScalarFunction(name = "SQRT", category = "i18n::Operator.Category.Mathematical")
  public static Object sqrt(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Double number = coerceToNumber(value);
    if (number < 0)
      throw errorArgumentOutOfRange(value);
    return FastMath.sqrt(number);
  }

  /**
   * Returns the square of a numeric expression.
   * 
   */
  @ScalarFunction(name = "SQUARE", category = "i18n::Operator.Category.Mathematical")
  public static Object square(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Double number = coerceToNumber(value);
    return FastMath.pow(number, 2);
  }

  @ScalarFunction(name = "POWER", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public static Object power(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object left = operands[0].eval(context);
    Object right = operands[1].eval(context);
    if (left == null || right == null) {
      return null;
    }
    Double power = coerceToNumber(right);
    if (power == 0)
      return 1L;
    if (power < 0)
      throw new ArithmeticException("Cannot power negative " + power);

    return FastMath.pow(coerceToNumber(left), coerceToNumber(right));
  }

  private static Object getHash(Object value, String algorithm) {
    if (value == null)
      return null;

    try {
      MessageDigest md = MessageDigest.getInstance(algorithm);
      md.update(coerceToBinary(value));
      return Hex.encodeHexString(md.digest());
    } catch (NoSuchAlgorithmException e) {
      throw new ExpressionException("Unknow algorithm: " + algorithm);
    }
  }

  @ScalarFunction(name = "TRANSLATE", minArgs = 3, maxArgs = 3,
      category = "i18n::Operator.Category.String")
  public static Object translate(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;
    Object v2 = operands[2].eval(context);
    if (v2 == null)
      return null;


    String string = coerceToString(v0);
    String findChars = coerceToString(v1);
    String replaceChars = coerceToString(v2);

    // if it stays null, then no replacements have been made
    StringBuilder buffer = null;
    // if shorter than findChars, then characters are removed
    // (if null, we don't access replaceChars at all)
    if (replaceChars == null)
      replaceChars = "";
    int replaceSize = replaceChars.length();

    for (int i = 0, size = string.length(); i < size; i++) {
      char ch = string.charAt(i);
      int index = findChars.indexOf(ch);
      if (index >= 0) {
        if (buffer == null) {
          buffer = new StringBuilder(size);
          if (i > 0) {
            buffer.append(string, 0, i);
          }
        }
        if (index < replaceSize) {
          ch = replaceChars.charAt(index);
        }
      }
      if (buffer != null) {
        buffer.append(ch);
      }
    }
    return buffer == null ? string : buffer.toString();
  }

  /**
   * The function return the ASCII value of the first character in a string. If the string is empty,
   * a value of 0 is returned.
   */
  @ScalarFunction(name = "ASCII", category = "i18n::Operator.Category.String")
  public static Object ascii(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    String string = value.toString();
    int ascii = 0;
    if (string.length() > 0) {
      ascii = string.charAt(0);
    }
    return Long.valueOf(ascii);
  }

  /**
   * The function converts a Unicode code point (including 7-bit ASCII) into the character that
   * matches the input Unicode. If an invalid code point is specified, an error is returned.
   *
   * @see {@link #ascii}
   */
  @ScalarFunction(name = "CHR", category = "i18n::Operator.Category.String")
  public static Object chr(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    int codePoint = coerceToInteger(value).intValue();

    if (!Character.isValidCodePoint(codePoint)) {
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Expression.ArgumentOutOfRange", codePoint));
    }
    return new String(Character.toChars(codePoint));
  }

  /**
   * Returns a string consisting of a the specified number of blank spaces.
   */
  @ScalarFunction(name = "SPACE", category = "i18n::Operator.Category.String")
  public static Object space(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    int length = Math.max(0, coerceToInteger(value).intValue());
    char[] chars = new char[length];
    for (int i = length - 1; i >= 0; i--) {
      chars[i] = ' ';
    }

    return new String(chars);
  }

  /**
   * The function reverses the order of characters in a string value, or of bytes in a binary value.
   */
  @ScalarFunction(name = "REVERSE", category = "i18n::Operator.Category.String")
  public static Object reverse(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    if (value instanceof byte[]) {
      byte[] data = coerceToBinary(value);
      byte[] result = new byte[data.length];
      for (int i = data.length - 1, j = 0; i >= 0; i--, j++) {
        result[j] = data[i];
      }
      return result;
    }

    StringBuilder builder = new StringBuilder(value.toString()).reverse();
    return builder.toString();
  }

  /**
   * Returns a string that contains a phonetic representation of the input string.
   */
  @ScalarFunction(name = "SOUNDEX", category = "i18n::Operator.Category.String")
  public static Object soundex(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return SOUNDEX.soundex(coerceToString(value));
  }

  @ScalarFunction(name = "DIFFERENCE", category = "i18n::Operator.Category.String", minArgs = 2,
      maxArgs = 2)
  public static Object difference(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    try {
      return Long.valueOf(SOUNDEX.difference(v0.toString(), v1.toString()));
    } catch (EncoderException e) {
      return null;
    }
  }

  /**
   * The function return the Unicode code point for the first Unicode character in a string. If the
   * string is empty, a value of 0 is returned.
   *
   * @see {@link #CHR}, {@link #ASCII},
   */
  @ScalarFunction(name = "UNICODE", category = "i18n::Operator.Category.String")
  public static Object unicode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    String string = value.toString();
    int codePoint = 0;
    if (string.length() > 0) {
      codePoint = string.codePointAt(0);
    }
    return Long.valueOf(codePoint);
  }

  /**
   * The function left-pads a string with another string, to a certain length.
   *
   * @see {@link #RAPD}
   */
  @ScalarFunction(name = "LPAD", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String")
  public static Object lpad(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    String str = v0.toString();

    Object v1 = operands[1].eval(context);
    int length = coerceToInteger(v1).intValue();

    // If this parameter is omitted, the function will pad spaces
    String pad = null;
    if (operands.length == 3) {
      Object v2 = operands[2].eval(context);
      pad = v2.toString();
    }

    if (length < 0) {
      length = 0;
    } else if (length > PAD_LIMIT) {
      throw new IllegalArgumentException("Paddind length exceeds maximum limit: " + PAD_LIMIT);
    }

    // If this parameter is omitted, the function will pad spaces
    if (pad == null) {
      pad = " ";
    }

    final int size = pad.length();
    final int index = length - str.length();

    if (index <= 0) {
      str = str.substring(0, length);
    } else if (size == 0) {
      // nothing to do
    } else if (index == size) {
      str = pad.concat(str);
    } else if (index < size) {
      str = pad.substring(0, index).concat(str);
    } else {
      final char[] padding = new char[index];
      final char[] padChars = pad.toCharArray();
      for (int i = 0; i < index; i++) {
        padding[i] = padChars[i % size];
      }
      str = new String(padding).concat(str);
    }

    return str;
  }

  /**
   * The function right-pads a string with another string, to a certain length.
   *
   * @see {@link #LPAD}
   */
  @ScalarFunction(name = "RPAD", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String")
  public static Object rpad(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    String str = v0.toString();

    Object v1 = operands[1].eval(context);
    int length = coerceToInteger(v1).intValue();

    // If this parameter is omitted, the function will pad spaces
    String pad = null;
    if (operands.length == 3) {
      Object v2 = operands[2].eval(context);
      pad = v2.toString();
    }

    if (length < 0) {
      length = 0;
    }
    if (length > PAD_LIMIT) {
      throw new IllegalArgumentException("Paddind length exceeds maximum limit: " + PAD_LIMIT);
    }

    // If this parameter is omitted, the function will pad spaces
    if (pad == null) {
      pad = " ";
    }

    final int size = pad.length();
    final int index = length - str.length();

    if (index <= 0) {
      str = str.substring(0, length);
    } else if (size == 0) {
      // nothing to do
    } else if (index == size) {
      str = str.concat(pad);
    } else if (index < size) {
      str = str.concat(pad.substring(0, index));
    } else {
      final char[] padding = new char[index];
      final char[] padChars = pad.toCharArray();
      for (int i = 0; i < index; i++) {
        padding[i] = padChars[i % size];
      }
      str = str.concat(new String(padding));
    }

    return str;
  }


  private static int parseRegexpFlags(String str) {
    int flags = Pattern.UNICODE_CASE;
    if (str != null) {
      for (int i = 0; i < str.length(); ++i) {
        switch (str.charAt(i)) {
          // Enables case-insensitive matching
          case 'i':
            flags |= Pattern.CASE_INSENSITIVE;
            break;
          // Enables case-sensitive matching
          case 'c':
            flags &= ~Pattern.CASE_INSENSITIVE;
            break;
          case 'n':
            flags |= Pattern.DOTALL;
            break;
          case 'm':
            flags |= Pattern.MULTILINE;
            break;
          default:
            throw new ExpressionException("Invalid input for Regexp");
        }
      }
    }
    return flags;
  }

  /**
   * The function returns the number of characters of the specified string.
   */
  @ScalarFunction(name = "LENGTH", category = "i18n::Operator.Category.String")
  public static Object length(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;
    return Long.valueOf(coerceToString(value).length());
  }

  /**
   * The function convert a string value to lower case.
   * 
   * @See {@link #INITCAP}, {@link #UPPER}
   */
  @ScalarFunction(name = "LOWER", alias = "LCASE", category = "i18n::Operator.Category.String")
  public static Object lower(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return coerceToString(value).toLowerCase(Locale.getDefault());
  }

  /**
   * The function convert a string value to upper case.
   * 
   * @See {@link #LOWER}, {@link #INITCAP}
   */
  @ScalarFunction(name = "UPPER", alias = "UCASE", category = "i18n::Operator.Category.String")
  public static Object upper(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return coerceToString(value).toUpperCase(Locale.getDefault());
  }

  /**
   * Returns a string with the first letter of each word in uppercase and the subsequent letters in
   * lowercase. @See {@link #LOWER}, {@link #UPPER}
   */
  @ScalarFunction(name = "INITCAP", category = "i18n::Operator.Category.String")
  public static Object initcap(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    String str = value.toString();
    int length = str.length();
    StringBuilder builder = new StringBuilder(length);
    boolean capitalizeNext = true;
    for (int i = 0; i < length; i++) {
      char ch = str.charAt(i);

      if (Characters.isWordDelimiter(ch)) {
        builder.append(ch);
        capitalizeNext = true;
      } else if (capitalizeNext) {
        builder.append(Character.toTitleCase(ch));
        capitalizeNext = false;
      } else {
        builder.append(Character.toLowerCase(ch));
      }
    }
    return builder.toString();
  }

  /**
   * The function removes leading and trailing characters from a string.
   *
   * @see {@link #ltrim}, {@link #rtrim}
   */
  @ScalarFunction(name = "TRIM", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Object trim(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    String string = value.toString();
    String characters = null;

    if (operands.length == 2) {
      Object stripChars = operands[1].eval(context);
      if (stripChars == null)
        return null;

      characters = stripChars.toString();
    }

    return StringUtils.strip(string, characters);
  }

  /**
   * The function removes leading characters from a string.
   *
   * @see {@link #trim}, {@link #rtrim}
   */
  @ScalarFunction(name = "LTRIM", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Object ltrim(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    String string = value.toString();
    String characters = null;

    if (operands.length == 2) {
      Object stripChars = operands[1].eval(context);
      if (stripChars == null)
        return null;
      characters = stripChars.toString();
    }

    return StringUtils.stripStart(string, characters);
  }

  /**
   * The function removes leading characters from a string.
   *
   * @see {@link #trim}, {@link #ltrim}
   */
  @ScalarFunction(name = "RTRIM", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Object rtrim(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    String string = value.toString();
    String characters = null;

    if (operands.length == 2) {
      Object stripChars = operands[1].eval(context);
      if (stripChars == null)
        return null;
      characters = stripChars.toString();
    }

    return StringUtils.stripEnd(string, characters);
  }

  /**
   * The function extracts a number of characters from a string (starting from left).
   * 
   * @See {@link #right}
   */
  @ScalarFunction(name = "LEFT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Object left(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    String s = v0.toString();

    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;
    int count = coerceToInteger(v1).intValue();

    if (count < 0) {
      count = 0;
    } else if (count > s.length()) {
      count = s.length();
    }
    return s.substring(0, count);
  }

  /**
   * The function extracts a number of characters from a string (starting from right)
   * 
   * @See {@link #left}
   */
  @ScalarFunction(name = "RIGHT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Object right(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    String s = v0.toString();

    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;
    int count = coerceToInteger(v1).intValue();


    if (count < 0) {
      count = 0;
    } else if (count > s.length()) {
      count = s.length();
    }
    return s.substring(s.length() - count);
  }

  /**
   * Encodes special characters in a string using the Java string literal encoding format.
   */
  @ScalarFunction(name = "STRINGENCODE", category = "i18n::Operator.Category.String")
  public static Object stringEncode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    String s = value.toString();

    StringBuilder builder = new StringBuilder(s.length());
    int length = s.length();
    for (int i = 0; i < length; i++) {
      char c = s.charAt(i);
      switch (c) {
        case '\t':
          // Escape horizontal tab
          builder.append("\\t");
          break;
        case '\n':
          // Escape linefeed
          builder.append("\\n");
          break;
        case '\f':
          // Escape form feed
          builder.append("\\f");
          break;
        case '\r':
          // Escape carriage return
          builder.append("\\r");
          break;
        case '\b':
          // Escape backspace
          builder.append("\\b");
          break;
        case '"':
          // Escape double quote
          builder.append("\\\"");
          break;
        case '\'':
          builder.append('\'');
          break;
        case '\\':
          // Escape backslash
          builder.append("\\\\");
          break;
        default:
          if (c >= ' ' && (c < 0x80)) {
            builder.append(c);
          } else {
            builder.append("\\u").append(HEX[c >>> 12]).append(HEX[c >>> 8 & 0xf])
                .append(HEX[c >>> 4 & 0xf]).append(HEX[c & 0xf]);
          }
      }
    }

    return builder.toString();
  }

  /**
   * Converts an encoded string using the Java string literal encoding format. Special characters
   * are \b, \t, \n, \f, \r, \", \, \\uXXXX.
   */
  @ScalarFunction(name = "STRINGDECODE", category = "i18n::Operator.Category.String")
  public static Object stringDecode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    String s = value.toString();

    int length = s.length();
    StringBuilder builder = new StringBuilder(length);
    for (int i = 0; i < length; i++) {
      char c = s.charAt(i);
      if (c == '\\') {
        if (i + 1 >= s.length()) {
          throw createFormatPatternException(s, i);
        }
        c = s.charAt(++i);
        switch (c) {
          case 't':
            builder.append('\t');
            break;
          case 'r':
            builder.append('\r');
            break;
          case 'n':
            builder.append('\n');
            break;
          case 'b':
            builder.append('\b');
            break;
          case 'f':
            builder.append('\f');
            break;
          case '#':
            // for properties files
            builder.append('#');
            break;
          case '=':
            // for properties files
            builder.append('=');
            break;
          case ':':
            // for properties files
            builder.append(':');
            break;
          case '"':
            builder.append('"');
            break;
          case '\\':
            builder.append('\\');
            break;
          case 'u':
            // Unicode format \u0000
            try {
              c = (char) (Integer.parseInt(s.substring(i + 1, i + 5), 16));
            } catch (NumberFormatException e) {
              throw createFormatPatternException(s, i);
            }
            i += 4;
            builder.append(c);
            break;

          default:
            // Octal format \<octal>
            // if (c >= '0' && c <= '7') {
            // try {
            // c = (char) (Integer.parseInt(s.substring(i, i + 3), 8));
            // } catch (NumberFormatException e) {
            // throw createFormatException(s, i);
            // }
            // i += 2;
            // builder.append(c);
            // }

            throw createFormatPatternException(s, i);
        }
      } else {
        builder.append(c);
      }
    }

    return builder.toString();
  }

  /**
   * The function encode the string as a URL.
   *
   * @see {@link #urldecode}
   */
  @ScalarFunction(name = "URLENCODE", category = "i18n::Operator.Category.String")
  public static Object urlEncode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    try {
      return URLEncoder.encode(value.toString(), StandardCharsets.UTF_8.name());
    } catch (Exception e) {
      throw new ExpressionException(BaseMessages.getString(PKG, "Error encoding url"), e);
    }
  }

  /**
   * The function decode the URL to a string.
   *
   * @see {@link #urlencode}
   */
  @ScalarFunction(name = "URLDECODE", category = "i18n::Operator.Category.String")
  public static Object urlDecode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    try {
      return URLDecoder.decode(value.toString(), StandardCharsets.UTF_8.name());
    } catch (Exception e) {
      throw new ExpressionException(BaseMessages.getString(PKG, "Error decoding url"), e);
    }
  }

  // -------------------------------------------------------------
  // COMPARISON
  // -------------------------------------------------------------


  /**
   * Compares whether two expressions are equal.
   *
   * <p>
   * The function is NULL-safe, meaning it treats NULLs as known values for comparing equality. Note
   * that this is different from the EQUAL comparison operator (=), which treats NULLs as unknown
   * values.
   */
  @ScalarFunction(name = "EQUAL_NULL", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Comparison")
  public static Object equal_null(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    Object v1 = operands[1].eval(context);

    // Treats NULLs as known values
    if (v0 == null && v1 == null) {
      return Boolean.TRUE;
    }
    if (v0 == null || v1 == null) {
      return Boolean.FALSE;
    }

    return compareTo(v0, v1) == 0;
  }

  // -------------------------------------------------------------
  // CONDITIONAL
  // -------------------------------------------------------------

  /**
   * The function returns the smallest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #GREATEST}
   */
  @ScalarFunction(name = "LEAST", minArgs = 1, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.Conditional")
  public static Object least(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object result = null;
    for (IExpression operand : operands) {
      Object value = operand.eval(context);
      // null is always smaller
      if (value == null)
        continue;
      if (result == null || compareTo(value, result) < 0) {
        result = value;
      }
    }

    return result;
  }

  /**
   * The function returns the largest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #LEAST}
   */
  @ScalarFunction(name = "GREATEST", minArgs = 1, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.Conditional")
  public static Object greatest(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object result = null;
    for (IExpression operand : operands) {
      Object value = operand.eval(context);
      if (compareTo(result, value) < 0)
        result = value;
    }

    return result;
  }

  /**
   * Single-level if-then-else expression. Similar to CASE, but only allows a single condition.
   */
  @ScalarFunction(name = "IF", minArgs = 3, maxArgs = 3,
      category = "i18n::Operator.Category.Conditional")
  public static Object iff(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    return operands[coerceToBoolean(value) ? 1 : 2].eval(context);
  }

  /**
   * The IFNULL function replace the null with value (Alias NVL).
   */
  @ScalarFunction(name = "IFNULL", alias = "NVL", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Conditional")
  public static Object ifnull(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return operands[1].eval(context);
    return value;
  }

  /**
   * The COALESCE function returns the first of its arguments that is not null. Null is returned
   * only if all arguments are null.
   */
  @ScalarFunction(name = "COALESCE", minArgs = 1, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.Conditional")
  public static Object coalesce(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    for (IExpression operand : operands) {
      Object value = operand.eval(context);
      if (value != null)
        return value;
    }

    return null;
  }

  @ScalarFunction(name = "NVL2", minArgs = 3, maxArgs = 3,
      category = "i18n::Operator.Category.Conditional")
  public static Object nvl2(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object condition = operands[0].eval(context);

    if (condition == null) {
      return operands[2].eval(context);
    }

    return operands[1].eval(context);
  }

  /**
   * Compares the select expression to each search expression in order. As soon as a search
   * expression matches the selection expression, the corresponding result expression is returned.
   */
  @ScalarFunction(name = "DECODE", minArgs = 4, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.Conditional")
  public static Object decode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);

    int index = -1;
    for (int i = 1, len = operands.length - 1; i < len; i += 2) {
      Object search = operands[i].eval(context);
      if (compareTo(value, search) == 0) {
        index = i + 1;
        break;
      }
    }
    if (index < 0 && operands.length % 2 == 0) {
      index = operands.length - 1;
    }
    if (index < 0)
      return null;

    return operands[index].eval(context);
  }

  /** The function NULLIF */
  @ScalarFunction(name = "NULLIF", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Conditional")
  public static Object nullIf(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    Object compare = operands[1].eval(context);

    if (compareTo(value, compare) == 0)
      return null;

    return value;
  }

  /**
   * Returns 0 if its argument is null; otherwise, returns its argument.
   */
  @ScalarFunction(name = "ZEROIFNULL", minArgs = 1, maxArgs = 1,
      category = "i18n::Operator.Category.Conditional")
  public static Object zeroIfNull(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);

    if (value == null)
      return 0L;

    return value;
  }

  /**
   * Returns NULL if the argument evaluates to 0; otherwise, returns the argument.
   */
  @ScalarFunction(name = "NULLIFZERO", minArgs = 1, maxArgs = 1,
      category = "i18n::Operator.Category.Conditional")
  public static Object nullIfZero(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);

    if (coerceToInteger(value) == 0L)
      return null;

    return value;
  }

  /**
   * The function repeats a string as many times as specified.
   */
  @ScalarFunction(name = "REPEAT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Object repeat(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    String value = coerceToString(operands[0].eval(context));
    if (value == null)
      return null;
    Object number = operands[1].eval(context);
    if (number == null)
      return null;
    int count = coerceToInteger(number).intValue();

    StringBuilder builder = new StringBuilder(value.length() * count);
    while (count-- > 0) {
      builder.append(value);
    }
    return builder.toString();
  }

  /**
   * Removes all occurrences of a specified substring, and optionally replaces them with another
   * string.
   */
  @ScalarFunction(name = "REPLACE", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String")
  public static Object replace(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    String string = coerceToString(v0);
    String search = coerceToString(v1);

    if (operands.length == 3) {
      Object v2 = operands[2].eval(context);
      String replacement = coerceToString(v2);
      return string.replace(search, replacement);
    }

    return StringUtils.remove(string, search);
  }

  /**
   * Returns the position in the string that is the first character of a specified occurrence of the
   * substring.
   */
  @ScalarFunction(name = "INSTR", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String")
  public static Object instr(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    Object v1 = operands[1].eval(context);

    if (v0 == null || v1 == null) {
      return null;
    }

    String string = v0.toString();
    String pattern = v1.toString();

    // If 3 operands
    int start = 0;
    if (operands.length == 3) {
      start = coerceToInteger(operands[2].eval(context)).intValue();

      if (start > 0)
        start -= 1;
      else if (start < 0) {
        return Long.valueOf(string.lastIndexOf(pattern, string.length() + start) + 1L);
      }
    }

    return Long.valueOf(string.indexOf(pattern, start) + 1L);
  }

  /**
   * Returns the portion of the string from string, startingfrom the character/byte specified by
   * start, with optionally limited length.
   */
  @ScalarFunction(name = "SUBSTRING", alias = {"SUBSTR"}, minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String")
  public static Object substring(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    String string = coerceToString(operands[0].eval(context));
    int length = string.length();
    int start = coerceToInteger(operands[1].eval(context)).intValue();

    // These compatibility conditions violate the Standard
    if (start == 0) {
      start = 1;
    } else if (start < 0) {
      start = length + start + 1;
    }

    // Only 2 operands
    if (operands.length == 2) {
      return string.substring(start - 1);
    }

    int end = start + coerceToInteger(operands[2].eval(context)).intValue();
    // SQL Standard requires "data exception - substring error" when
    // end < start but expression does not throw it for compatibility
    start = Math.max(start, 1);
    end = Math.min(end, length + 1);
    if (start > length || end <= start) {
      // TODO: option to treatEmptyStringsAsNull
      return null;
    }

    return string.substring(start - 1, end - 1);
  }

  @ScalarFunction(name = "REGEXP_LIKE", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.Comparison")
  public static Object regexp_like(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    Object v0 = operands[0].eval(context);
    if (v0 == null) {
      return null;
    }
    String input = v0.toString();

    Object v1 = operands[1].eval(context);
    if (v1 == null) {
      return null;
    }
    String regexp = v1.toString();
    // An empty pattern matches nothing
    if (regexp.length() == 0)
      return Boolean.FALSE;

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 3) {
      Object v2 = operands[2].eval(context);
      flags = parseRegexpFlags(v2.toString());
    }

    try {
      Pattern pattern = Pattern.compile(regexp, flags);
      return pattern.matcher(input).find();
    } catch (PatternSyntaxException e) {
      throw createRegexpPatternException(regexp);
    }
  }


  @ScalarFunction(name = "REGEXP_REPLACE", minArgs = 2, maxArgs = 6,
      category = "i18n::Operator.Category.String")
  public static Object regexp_replace(final IExpressionContext context,
      final IExpression[] operands) throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null) {
      return null;
    }
    String input = v0.toString();

    Object v1 = operands[1].eval(context);
    if (v1 == null) {
      return null;
    }
    String regexp = v1.toString();

    // An empty pattern matches nothing
    if (regexp.length() == 0)
      return input;

    // Default empty string
    String replacement = "";
    if (operands.length >= 3) {
      Object v2 = operands[2].eval(context);
      if (v2 != null) {
        replacement = v2.toString();
      }
    }

    // Default position 1
    int position = 1;
    if (operands.length >= 4) {
      Object v3 = operands[3].eval(context);
      if (v3 != null) {
        position = coerceToInteger(v3).intValue();
      }
    }

    // Default occurrence 0
    int occurrence = 0;
    if (operands.length >= 5) {
      Object v4 = operands[4].eval(context);
      if (v4 != null) {
        occurrence = coerceToInteger(v4).intValue();
      }
    }

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 6) {
      Object v5 = operands[5].eval(context);
      flags = parseRegexpFlags(v5.toString());
    }

    try {

      // Back reference
      if ((replacement.indexOf('\\') >= 0) || (replacement.indexOf('$') >= 0)) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < replacement.length(); i++) {
          char c = replacement.charAt(i);
          if (c == '$') {
            sb.append('\\');
          } else if (c == '\\' && ++i < replacement.length()) {
            c = replacement.charAt(i);
            sb.append(c >= '0' && c <= '9' ? '$' : '\\');
          }
          sb.append(c);
        }
        replacement = sb.toString();
      }

      Matcher matcher =
          Pattern.compile(regexp, flags).matcher(input).region(position - 1, input.length());
      if (occurrence == 0) {
        return matcher.replaceAll(replacement);
      } else {
        StringBuffer buffer = new StringBuffer();
        int index = 1;
        while (matcher.find()) {
          if (index == occurrence) {
            matcher.appendReplacement(buffer, replacement);
            break;
          }
          index++;
        }
        matcher.appendTail(buffer);
        return buffer.toString();
      }
    } catch (PatternSyntaxException e) {
      throw createRegexpPatternException(regexp);
    } catch (StringIndexOutOfBoundsException | IllegalArgumentException e) {
      // TODO: i18n
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Error regexp replace {0}", replacement));
    }
  }

  @ScalarFunction(name = "REGEXP_SUBSTR", minArgs = 2, maxArgs = 4,
      category = "i18n::Operator.Category.String")
  public static Object regexp_substr(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null) {
      return null;
    }
    String input = v0.toString();

    Object v1 = operands[1].eval(context);
    if (v1 == null) {
      return null;
    }
    String regexp = v1.toString();
    // An empty pattern matches nothing
    if (regexp.length() == 0)
      return null;

    // Default position 1
    int position = 1;
    if (operands.length >= 3) {
      Object v2 = operands[2].eval(context);
      if (v2 != null) {
        position = coerceToInteger(v2).intValue();
      }
    }

    // Default occurrence
    int occurrence = 0;
    if (operands.length >= 4) {
      Object v3 = operands[3].eval(context);
      if (v3 != null) {
        occurrence = coerceToInteger(v3).intValue();
      }
    }

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 5) {
      Object v4 = operands[5].eval(context);
      flags = parseRegexpFlags(v4.toString());
    }

    try {
      Matcher matcher = Pattern.compile(regexp, flags).matcher(input);

      boolean found = matcher.find(position);
      for (int index = 1; index < occurrence && found; index++) {
        found = matcher.find();
      }

      if (found) {
        return matcher.group(0); // subexpression);
      }

      return null;
    } catch (PatternSyntaxException e) {
      throw createRegexpPatternException(regexp);
    }
  }


  @ScalarFunction(name = "REGEXP_INSTR", minArgs = 2, maxArgs = 6,
      category = "i18n::Operator.Category.String")
  public static Object regexp_instr(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null) {
      return null;
    }
    String input = v0.toString();

    Object v1 = operands[1].eval(context);
    if (v1 == null) {
      return null;
    }
    String regexp = v1.toString();
    // An empty pattern matches nothing
    if (regexp.length() == 0)
      return 0L;


    // Default position 1
    int position = 1;
    if (operands.length >= 3) {
      Object v2 = operands[2].eval(context);
      if (v2 != null) {
        position = coerceToInteger(v2).intValue();
      }
    }

    // Default occurrence
    int occurrence = 0;
    if (operands.length >= 4) {
      Object v3 = operands[3].eval(context);
      if (v3 != null) {
        occurrence = coerceToInteger(v3).intValue();
      }
    }

    // Return-option
    int returnOption = 0;
    if (operands.length >= 5) {
      Object v4 = operands[4].eval(context);
      if (v4 != null) {
        returnOption = coerceToInteger(v4).intValue();
      }
    }

    // Flags
    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 6) {
      Object v5 = operands[5].eval(context);
      flags = parseRegexpFlags(v5.toString());
    }

    try {
      Matcher matcher =
          Pattern.compile(regexp, flags).matcher(input).region(position - 1, input.length());

      boolean found = matcher.find(position);
      for (int index = 1; index < occurrence && found; index++) {
        found = matcher.find();
      }

      if (found) {
        return Long.valueOf(1L + ((returnOption == 0) ? matcher.start() : matcher.end()));
      }

      return 0L;
    } catch (PatternSyntaxException e) {
      throw createRegexpPatternException(regexp);
    }
  }

  /**
   * The function returns TRUE if the first value starts with second value. Both values must be data
   * type string or binary.
   *
   * @see {@link #endswith}
   */
  @ScalarFunction(name = "STARTSWITH", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Comparison")
  public static Object startsWith(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    if (v0 instanceof byte[]) {
      byte[] data = coerceToBinary(v0);
      byte[] prefix = coerceToBinary(v1);
      if (prefix.length > data.length) {
        return Boolean.TRUE;
      } else {
        int end = prefix.length;
        for (int i = 0; i < end; i++) {
          if (data[i] != prefix[i]) {
            return Boolean.FALSE;
          }
        }
      }
      return Boolean.TRUE;
    }

    return coerceToString(v0).startsWith(coerceToString(v1));
  }

  /**
   * The function returns TRUE if the first value ends with second value. Both values must be data
   * type of string or binary.
   *
   * @see {@link #startwith}
   */
  @ScalarFunction(name = "ENDSWITH", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Comparison")
  public static Object endsWith(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    if (v0 instanceof byte[]) {
      byte[] data = coerceToBinary(v0);
      byte[] suffix = coerceToBinary(v1);
      int startOffset = data.length - suffix.length;

      if (startOffset < 0) {
        return Boolean.FALSE;
      } else {
        int end = startOffset + suffix.length;
        for (int i = startOffset; i < end; i++) {
          if (data[i] != suffix[i]) {
            return Boolean.FALSE;
          }
        }
      }
      return Boolean.TRUE;
    }

    return coerceToString(v0).endsWith(coerceToString(v1));
  }


  /** Contains function */
  @ScalarFunction(name = "CONTAINS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Comparison")
  public static Object contains(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    if (v0.toString().contains(v1.toString()))
      return Boolean.TRUE;

    return Boolean.FALSE;
  }

  /**
   * Converts a string or numeric expression to a boolean value.
   */
  @ScalarFunction(name = "TO_BOOLEAN", category = "i18n::Operator.Category.Conversion")
  public static Object to_boolean(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    return convertTo(value, Type.BOOLEAN);
  }

  /**
   * Converts a string or numeric expression to a boolean value.
   */
  @ScalarFunction(name = "TRY_TO_BOOLEAN", category = "i18n::Operator.Category.Conversion")
  public static Object try_to_boolean(final IExpressionContext context,
      final IExpression[] operands) throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    try {
      return convertTo(value, Type.BOOLEAN);
    } catch (Exception e) {
      return null;
    }
  }

  /** Converts a numeric or date expression to a string value. */
  @ScalarFunction(name = "TO_CHAR", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.Conversion")
  public static Object to_char(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    String pattern = null;
    if (operands.length > 1) {
      Object v1 = operands[1].eval(context);
      if (v1 != null)
        pattern = coerceToString(v1);
    }

    switch (Type.fromJava(value)) {
      case INTEGER:
      case NUMBER:
      case BIGNUMBER:
        return NumberFormat.ofPattern(pattern).format(coerceToBigNumber(value));
      case DATE:
        ZonedDateTime datetime = coerceToDate(value).atZone(context.getZone());
        return DateTimeFormat.ofPattern(pattern).format(datetime);
      case STRING:
        return value;

      case BINARY:
      case BOOLEAN:
      case NONE:
    }

    throw createUnexpectedDataTypeException("TO_CHAR", Type.fromJava(value));
  }

  /** Converts a string expression to a number value. */
  @ScalarFunction(name = "TO_NUMBER", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.Conversion")
  public static Object to_number(final IExpressionContext context, final IExpression[] operands)
      throws ParseException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    // No format
    if (operands.length == 1) {
      return NumberFormat.ofPattern(null).parse(coerceToString(v0));
    }

    Object v1 = operands[1].eval(context);
    if (operands.length == 2) {
      return NumberFormat.ofPattern(coerceToString(v1)).parse(coerceToString(v0));
    }

    // Precision and scale
    int precision = coerceToInteger(v1).intValue();
    Object v2 = operands[2].eval(context);
    int scale = coerceToInteger(v2).intValue();
    return NumberFormat.parse(coerceToString(v0), precision, scale);
  }

  @ScalarFunction(name = "TRY_TO_NUMBER", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.Conversion")
  public static Object try_to_number(final IExpressionContext context, final IExpression[] operands)
      throws ParseException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    try {
      // No format
      if (operands.length == 1) {
        return NumberFormat.ofPattern(null).parse(value.toString());
      }

      // With format
      if (operands.length == 2) {
        Object v1 = operands[1].eval(context);
        return NumberFormat.ofPattern(coerceToString(v1)).parse(coerceToString(value));
      }

      // Precision and scale
      if (operands.length == 3) {
        Object v1 = operands[1].eval(context);
        int precision = coerceToInteger(v1).intValue();
        Object v2 = operands[2].eval(context);
        int scale = coerceToInteger(v2).intValue();

        return NumberFormat.parse(value.toString(), precision, scale);
      }
    } catch (RuntimeException e) {
      // Ignore
    }

    return null;
  }

  /** Converts a string expression to a date value. */
  @ScalarFunction(name = "TO_DATE", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.Conversion")
  public static Object to_date(final IExpressionContext context, final IExpression[] operands)
      throws ParseException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    switch (Type.fromJava(value)) {
      case DATE:
        return value;
      case STRING:
        String pattern = null;
        if (operands.length > 1) {
          Object v1 = operands[1].eval(context);
          if (v1 != null)
            pattern = coerceToString(v1);
        } else {
          pattern = (String) context.getAttribute(ExpressionContext.NLS_DATE_FORMAT);
        }
        return DateTimeFormat.ofPattern(pattern).parse(value.toString());
      default:
        throw createUnexpectedDataTypeException("TO_DATE", Type.fromJava(value));
    }
  }

  /**
   * Converts a value of one data type into another data type if the cast succeeds, otherwise
   * returns null.
   */
  @ScalarFunction(name = "TRY_TO_DATE", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.Conversion")
  public static Object try_to_date(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    switch (Type.fromJava(value)) {
      case DATE:
        return value;
      case STRING:

        String pattern = null;
        if (operands.length > 1) {
          Object v1 = operands[1].eval(context);
          if (v1 != null)
            pattern = v1.toString();
        } else {
          pattern = (String) context.getAttribute(ExpressionContext.NLS_DATE_FORMAT);
        }
        try {
          DateTimeFormat format = DateTimeFormat.ofPattern(pattern);
          return format.parse(coerceToString(value));
        } catch (ParseException | RuntimeException e) {
          // Ignore
        }
        break;
      default:
        // Try nothing
    }

    return null;
  }

  /**
   * Round down numeric expressions or truncates a date or timestamp to the specified part.
   */
  @ScalarFunction(name = "TRUNCATE", alias = "TRUNC", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public static Object truncate(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    Type type = Type.fromJava(value);

    switch (type) {
      case INTEGER:
      case NUMBER:
      case BIGNUMBER:
        BigDecimal number = coerceToBigNumber(value);
        int scale = 0;
        if (operands.length == 2) {
          Object pattern = operands[1].eval(context);
          if (pattern == null)
            return null;
          scale = coerceToInteger(pattern).intValue();
        }

        if (scale > number.scale())
          scale = number.scale();
        return number.movePointRight(scale).setScale(0, RoundingMode.DOWN).movePointLeft(scale);

      case DATE:
        Instant instant = coerceToDate(value);
        DatePart part = DatePart.DAY;

        if (operands.length == 2) {
          Object pattern = operands[1].eval(context);
          if (pattern == null)
            return null;
          part = DatePart.of(pattern.toString());
        }

        switch (part) {
          // First day of the year
          case YEAR: {
            ZonedDateTime datetime =
                ZonedDateTime.ofInstant(instant.truncatedTo(ChronoUnit.DAYS), context.getZone());
            return datetime.withDayOfYear(1).toInstant();
          }
          // First day of the month
          case MONTH: {
            ZonedDateTime datetime =
                ZonedDateTime.ofInstant(instant.truncatedTo(ChronoUnit.DAYS), context.getZone());
            return datetime.withDayOfMonth(1).toInstant();
          }
          // First day of the quarter
          case QUARTER: {
            ZonedDateTime datetime =
                ZonedDateTime.ofInstant(instant.truncatedTo(ChronoUnit.DAYS), context.getZone());
            int month = (datetime.getMonthValue() / 3) * 3 + 1;
            return datetime.withMonth(month).withDayOfMonth(1).toInstant();
          }
          // First day of the week
          case WEEKOFYEAR: {
            ZonedDateTime datetime =
                ZonedDateTime.ofInstant(instant.truncatedTo(ChronoUnit.DAYS), context.getZone());
            DayOfWeek dow = DayOfWeek.of(((ExpressionContext) context).getFirstDayOfWeek());
            return datetime.with(TemporalAdjusters.previousOrSame(dow)).toInstant();
          }

          case DAY:
            return instant.truncatedTo(ChronoUnit.DAYS);
          case HOUR:
            return instant.truncatedTo(ChronoUnit.HOURS);
          case MINUTE:
            return instant.truncatedTo(ChronoUnit.MINUTES);
          case SECOND:
            return instant.truncatedTo(ChronoUnit.SECONDS);
          case MILLISECOND:
            return instant.truncatedTo(ChronoUnit.MILLIS);
          case MICROSECOND:
            return instant.truncatedTo(ChronoUnit.MICROS);
          case NANOSECOND:
            return instant.truncatedTo(ChronoUnit.NANOS);
          default:
            throw createUnexpectedDatePartException("TRUNCATE", part);
        }
      case STRING:
      case BOOLEAN:
      default:
        throw createUnexpectedDataTypeException("TRUNCATE", type);
    }
  }

  // -------------------------------------------------------------
  // DATE AND TIME
  // -------------------------------------------------------------

  /**
   * Build DATE(YYYY,MM,DD) function
   */
  @ScalarFunction(name = "DATE", minArgs = 3, maxArgs = 3,
      category = "i18n::Operator.Category.Date")
  public static Object date(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    Object v2 = operands[2].eval(context);
    if (v2 == null)
      return null;

    int year = coerceToInteger(v0).intValue();
    int month = coerceToInteger(v1).intValue();
    int day = coerceToInteger(v2).intValue();

    int monthsToAdd = 0;
    if (month < 1) {
      monthsToAdd = month;
      month = 1;
    } else if (month > 12) {
      monthsToAdd = month - 1;
      month = 1;
    }

    int daysToAdd = 0;
    if (day < 1 || day > 31) {
      daysToAdd = day;
      day = 1;
    }

    LocalDate date = LocalDate.of(year, month, day);
    if (monthsToAdd != 0)
      date = date.plusMonths(monthsToAdd);
    if (daysToAdd != 0)
      date = date.plusDays(daysToAdd);

    return date.atStartOfDay(ZoneOffset.UTC).toInstant();
  }

  /**
   * Day of the month (number from 1-31).
   */
  @ScalarFunction(name = "DAY", alias = "DAYOFMONTH", category = "i18n::Operator.Category.Date")
  public static Object day(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone());
    return Long.valueOf(dt.getDayOfMonth());
  }

  /**
   * Returns the name of the weekday (in English).
   */
  @ScalarFunction(name = "DAYNAME", category = "i18n::Operator.Category.Date")
  public static Object dayname(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    DayOfWeek weekday =
        ZonedDateTime.ofInstant(coerceToDate(value), context.getZone()).getDayOfWeek();
    return weekday.getDisplayName(TextStyle.FULL, Locale.ENGLISH);
  }

  /**
   * Day of the week (Sunday=1 to Saturday=7).
   */
  @ScalarFunction(name = "DAYOFWEEK", category = "i18n::Operator.Category.Date")
  public static Object dayofweek(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    DayOfWeek dow = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone()).getDayOfWeek();

    int result = dow.getValue() + 1;
    if (result == 8)
      result = 1;

    return Long.valueOf(result);
  }

  /**
   * Day of the week (Monday=1 to Sunday=7).
   */
  @ScalarFunction(name = "DAYOFWEEK_ISO", category = "i18n::Operator.Category.Date")
  public static Object dayofweek_iso(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    DayOfWeek dow = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone()).getDayOfWeek();
    return Long.valueOf(dow.getValue());

    // ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
    // return Object.of(DatePart.DAYOFWEEKISO.get(dt));
  }

  /** Day of the year (number from 1-366). */
  @ScalarFunction(name = "DAYOFYEAR", category = "i18n::Operator.Category.Date")
  public static Object dayofyear(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone());
    return Long.valueOf(dt.getDayOfYear());
  }

  /** Week of the year (number from 1-54). */
  @ScalarFunction(name = "WEEKOFYEAR", alias = "WEEK", category = "i18n::Operator.Category.Date")
  public static Object weekOfYear(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone());
    return Long.valueOf(dt.get(ChronoField.ALIGNED_WEEK_OF_YEAR));
  }

  /** Week from the beginning of the month (0-5) */
  @ScalarFunction(name = "WEEKOFMONTH", category = "i18n::Operator.Category.Date")
  public static Object weekOfMonth(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone());
    return Long.valueOf(dt.get(ChronoField.ALIGNED_WEEK_OF_MONTH));
  }

  /** Month of the year (number from 1-12). */
  @ScalarFunction(name = "MONTH", category = "i18n::Operator.Category.Date")
  public static Object month(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone());
    return Long.valueOf(dt.getMonthValue());
  }

  /** Returns the name of the month (in English). */
  @ScalarFunction(name = "MONTHNAME", category = "i18n::Operator.Category.Date")
  public static Object monthName(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    Month month = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone()).getMonth();
    return month.getDisplayName(TextStyle.FULL, Locale.ENGLISH);
  }

  /** Quarter of the year (number from 1-4). */
  @ScalarFunction(name = "QUARTER", category = "i18n::Operator.Category.Date")
  public static Object quarter(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone());
    return Long.valueOf(dt.get(IsoFields.QUARTER_OF_YEAR));
  }

  /** The year of a date */
  @ScalarFunction(name = "YEAR", category = "i18n::Operator.Category.Date")
  public static Object year(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone());
    return Long.valueOf(dt.getYear());
  }

  /** The hour (0-23). @See {@link #MINUTE}, {@link #SECOND} */
  @ScalarFunction(name = "HOUR", category = "i18n::Operator.Category.Date")
  public static Object hour(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    LocalTime time = LocalTime.from(coerceToDate(value).atZone(context.getZone()));
    return Long.valueOf(time.getHour());
  }

  /** The minute (0-59). @See {@link #HOUR}, {@link #SECOND} */
  @ScalarFunction(name = "MINUTE", category = "i18n::Operator.Category.Date")
  public static Object minute(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    LocalTime time = LocalTime.from(coerceToDate(value).atZone(context.getZone()));
    return Long.valueOf(time.getMinute());
  }

  /** The second (0-59). @See {@link #HOUR}, {@link #MINUTE} */
  @ScalarFunction(name = "SECOND", category = "i18n::Operator.Category.Date")
  public static Object second(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    LocalTime time = LocalTime.from(coerceToDate(value).atZone(context.getZone()));
    return Long.valueOf(time.getSecond());
  }

  /** Adds or subtracts a specified number of days to a date or timestamp */
  @ScalarFunction(name = "ADD_DAYS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object add_days(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Instant value = coerceToDate(operands[0].eval(context));
    if (value == null)
      return value;
    Object days = operands[1].eval(context);
    if (days == null)
      return null;

    ZonedDateTime dt =
        ZonedDateTime.ofInstant(value, context.getZone()).plusDays(coerceToInteger(days));
    return dt.toInstant();
  }

  /** Adds or subtracts a specified number of weeks to a date or timestamp */
  @ScalarFunction(name = "ADD_WEEKS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object add_weeks(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Object weeks = operands[1].eval(context);
    if (weeks == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone())
        .plusWeeks(coerceToInteger(weeks));
    return dt.toInstant();
  }

  /** Adds or subtracts a specified number of months to a date or timestamp */
  @ScalarFunction(name = "ADD_MONTHS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object add_months(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Object months = operands[1].eval(context);
    if (months == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone())
        .plusMonths(coerceToInteger(months));
    return dt.toInstant();
  }

  /** Adds or subtracts a specified number of months to a date or timestamp */
  @ScalarFunction(name = "ADD_YEARS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object add_years(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Object years = operands[1].eval(context);
    if (years == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone())
        .plusYears(coerceToInteger(years));
    return dt.toInstant();
  }

  /** Adds or subtracts a specified number of hours to a date or timestamp */
  @ScalarFunction(name = "ADD_HOURS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object add_hours(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;
    Object hours = operands[1].eval(context);
    if (hours == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone())
        .plusHours(coerceToInteger(hours));
    return dt.toInstant();
  }

  /** Adds or subtracts a specified number of minutes to a date or timestamp */
  @ScalarFunction(name = "ADD_MINUTES", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object add_minutes(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;
    Object minutes = operands[1].eval(context);
    if (minutes == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone())
        .plusMinutes(coerceToInteger(minutes));
    return dt.toInstant();
  }

  /**
   * Adds or subtracts a specified number of seconds to a date or timestamp
   */
  @ScalarFunction(name = "ADD_SECONDS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object add_seconds(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Object seconds = operands[1].eval(context);
    if (seconds == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone())
        .plusSeconds(coerceToInteger(seconds));
    return dt.toInstant();
  }

  /**
   * Returns number of days between two date values.
   */
  @ScalarFunction(name = "DAYS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object days_between(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    LocalDate startDate = coerceToDate(v0).atZone(context.getZone()).toLocalDate();
    LocalDate endDate = coerceToDate(v1).atZone(context.getZone()).toLocalDate();
    return startDate.until(endDate, ChronoUnit.DAYS);
  }

  /**
   * Returns number of months between two date.
   */
  @ScalarFunction(name = "MONTHS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object months_between(final IExpressionContext context,
      final IExpression[] operands) throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    LocalDate startDate = coerceToDate(v0).atZone(context.getZone()).toLocalDate();
    LocalDate endDate = coerceToDate(v1).atZone(context.getZone()).toLocalDate();
    long days = startDate.until(endDate, ChronoUnit.DAYS);
    return days / 31d;
  }

  /** Returns number of years between two date. */
  @ScalarFunction(name = "YEARS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object years_between(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    LocalDate startDate = coerceToDate(v0).atZone(context.getZone()).toLocalDate();
    LocalDate endDate = coerceToDate(v1).atZone(context.getZone()).toLocalDate();
    return startDate.until(endDate, ChronoUnit.YEARS);
  }

  /** Return the number of hours between two timestamps */
  @ScalarFunction(name = "HOURS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object hours_between(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value1 = operands[0].eval(context);
    if (value1 == null)
      return null;
    Object value2 = operands[1].eval(context);
    if (value2 == null)
      return null;

    LocalDateTime startDateTime = coerceToDate(value1).atZone(context.getZone()).toLocalDateTime();
    LocalDateTime endDateTime = coerceToDate(value2).atZone(context.getZone()).toLocalDateTime();
    return startDateTime.until(endDateTime, ChronoUnit.HOURS);
  }

  /** Return the number of minutes between two timestamps */
  @ScalarFunction(name = "MINUTES_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object minutes_between(final IExpressionContext context,
      final IExpression[] operands) throws ExpressionException {
    Object value1 = operands[0].eval(context);
    if (value1 == null)
      return null;
    Object value2 = operands[1].eval(context);
    if (value2 == null)
      return null;

    LocalDateTime startDateTime = coerceToDate(value1).atZone(context.getZone()).toLocalDateTime();
    LocalDateTime endDateTime = coerceToDate(value2).atZone(context.getZone()).toLocalDateTime();
    return startDateTime.until(endDateTime, ChronoUnit.MINUTES);
  }

  /** Return the number of seconds between two timestamps */
  @ScalarFunction(name = "SECONDS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object seconds_between(final IExpressionContext context,
      final IExpression[] operands) throws ExpressionException {
    Object value1 = operands[0].eval(context);
    if (value1 == null)
      return null;
    Object value2 = operands[1].eval(context);
    if (value2 == null)
      return null;

    LocalDateTime startDateTime = coerceToDate(value1).atZone(context.getZone()).toLocalDateTime();
    LocalDateTime endDateTime = coerceToDate(value2).atZone(context.getZone()).toLocalDateTime();
    return startDateTime.until(endDateTime, ChronoUnit.SECONDS);
  }

  /** Returns the first day of the month. */
  @ScalarFunction(name = "FIRST_DAY", category = "i18n::Operator.Category.Date")
  public static Object first_day(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone())
        .with(TemporalAdjusters.firstDayOfMonth());
    return dt.toInstant();
  }

  /** Returns the last day of the month. */
  @ScalarFunction(name = "LAST_DAY", category = "i18n::Operator.Category.Date")
  // TODO: @ScalarFunction(name = "LAST_DAY", minArgs = 1, maxArgs = 2, category =
  // "i18n::Operator.Category.Date")
  public static Object last_day(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    TemporalAdjuster adjuster = TemporalAdjusters.lastDayOfMonth();
    // if ( operands.length==2) {
    // Object v1 = operands[1].eval(context);
    //
    // }

    ZonedDateTime dt =
        ZonedDateTime.ofInstant(coerceToDate(value), context.getZone()).with(adjuster);
    return dt.toInstant();
  }

  /**
   * Returns the date of the first specified day of week that occurs after the input date.
   */
  @ScalarFunction(name = "NEXT_DAY", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object next_day(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    Object dow = operands[1].eval(context);
    if (dow == null)
      return null;

    DayOfWeek dayofweek = DayOfWeek.valueOf(dow.toString().toUpperCase());
    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone())
        .with(TemporalAdjusters.next(dayofweek));

    return dt.toInstant();
  }

  /**
   * Returns the date of the first specified day of week that occurs before the input date.
   */
  @ScalarFunction(name = "PREVIOUS_DAY", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Object previous_day(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    Object dow = operands[1].eval(context);
    if (dow == null)
      return null;

    DayOfWeek dayofweek = DayOfWeek.valueOf(dow.toString().toUpperCase());
    ZonedDateTime dt = ZonedDateTime.ofInstant(coerceToDate(value), context.getZone())
        .with(TemporalAdjusters.previous(dayofweek));

    return dt.toInstant();
  }
}


