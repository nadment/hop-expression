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
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.math3.util.FastMath;
import org.apache.hop.core.util.HopJaroWinklerDistance;
import org.apache.hop.expression.DataTypeName;
import org.apache.hop.expression.DatePart;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.ScalarFunction;
import org.apache.hop.expression.util.Characters;
import org.apache.hop.expression.util.Coerse;
import org.apache.hop.expression.util.Converter;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.FirstDayOfQuarter;
import org.apache.hop.expression.util.LastDayOfQuarter;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.expression.util.Regexp;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.ParseException;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.TextStyle;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.IsoFields;
import java.time.temporal.TemporalAdjuster;
import java.time.temporal.TemporalAdjusters;
import java.util.Locale;
import java.util.Random;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

public class BuiltInFunctions {

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  /** The maximum size to which the padding can expand. */
  private static final int PAD_LIMIT = 8192;

  private static final Soundex SOUNDEX = new Soundex();
  private static final FirstDayOfQuarter FirstDayOfQuarter = new FirstDayOfQuarter();
  private static final LastDayOfQuarter LastDayOfQuarter = new LastDayOfQuarter();
  
  private BuiltInFunctions() {
    // Utility class
  }
  
  /**
   * Returns the arc cosine, the angle in radians whose cosine is the specified float expression.
   */
  @ScalarFunction(id = "ACOS", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/acos.html")
  public static Object acos(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Double d = Coerse.toNumber(value);
    if (d < -1.0 || d > 1.0) {
      throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, value);
    }
    return FastMath.acos(d);
  }

  @ScalarFunction(id = "ACOSH", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/acosh.html")
  public static Object acosh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.acosh(Coerse.toNumber(value));
  }

  @ScalarFunction(id = "ASINH", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/asinh.html")
  public static Object asinh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    return FastMath.asinh(Coerse.toNumber(value));
  }

  @ScalarFunction(id = "ATAN", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/atan.html")
  public static Object atan(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    return FastMath.atan(Coerse.toNumber(value));
  }

  @ScalarFunction(id = "ATANH", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/atanh.html")
  public static Object atanh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    return FastMath.atanh(Coerse.toNumber(value));
  }

  @ScalarFunction(id = "ATAN2", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Trigonometry", documentationUrl = "/docs/atan2.html")
  public static Object atan2(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    return FastMath.atan2(Coerse.toNumber(v0), Coerse.toNumber(v1));
  }

  /** Returns the trigonometric cosine of the specified angle in radians in the specified number. */
  @ScalarFunction(id = "COS", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/cos.html")
  public static Object cos(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.cos(Coerse.toNumber(value));
  }

  /** Returns the trigonometric cosine of the specified angle in radians in the specified number. */
  @ScalarFunction(id = "COSH", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/cosh.html")
  public static Object cosh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.cosh(Coerse.toNumber(value));
  }

  /** Returns the trigonometric cotangent of the angle in radians specified by float expression. */
  @ScalarFunction(id = "COT", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/cot.html")
  public static Object cot(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    double number = Coerse.toNumber(value);
    if (number == 0)
      throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, value);

    return FastMath.cos(number) / FastMath.sin(number);
  }

  @ScalarFunction(id = "ASIN", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/asin.html")
  public static Object asin(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.asin(Coerse.toNumber(value));
  }

  /**
   * Calculates the trigonometric sine of the angle in radians.
   */
  @ScalarFunction(id = "SIN", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/sin.html")
  public static Object sin(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;
    return FastMath.sin(Coerse.toNumber(value));
  }

  /**
   * Calculates the hyperbolic sine of its argument.
   */
  @ScalarFunction(id = "SINH", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/sinh.html")
  public static Object sinh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.sinh(Coerse.toNumber(value));
  }

  /**
   * Calculates the tangent of its argument, the argument should be expressed in radians.
   */
  @ScalarFunction(id = "TAN", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/tan.html")
  public static Object tan(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.tan(Coerse.toNumber(value));
  }

  /**
   * Calculates the hyperbolic tangent of its argument.
   */
  @ScalarFunction(id = "TANH", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/tanh.html")
  public static Object tanh(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.tanh(Coerse.toNumber(value));
  }

  /**
   * Returns the number of PI.
   */
  @ScalarFunction(id = "PI", minArgs = 0, maxArgs = 0,
      category = "i18n::Operator.Category.Mathematical", documentationUrl = "/docs/pi.html")
  public static Object pi(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    return Math.PI;
  }

  /** Returns the absolute (positive) value of the numeric value. */
  @ScalarFunction(id = "ABS", category = "i18n::Operator.Category.Mathematical",
      documentationUrl = "/docs/abs.html")
  public static Object abs(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;

    if (value instanceof Double) {
      return FastMath.abs((double) value);
    }
    if (value instanceof Long) {
      return FastMath.abs((long) value);
    }

    return Coerse.toBigNumber(value).abs();
  }

  /** Returns the sign of a number. */
  @ScalarFunction(id = "SIGN", category = "i18n::Operator.Category.Mathematical",
      documentationUrl = "/docs/sign.html")
  public static Object sign(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;
    Double number = Coerse.toNumber(value);
    if (number == 0)
      return 0L;
    return (number > 0) ? 1L : -1L;
  }

  /** Function to converts radians to degrees. */
  @ScalarFunction(id = "DEGREES", category = "i18n::Operator.Category.Mathematical",
      documentationUrl = "/docs/degrees.html")
  public static Object degrees(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.toDegrees(Coerse.toNumber(value));
  }

  /** The function converts degrees to radians. */
  @ScalarFunction(id = "RADIANS", category = "i18n::Operator.Category.Mathematical",
      documentationUrl = "/docs/radians.html")
  public static Object radians(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.toRadians(Coerse.toNumber(value));
  }

  /** Returns the exponential value of a numeric expression. */
  @ScalarFunction(id = "EXP", category = "i18n::Operator.Category.Mathematical",
      documentationUrl = "/docs/exp.html")
  public static Object exp(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.exp(Coerse.toNumber(value));
  }


  @ScalarFunction(id = "DIV0", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical", documentationUrl = "/docs/div0.html")
  public static Object div0(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object left = operands[0].eval(context);
    if (left == null)
      return null;
    Object right = operands[1].eval(context);
    if (right == null)
      return null;

    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      BigDecimal divisor = Coerse.toBigNumber(right);

      // prevent a division by zero and return zero
      if (divisor.signum() == 0)
        return divisor;

      return Coerse.toBigNumber(left).divide(Coerse.toBigNumber(right), MathContext.DECIMAL128);
    }
    if (left instanceof Double || right instanceof Double) {
      double divisor = Coerse.toNumber(right);
      // prevent a division by zero and return zero
      if (divisor == 0D)
        return 0D;
      return Coerse.toNumber(left) / divisor;
    }
    if (left instanceof Long || right instanceof Long) {
      long divisor = Coerse.toInteger(right);
      // prevent a division by zero and return zero
      if (divisor == 0L)
        return 0L;

      return Coerse.toInteger(left) / divisor;
    }

    BigDecimal divisor = Coerse.toBigNumber(right);
    // prevent a division by zero and return zero
    if (divisor.signum() == 0)
      return divisor;

    return Coerse.toBigNumber(left).divide(divisor);
  }

  /** Returns the values rounded to the nearest equal or larger integer. */
  @ScalarFunction(id = "CEILING", names = "CEIL", category = "i18n::Operator.Category.Mathematical",
      documentationUrl = "/docs/ceiling.html")
  public static Object ceiling(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    if (value instanceof Long)
      return value;
    if (value instanceof BigDecimal) {
      return Coerse.toBigNumber(value).setScale(0, RoundingMode.CEILING);
    }
    return FastMath.ceil(Coerse.toNumber(value));
  }

  /** Returns the values rounded to the nearest equal or smaller integer. */
  @ScalarFunction(id = "FLOOR", category = "i18n::Operator.Category.Mathematical",
      documentationUrl = "/docs/floor.html")
  public static Object floor(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    if (value instanceof Long)
      return value;
    if (value instanceof BigDecimal) {
      return Coerse.toBigNumber(value).setScale(0, RoundingMode.FLOOR);
    }
    return FastMath.floor(Coerse.toNumber(value));
  }

  /** Returns the values rounded to the nearest integer. */
  @ScalarFunction(id = "ROUND", category = "i18n::Operator.Category.Mathematical",
      documentationUrl = "/docs/round.html")
  public static Object round(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;
    return FastMath.round(Coerse.toNumber(value));
  }
  
  /**
   * Returns the natural logarithm of a numeric value.
   */
  @ScalarFunction(id = "LN", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/ln.html")
  public static Object ln(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Double number = Coerse.toNumber(value);
    if (number <= 0)
      throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, value);
    return FastMath.log(number);
  }

  /**
   * Returns the specified base logarithm of a numeric value.
   */
  @ScalarFunction(id = "LOG", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Trigonometry", documentationUrl = "/docs/log.html")
  public static Object log(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object base = operands[0].eval(context);
    if (base == null)
      return null;

    Object value = operands[1].eval(context);
    if (value == null)
      return null;
    Double number = Coerse.toNumber(value);
    if (number <= 0)
      throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, value);

    return FastMath.log(number) / FastMath.log(Coerse.toNumber(base));
  }

  /**
   * Returns the base 10 logarithm of a numeric value.
   */
  @ScalarFunction(id = "LOG10", category = "i18n::Operator.Category.Trigonometry",
      documentationUrl = "/docs/log10.html")
  public static Object log10(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Double number = Coerse.toNumber(value);
    if (number <= 0)
      throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, value);
    return FastMath.log10(number);
  }

  /**
   * Returns the cubic root of a numeric expression.
   * 
   * @See {@link #SQRT}
   */
  @ScalarFunction(id = "CBRT", category = "i18n::Operator.Category.Mathematical",
      documentationUrl = "/docs/cbrt.html")
  public static Object cbrt(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return FastMath.cbrt(Coerse.toNumber(value));
  }

  /**
   * Returns the square-root of a non-negative numeric expression.
   * 
   * @See {@link #CBRT}
   */
  @ScalarFunction(id = "SQRT", category = "i18n::Operator.Category.Mathematical",
      documentationUrl = "/docs/sqrt.html")
  public static Object sqrt(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Double number = Coerse.toNumber(value);
    if (number < 0)
      throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, value);
    return FastMath.sqrt(number);
  }

  /**
   * Returns the square of a numeric expression.
   * 
   */
  @ScalarFunction(id = "SQUARE", category = "i18n::Operator.Category.Mathematical",
      documentationUrl = "/docs/square.html")
  public static Object square(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Double number = Coerse.toNumber(value);
    return FastMath.pow(number, 2);
  }

  @ScalarFunction(id = "POWER", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical", documentationUrl = "/docs/power.html")
  public static Object power(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object left = operands[0].eval(context);
    Object right = operands[1].eval(context);
    if (left == null || right == null) {
      return null;
    }
    Double power = Coerse.toNumber(right);
    if (power == 0)
      return 1L;
    // Power can not be negative
    if (power < 0)
      throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, power);

    return FastMath.pow(Coerse.toNumber(left), Coerse.toNumber(right));
  }

  /**
   * The function return the current date at time 00:00
   */
  @ScalarFunction(id = "CURRENT_DATE", names = {"TODAY"}, deterministic = false, minArgs = 0,
      maxArgs = 0, category = "i18n::Operator.Category.Date", documentationUrl = "/docs/current_date.html")
  public static Object current_date(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    return context.getAttribute(ExpressionContext.CACHED_TODAY);
  }
  
  /**
   * The function return the current date and time
   */
  @ScalarFunction(id = "CURRENT_TIMESTAMP", names = {"NOW"}, deterministic = false, minArgs = 0,
      maxArgs = 0, category = "i18n::Operator.Category.Date", documentationUrl = "/docs/current_timestamp.html")
  public static Object current_datetime(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    return context.getAttribute(ExpressionContext.CACHED_NOW);
  }
  
  /**
   * The function return the current time zone
   */
  @ScalarFunction(id = "CURRENT_TIMEZONE", deterministic = false, minArgs = 0,
      maxArgs = 0, category = "i18n::Operator.Category.Date", documentationUrl = "/docs/current_timezone.html")
  public static Object current_timezone(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    return context.getAttribute(ExpressionContext.CACHED_TIMEZONE);
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
  @ScalarFunction(id = "MD5", category = "i18n::Operator.Category.Cryptographic",
      documentationUrl = "/docs/md5.html")
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
  @ScalarFunction(id = "SHA1", category = "i18n::Operator.Category.Cryptographic",
      documentationUrl = "/docs/sha1.html")
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
  @ScalarFunction(id = "SHA256", category = "i18n::Operator.Category.Cryptographic",
      documentationUrl = "/docs/sha256.html")
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
  @ScalarFunction(id = "SHA384", category = "i18n::Operator.Category.Cryptographic",
      documentationUrl = "/docs/sha384.html")
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
  @ScalarFunction(id = "SHA512", category = "i18n::Operator.Category.Cryptographic",
      documentationUrl = "/docs/sha512.html")
  public static Object sha512(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    return getHash(operands[0].eval(context), "SHA-512");
  }

  @ScalarFunction(id = "GETBIT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Bitwise", documentationUrl = "/docs/getbit.html")
  public static Object getbit(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    return (Coerse.toInteger(v0) & (1L << Coerse.toInteger(v1).intValue())) != 0;
  }


  @ScalarFunction(id = "RANDOM", deterministic = false, minArgs = 0, maxArgs = 1,
      category = "i18n::Operator.Category.Mathematical", documentationUrl = "/docs/random.html")
  public static Object random(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    Random random = (Random) context.getAttribute(ExpressionContext.CACHED_RANDOM);

    if (operands.length == 1) {
      Object value = operands[0].eval(context);
      // FIXME: What if multi random in the same with different SEED ?
      random.setSeed(Coerse.toInteger(value));
    }
    return random.nextDouble();
  }

  /** Returns a randomly generated UUID (Universal Unique Identifier defined by RFC 4122) */
  @ScalarFunction(id = "UUID", minArgs = 0, maxArgs = 0, deterministic = false,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/uuid.html")
  public static Object uuid(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    return UUID.randomUUID().toString();
  }

  private static Object getHash(Object value, String algorithm) throws ExpressionException {
    if (value == null)
      return null;

    try {
      MessageDigest md = MessageDigest.getInstance(algorithm);
      md.update(Coerse.toBinary(value));
      return Hex.encodeHexString(md.digest());
    } catch (NoSuchAlgorithmException e) {
      throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, algorithm);
    }
  }

  @ScalarFunction(id = "TRANSLATE", minArgs = 3, maxArgs = 3,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/translate.html")
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

    String string = Coerse.toString(v0);
    String findChars = Coerse.toString(v1);
    String replaceChars = Coerse.toString(v2);

    StringBuilder buffer = new StringBuilder(string.length());
    // if shorter than findChars, then characters are removed
    // (if null, we don't access replaceChars at all)
    if (replaceChars == null)
      replaceChars = "";
    int replaceSize = replaceChars.length();

    for (int i = 0, size = string.length(); i < size; i++) {
      char ch = string.charAt(i);
      int index = findChars.indexOf(ch);
      if (index >= 0) {
        if (index < replaceSize) {
          buffer.append(replaceChars.charAt(index));
        }
      } else {
        buffer.append(ch);
      }
    }
    return buffer.toString();
  }

  /**
   * The function compute Levenshtein distance.
   */
  @ScalarFunction(id = "LEVENSHTEIN", category = "i18n::Operator.Category.String", minArgs = 2,
      maxArgs = 2, documentationUrl = "/docs/levenshtein.html")
  public static Object levenshtein(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    return Long
        .valueOf(StringUtils.getLevenshteinDistance(Coerse.toString(v0), Coerse.toString(v1)));
  }

  /**
   * The function compute Jaro Winkler distance.
   */
  @ScalarFunction(id = "JAROWINKLER", category = "i18n::Operator.Category.String", minArgs = 2,
      maxArgs = 2, documentationUrl = "/docs/jarowinkler.html")
  public static Object jarowinkler(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    HopJaroWinklerDistance jaro = new HopJaroWinklerDistance();
    jaro.apply(Coerse.toString(v0), Coerse.toString(v1));
    return Long.valueOf(Math.round(100*jaro.getJaroDistance()));
  }  
  
  /**
   * The function return the ASCII value of the first character in a string. If the string is empty,
   * a value of 0 is returned.
   */
  @ScalarFunction(id = "ASCII", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/ascii.html")
  public static Object ascii(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    String string = Coerse.toString(value);
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
  @ScalarFunction(id = "CHR", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/chr.html")
  public static Object chr(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    int codePoint = Coerse.toInteger(value).intValue();

    if (!Character.isValidCodePoint(codePoint)) {
      throw new ExpressionException(ExpressionError.ARGUMENT_OUT_OF_RANGE, codePoint);
    }
    return new String(Character.toChars(codePoint));
  }

  /**
   * Returns a string consisting of a the specified number of blank spaces.
   */
  @ScalarFunction(id = "SPACE", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/space.html")
  public static Object space(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    int length = Coerse.toInteger(value).intValue();
    if (length < 0)
      return null;

    char[] chars = new char[length];
    for (int i = length - 1; i >= 0; i--) {
      chars[i] = ' ';
    }

    return new String(chars);
  }

  /**
   * The function reverses the order of characters in a string value, or of bytes in a binary value.
   */
  @ScalarFunction(id = "REVERSE", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/reverse.html")
  public static Object reverse(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    if (value instanceof byte[]) {
      byte[] data = Coerse.toBinary(value);
      byte[] result = new byte[data.length];
      for (int i = data.length - 1, j = 0; i >= 0; i--, j++) {
        result[j] = data[i];
      }
      return result;
    }

    StringBuilder builder = new StringBuilder(Coerse.toString(value)).reverse();
    return builder.toString();
  }

  /**
   * Returns a string that contains a phonetic representation of the input string.
   */
  @ScalarFunction(id = "SOUNDEX", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/soundex.html")
  public static Object soundex(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return SOUNDEX.soundex(Coerse.toString(value));
  }

  @ScalarFunction(id = "DIFFERENCE", category = "i18n::Operator.Category.String", minArgs = 2,
      maxArgs = 2, documentationUrl = "/docs/difference.html")
  public static Object difference(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    try {
      return Long.valueOf(SOUNDEX.difference(Coerse.toString(v0), Coerse.toString(v1)));
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
  @ScalarFunction(id = "UNICODE", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/unicode.html")
  public static Object unicode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    String string = Coerse.toString(value);
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
  @ScalarFunction(id = "LPAD", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/lpad.html")
  public static Object lpad(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    String str = Coerse.toString(v0);

    Object v1 = operands[1].eval(context);
    int length = Coerse.toInteger(v1).intValue();

    // If this parameter is omitted, the function will pad spaces
    String pad = null;
    if (operands.length == 3) {
      Object v2 = operands[2].eval(context);
      pad = Coerse.toString(v2);
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
  @ScalarFunction(id = "RPAD", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/rpad.html")
  public static Object rpad(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    String str = Coerse.toString(v0);

    Object v1 = operands[1].eval(context);
    int length = Coerse.toInteger(v1).intValue();

    // If this parameter is omitted, the function will pad spaces
    String pad = null;
    if (operands.length == 3) {
      Object v2 = operands[2].eval(context);
      pad = Coerse.toString(v2);
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

  /**
   * The function returns the number of characters of the specified string.
   */
  @ScalarFunction(id = "LENGTH", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/length.html")
  public static Object length(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return value;
    if (value instanceof byte[]) {
      return ((byte[]) value).length;
    }
    return Long.valueOf(Coerse.toString(value).length());
  }

  /**
   * The function convert a string value to lower case.
   * 
   * @See {@link #INITCAP}, {@link #UPPER}
   */
  @ScalarFunction(id = "LOWER", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/lower.html")
  public static Object lower(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Coerse.toString(value).toLowerCase(Locale.getDefault());
  }

  /**
   * The function convert a string value to upper case.
   * 
   * @See {@link #LOWER}, {@link #INITCAP}
   */
  @ScalarFunction(id = "UPPER", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/upper.html")
  public static Object upper(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Coerse.toString(value).toUpperCase(Locale.getDefault());
  }

  /**
   * Returns a string with the first letter of each word in uppercase and the subsequent letters in
   * lowercase. @See {@link #LOWER}, {@link #UPPER}
   */
  @ScalarFunction(id = "INITCAP", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/initcap.html")
  public static Object initcap(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    String str = Coerse.toString(value);
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
  @ScalarFunction(id = "TRIM", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/trim.html")
  public static Object trim(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    String string = Coerse.toString(value);
    String characters = null;

    if (operands.length == 2) {
      Object stripChars = operands[1].eval(context);
      if (stripChars == null)
        return null;

      characters = Coerse.toString(stripChars);
    }

    return StringUtils.strip(string, characters);
  }

  /**
   * The function removes leading characters from a string.
   *
   * @see {@link #trim}, {@link #rtrim}
   */
  @ScalarFunction(id = "LTRIM", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/ltrim.html")
  public static Object ltrim(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    String string = Coerse.toString(value);
    String characters = null;

    if (operands.length == 2) {
      Object stripChars = operands[1].eval(context);
      if (stripChars == null)
        return null;
      characters = Coerse.toString(stripChars);
    }

    return StringUtils.stripStart(string, characters);
  }

  /**
   * The function removes leading characters from a string.
   *
   * @see {@link #trim}, {@link #ltrim}
   */
  @ScalarFunction(id = "RTRIM", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/rtrim.html")
  public static Object rtrim(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    String string = Coerse.toString(value);
    String characters = null;

    if (operands.length == 2) {
      Object stripChars = operands[1].eval(context);
      if (stripChars == null)
        return null;
      characters = Coerse.toString(stripChars);
    }

    return StringUtils.stripEnd(string, characters);
  }


  /**
   * Trims white space from the beginning and end of the string, and replaces all other white space
   * with single blanks.
   * 
   * White space is defined as any sequence of blanks, null characters, newlines (line feeds),
   * carriage returns, horizontal tabs and form feeds (vertical tabs).
   */
  @ScalarFunction(id = "SQUEEZE", category = "i18n::Operator.Category.String", minArgs = 1,
      maxArgs = 1, documentationUrl = "/docs/squeeze.html")
  public static Object squeeze(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    String str = Coerse.toString(v0);
    char[] a = str.toCharArray();
    int n = 1;
    for (int i = 1; i < a.length; i++) {
      a[n] = a[i];
      if (!Character.isSpaceChar(a[n]))
        n++;
      else {
        a[n] = ' ';
        if (a[n - 1] != ' ')
          n++;
      }
    }

    return new String(a, 0, n);
  }
  
  /**
   * The function extracts a number of characters from a string starting from left.
   * 
   * @See {@link #right}
   */
  @ScalarFunction(id = "LEFT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/left.html")
  public static Object left(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;
    int length = Coerse.toInteger(v1).intValue();
    if (length < 0) {
      length = 0;
    }

    if (v0 instanceof byte[]) {
      byte[] bytes = (byte[]) v0;
      if (bytes.length <= length)
        return bytes;
      byte[] result = new byte[length];
      System.arraycopy(bytes, 0, result, 0, length);
      return result;
    }

    String str = Coerse.toString(v0);
    if (str.length() <= length) {
      return str;
    }
    return str.substring(0, length);
  }

  /**
   * The function extracts a number of characters from a string (starting from right)
   * 
   * @See {@link #left}
   */
  @ScalarFunction(id = "RIGHT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/right.html")
  public static Object right(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;
    int length = Coerse.toInteger(v1).intValue();
    if (length < 0) {
      length = 0;
    }

    if (v0 instanceof byte[]) {
      byte[] bytes = (byte[]) v0;
      if (bytes.length <= length)
        return bytes;
      byte[] result = new byte[length];
      System.arraycopy(bytes, bytes.length - length, result, 0, length);
      return result;
    }

    String str = Coerse.toString(v0);
    if (str.length() <= length) {
      return str;
    }
    return str.substring(str.length() - length);
  }

  /**
   * Encodes special characters in a string using the Java string literal encoding format.
   */
  @ScalarFunction(id = "STRINGENCODE", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/stringencode.html")
  public static Object stringEncode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    String str = Coerse.toString(value);

    return StringEscapeUtils.escapeJava(str);
  }

  /**
   * Converts an encoded string using the Java string literal encoding format. Special characters
   * are \b, \t, \n, \f, \r, \", \\, \\000, \\u0000
   */
  @ScalarFunction(id = "STRINGDECODE", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/stringdecode.html")
  public static Object stringDecode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    String str = Coerse.toString(value);

    return StringEscapeUtils.unescapeJava(str);
  }

  /**
   * The function encode the string as a URL.
   *
   * @see {@link #urldecode}
   */
  @ScalarFunction(id = "URLENCODE", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/urlencode.html")
  public static Object urlEncode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    try {
      return URLEncoder.encode(Coerse.toString(value), StandardCharsets.UTF_8.name());
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.URLENCODE_ERROR, value, e.getMessage());
    }
  }

  /**
   * The function decode the URL to a string.
   *
   * @see {@link #urlencode}
   */
  @ScalarFunction(id = "URLDECODE", category = "i18n::Operator.Category.String",
      documentationUrl = "/docs/urldecode.html")
  public static Object urlDecode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    try {
      return URLDecoder.decode(Coerse.toString(value), StandardCharsets.UTF_8.name());
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.URLDECODE_ERROR, value, e.getMessage());
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
  @ScalarFunction(id = "EQUAL_NULL", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Comparison", documentationUrl = "/docs/equal_null.html")
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

    return Coerse.compare(v0, v1) == 0;
  }

  // -------------------------------------------------------------
  // CONDITIONAL
  // -------------------------------------------------------------

  /**
   * The function returns the smallest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #GREATEST}
   */
  @ScalarFunction(id = "LEAST", minArgs = 1, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/least.html")
  public static Object least(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object result = null;
    for (IExpression operand : operands) {
      Object value = operand.eval(context);
      // null is always smaller
      if (value == null)
        continue;
      if (result == null || Coerse.compare(value, result) < 0) {
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
  @ScalarFunction(id = "GREATEST", minArgs = 1, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/greatest.html")
  public static Object greatest(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object result = null;
    for (IExpression operand : operands) {
      Object value = operand.eval(context);
      if (Coerse.compare(result, value) < 0)
        result = value;
    }

    return result;
  }

  /**
   * Single-level if-then-else expression. Similar to CASE, but only allows a single condition.
   */
  @ScalarFunction(id = "IF", minArgs = 3, maxArgs = 3,
      category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/if.html")
  public static Object iff(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    return operands[Coerse.toBoolean(value) ? 1 : 2].eval(context);
  }

  /**
   * The IFNULL function replace the null with value (Alias NVL).
   */
  @ScalarFunction(id = "IFNULL", names = "NVL", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/ifnull.html")
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
  @ScalarFunction(id = "COALESCE", minArgs = 1, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/coalesce.html")
  public static Object coalesce(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    for (IExpression operand : operands) {
      Object value = operand.eval(context);
      if (value != null)
        return value;
    }

    return null;
  }

  @ScalarFunction(id = "NVL2", minArgs = 3, maxArgs = 3,
      category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/nvl2.html")
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
  @ScalarFunction(id = "DECODE", minArgs = 4, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/decode.html")
  public static Object decode(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);

    int index = -1;
    for (int i = 1, len = operands.length - 1; i < len; i += 2) {
      Object search = operands[i].eval(context);
      if (Coerse.compare(value, search) == 0) {
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
  @ScalarFunction(id = "NULLIF", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/nullif.html")
  public static Object nullIf(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    Object compare = operands[1].eval(context);

    if (Coerse.compare(value, compare) == 0)
      return null;

    return value;
  }

  /**
   * Returns 0 if its argument is null; otherwise, returns its argument.
   */
  @ScalarFunction(id = "ZEROIFNULL", minArgs = 1, maxArgs = 1,
      category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/zeroifnull.html")
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
  @ScalarFunction(id = "NULLIFZERO", minArgs = 1, maxArgs = 1,
      category = "i18n::Operator.Category.Conditional", documentationUrl = "/docs/nullifzero.html")
  public static Object nullIfZero(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);

    if (Coerse.toInteger(value) == 0L)
      return null;

    return value;
  }

  /**
   * The function repeats a string as many times as specified.
   */
  @ScalarFunction(id = "REPEAT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/repeat.html")
  public static Object repeat(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;
    int count = Coerse.toInteger(v1).intValue();

    if (v0 instanceof byte[]) {
      byte[] bytes = (byte[]) v0;
      try {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        for (int i = 0; i < count; i++) {
          buffer.write(bytes);
        }
        return buffer.toByteArray();
      } catch (IOException e) {
        return null;
      }
    }

    String value = Coerse.toString(v0);
    StringBuilder builder = new StringBuilder(value.length() * count);
    while (count-- > 0) {
      builder.append(value);
    }
    return builder.toString();
  }

  /**
   * Replaces a substring of the specified length, starting at the specified position, with a new
   * string or binary value.
   */
  @ScalarFunction(id = "INSERT", minArgs = 4, maxArgs = 4,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/insert.html")
  public static Object insert(final IExpressionContext context, final IExpression[] operands)
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
    Object v3 = operands[3].eval(context);
    if (v3 == null)
      return null;

    int position = Coerse.toInteger(v1).intValue();
    int length = Coerse.toInteger(v2).intValue();

    if (v0 instanceof byte[]) {
      byte[] bytes = (byte[]) v0;
      int start = Math.min(Math.max(0, position - 1), bytes.length);
      length = Math.min(length, bytes.length);
      if (length < 0)
        throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, length);

      try {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        buffer.write(bytes, 0, start);
        buffer.write(Coerse.toBinary(v3));
        buffer.write(bytes, start + length, bytes.length - start - length);
        return buffer.toByteArray();
      } catch (IOException e) {
        throw new ExpressionException(ExpressionError.INTERNAL_ERROR, e);
      }
    }

    String str = Coerse.toString(v0);
    int start = Math.min(Math.max(0, position - 1), str.length());

    length = Math.min(length, str.length());
    if (length < 0)
      throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, length);

    StringBuilder builder = new StringBuilder();
    builder.append(str.substring(0, start));
    builder.append(Coerse.toString(v3));
    builder.append(str.substring(start + length));
    return builder.toString();
  }

  /**
   * Returns the position in the string that is the first character of a specified occurrence of the
   * substring.
   */
  @ScalarFunction(id = "INSTR", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/instr.html")
  public static Object instr(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    Object v1 = operands[1].eval(context);

    if (v0 == null || v1 == null) {
      return null;
    }

    String str = Coerse.toString(v0);
    String substr = Coerse.toString(v1);

    // If 3 operands
    int start = 0;
    if (operands.length == 3) {
      start = Coerse.toInteger(operands[2].eval(context)).intValue();

      if (start > 0)
        start -= 1;
      else if (start < 0) {
        return Long.valueOf(str.lastIndexOf(substr, str.length() + start) + 1L);
      }
    }

    return Long.valueOf(str.indexOf(substr, start) + 1L);
  }

  /**
   * Removes all occurrences of a specified substring, and optionally replaces them with another
   * string.
   */
  @ScalarFunction(id = "REPLACE", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/replace.html")
  public static Object replace(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    String string = Coerse.toString(v0);
    String search = Coerse.toString(v1);

    if (operands.length == 3) {
      Object v2 = operands[2].eval(context);
      String replacement = Coerse.toString(v2);
      return string.replace(search, replacement);
    }

    return StringUtils.remove(string, search);
  }



  /**
   * Returns the portion of the string from string, starting from the character/byte specified by
   * start, with optionally limited length.
   */
  @ScalarFunction(id = "SUBSTRING", names = {"SUBSTR"}, minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/substring.html")
  public static Object substring(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    String string = Coerse.toString(operands[0].eval(context));
    int length = string.length();
    int start = Coerse.toInteger(operands[1].eval(context)).intValue();

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

    int end = start + Coerse.toInteger(operands[2].eval(context)).intValue();
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

  @ScalarFunction(id = "REGEXP_REPLACE", minArgs = 2, maxArgs = 6,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/regexp_replace.html")
  public static Object regexp_replace(final IExpressionContext context,
      final IExpression[] operands) throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null) {
      return null;
    }
    String input = Coerse.toString(v0);

    Object v1 = operands[1].eval(context);
    if (v1 == null) {
      return null;
    }
    String regexp = Coerse.toString(v1);

    // An empty pattern matches nothing
    if (regexp.length() == 0)
      return input;

    // Default empty string
    String replacement = "";
    if (operands.length >= 3) {
      Object v2 = operands[2].eval(context);
      if (v2 != null) {
        replacement = Coerse.toString(v2);
      }
    }

    // Default position 1
    int position = 1;
    if (operands.length >= 4) {
      Object v3 = operands[3].eval(context);
      if (v3 != null) {
        position = Coerse.toInteger(v3).intValue();
      }
    }

    // Default occurrence 0
    int occurrence = 0;
    if (operands.length >= 5) {
      Object v4 = operands[4].eval(context);
      if (v4 != null) {
        occurrence = Coerse.toInteger(v4).intValue();
      }
    }

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 6) {
      Object v5 = operands[5].eval(context);
      flags = Regexp.parseFlags(Coerse.toString(v5));
    }

    try {

      // Back reference
      if ((replacement.indexOf('\\') >= 0) || (replacement.indexOf('$') >= 0)) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < replacement.length(); i++) {
          char c = replacement.charAt(i);
          if (c == '$') {
            builder.append('\\');
          } else if (c == '\\' && ++i < replacement.length()) {
            c = replacement.charAt(i);
            builder.append(c >= '0' && c <= '9' ? '$' : '\\');
          }
          builder.append(c);
        }
        replacement = builder.toString();
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
      throw new ExpressionException(ExpressionError.INVALID_REGEXP_PATTERN, regexp);
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.REGEXP_REPLACE_ERROR, replacement);
    }
  }

  @ScalarFunction(id = "REGEXP_SUBSTR", minArgs = 2, maxArgs = 4,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/regexp_substr.html")
  public static Object regexp_substr(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null) {
      return null;
    }
    String input = Coerse.toString(v0);

    Object v1 = operands[1].eval(context);
    if (v1 == null) {
      return null;
    }
    String regexp = Coerse.toString(v1);
    // An empty pattern matches nothing
    if (regexp.length() == 0)
      return null;

    // Default position 1
    int position = 1;
    if (operands.length >= 3) {
      Object v2 = operands[2].eval(context);
      if (v2 != null) {
        position = Coerse.toInteger(v2).intValue();
      }
    }

    // Default occurrence
    int occurrence = 0;
    if (operands.length >= 4) {
      Object v3 = operands[3].eval(context);
      if (v3 != null) {
        occurrence = Coerse.toInteger(v3).intValue();
      }
    }

    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 5) {
      Object v4 = operands[5].eval(context);
      flags = Regexp.parseFlags(Coerse.toString(v4));
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
      throw new ExpressionException(ExpressionError.INVALID_REGEXP_PATTERN, regexp);
    }
  }


  @ScalarFunction(id = "REGEXP_INSTR", minArgs = 2, maxArgs = 6,
      category = "i18n::Operator.Category.String", documentationUrl = "/docs/regexp_instr.html")
  public static Object regexp_instr(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null) {
      return null;
    }
    String input = Coerse.toString(v0);

    Object v1 = operands[1].eval(context);
    if (v1 == null) {
      return null;
    }
    String regexp = Coerse.toString(v1);
    // An empty pattern matches nothing
    if (regexp.length() == 0)
      return 0L;

    // Default position 1
    int position = 1;
    if (operands.length >= 3) {
      Object v2 = operands[2].eval(context);
      if (v2 != null) {
        position = Coerse.toInteger(v2).intValue();
      }
    }

    // Default occurrence
    int occurrence = 0;
    if (operands.length >= 4) {
      Object v3 = operands[3].eval(context);
      if (v3 != null) {
        occurrence = Coerse.toInteger(v3).intValue();
      }
    }

    // Return-option
    int returnOption = 0;
    if (operands.length >= 5) {
      Object v4 = operands[4].eval(context);
      if (v4 != null) {
        returnOption = Coerse.toInteger(v4).intValue();
      }
    }

    // Flags
    int flags = Pattern.UNICODE_CASE;
    if (operands.length == 6) {
      Object v5 = operands[5].eval(context);
      flags = Regexp.parseFlags(Coerse.toString(v5));
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
      throw new ExpressionException(ExpressionError.INVALID_REGEXP_PATTERN, regexp);
    }
  }

  /**
   * The function returns TRUE if the first value starts with second value. Both values must be data
   * type string or binary.
   *
   * @see {@link #endswith}
   */
  @ScalarFunction(id = "STARTSWITH", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Comparison", documentationUrl = "/docs/startswith.html")
  public static Object startsWith(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    if (v0 instanceof byte[]) {
      byte[] data = Coerse.toBinary(v0);
      byte[] prefix = Coerse.toBinary(v1);
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

    return Coerse.toString(v0).startsWith(Coerse.toString(v1));
  }

  /**
   * The function returns TRUE if the first value ends with second value. Both values must be data
   * type of string or binary.
   *
   * @see {@link #startwith}
   */
  @ScalarFunction(id = "ENDSWITH", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Comparison", documentationUrl = "/docs/endswith.html")
  public static Object endsWith(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    if (v0 instanceof byte[]) {
      byte[] data = Coerse.toBinary(v0);
      byte[] suffix = Coerse.toBinary(v1);
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

    return Coerse.toString(v0).endsWith(Coerse.toString(v1));
  }


  /** Contains function */
  @ScalarFunction(id = "CONTAINS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Comparison", documentationUrl = "/docs/contains.html")
  public static Object contains(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    if (Coerse.toString(v0).contains(Coerse.toString(v1)))
      return Boolean.TRUE;

    return Boolean.FALSE;
  }

  /**
   * Splits a string on the specified string delimiter and returns the part at the specified position.
   * 
   * @param context
   * @param operands
   * @return
   * @throws ExpressionException
   * @See {@link #strtok}
   */
  @ScalarFunction(id = "SPLIT_PART", category = "i18n::Operator.Category.String", minArgs = 3, maxArgs = 3, documentationUrl = "/docs/split_part.html")  
  public static Object split_part(final IExpressionContext context, final IExpression[] operands)
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
    
    String str = Coerse.toString(v0);
    String delimiter = Coerse.toString(v1);
    int index = Coerse.toInteger(v2).intValue();
    
    String[] parts = StringUtils.splitByWholeSeparator(str, delimiter, -1);
    
    // If the part number is negative, the parts are counted backward from the end of the string.
    if ( index<0 ) index += parts.length+1;
    
    // If the part index is out of range, the returned value is an empty string.
    if ( index<1 || index>parts.length ) {
      return "";
    }
          
    return parts[index-1];    
  }
  
  /**
   * Splits a string on the specified list of delimiter characters and returns the part at the specified position.
   * 
   * @param context The expression context
   * @param operands The operands
   * @return value
   * @throws ExpressionException
   * @See {@link #split_part}
   */
  @ScalarFunction(id = "STRTOK", category = "i18n::Operator.Category.String", minArgs = 1, maxArgs = 3, documentationUrl = "/docs/strtok.html")  
  public static Object strtok(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    String str = Coerse.toString(v0);

    // Default value
    String delimiter = " ";
    int index = 1;
    
    if ( operands.length==2) {
      Object v1 = operands[1].eval(context);
      if (v1 == null)
        return null;
    
      if ( v1 instanceof Number ) {
        index = Coerse.toInteger(v1).intValue();   
      }
      else {
        delimiter = Coerse.toString(v1);
      }
    }
    else if ( operands.length==3) {
      Object v1 = operands[1].eval(context);
      if (v1 == null)
        return null;
      delimiter = Coerse.toString(v1);
      Object v2 = operands[2].eval(context);
      if (v2 == null)
        return null;
      index = Coerse.toInteger(v2).intValue();      
    }
    
    String[] parts = StringUtils.split(str,delimiter);
    
    if ( index<0 ) index += parts.length+1;
    
    // If the part index is out of range, the returned value is an empty string.
    if ( index<1 || index>parts.length ) {
      return null;
    }
          
    return parts[index-1];
  }
  
  /**
   * Converts a string or numeric expression to a boolean value.
   */
  @ScalarFunction(id = "TO_BOOLEAN", category = "i18n::Operator.Category.Conversion",
      documentationUrl = "/docs/to_boolean.html")
  public static Object to_boolean(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    return Converter.to(value, DataTypeName.BOOLEAN, null);
  }

  /** Converts a numeric or date expression to a string value. */
  @ScalarFunction(id = "TO_CHAR", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.Conversion", documentationUrl = "/docs/to_char.html")
  public static Object to_char(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null) {
      return null;
    }

    String pattern = null;
    if (operands.length > 1) {
      Object v1 = operands[1].eval(context);
      if (v1 != null)
        pattern = Coerse.toString(v1);
    }

    if ( v0 instanceof String) {
      return v0;
    }
    
    if ( v0 instanceof Number ) {
      return NumberFormat.of(pattern).format(Coerse.toBigNumber(v0));
    }
    
    if ( v0 instanceof ZonedDateTime ) {
      return DateTimeFormat.of(pattern).format(Coerse.toDate(v0));
    }
    
    throw new ExpressionException(ExpressionError.UNEXPECTED_DATA_TYPE, "TO_CHAR", DataTypeName.from(v0));
  }

  /** Converts a string expression to a number value. */
  @ScalarFunction(id = "TO_NUMBER", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.Conversion", documentationUrl = "/docs/to_number.html")
  public static Object to_number(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    try {
      String str = Coerse.toString(v0);
      String format = null;

      // With format
      if (operands.length == 2) {
        format = Coerse.toString(operands[1].eval(context));
      }
      return NumberFormat.of(format).parse(str);
    } catch (ParseException e) {
      throw new ExpressionException(ExpressionError.PARSE_ERROR, e.getMessage());
    }
  }

  /** Converts a string expression to a date value. */
  @ScalarFunction(id = "TO_DATE", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.Conversion", documentationUrl = "/docs/to_date.html")
  public static Object to_date(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    if (v0 instanceof ZonedDateTime)
      return v0;

    String format = null;
    if (operands.length > 1) {
      Object v1 = operands[1].eval(context);
      if (v1 != null)
        format = Coerse.toString(v1);
    } else {
      format = (String) context.getAttribute(ExpressionContext.EXPRESSION_DATE_FORMAT);
    }
    try {
      return DateTimeFormat.of(format).parse(Coerse.toString(v0));
    } catch (ParseException e) {
      throw new ExpressionException(ExpressionError.PARSE_ERROR, e.getMessage());
    }
  }

  /**
   * Round down numeric expressions or truncates a date or timestamp to the specified part.
   */
  @ScalarFunction(id = "TRUNCATE", names = "TRUNC", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical", documentationUrl = "/docs/truncate.html")
  public static Object truncate(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    BigDecimal number = Coerse.toBigNumber(value);
    int scale = 0;
    if (operands.length == 2) {
      Object pattern = operands[1].eval(context);
      if (pattern == null)
        return null;
      scale = Coerse.toInteger(pattern).intValue();
    }

    if (scale > number.scale())
      scale = number.scale();
    return number.movePointRight(scale).setScale(0, RoundingMode.DOWN).movePointLeft(scale);
  }

  // -------------------------------------------------------------
  // DATE AND TIME
  // -------------------------------------------------------------

  /**
   * Build DATE(YYYY,MM,DD) function
   */
  @ScalarFunction(id = "DATE", minArgs = 3, maxArgs = 3, category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/date.html")
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

    int year = Coerse.toInteger(v0).intValue();
    int month = Coerse.toInteger(v1).intValue();
    int day = Coerse.toInteger(v2).intValue();

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

    LocalDate localDate = LocalDate.of(year, month, day);
    if (monthsToAdd != 0)
      localDate = localDate.plusMonths(monthsToAdd);
    if (daysToAdd != 0)
      localDate = localDate.plusDays(daysToAdd);

    return localDate.atStartOfDay().atZone(ZoneId.systemDefault());
  }



  /**
   * Truncates a date or timestamp to the specified part.
   */
  @ScalarFunction(id = "DATE_TRUNC", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/date_trunc.html")
  public static Object date_trunc(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {

    DatePart part = Coerse.toDatePart(operands[0].eval(context));

    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    ZonedDateTime datetime = Coerse.toDate(v1);

    switch (part) {
      case MILLENNIUM:
        return datetime.withDayOfYear(1).minusYears(datetime.getYear() % 1000);
      case CENTURY:
        return datetime.withDayOfYear(1).minusYears(datetime.getYear() % 100);
      case DECADE:
        return datetime.withDayOfYear(1).minusYears(datetime.getYear() % 10);
      // First day of the year
      case YEAR:
        return datetime.withDayOfYear(1);
      // First day of the month
      case MONTH:
        return datetime.withDayOfMonth(1);
      // First day of the quarter
      case QUARTER:
        int month = (datetime.getMonthValue() / 3) * 3 + 1;
        return datetime.withMonth(month).withDayOfMonth(1);
      // First day of the week (the week starts on Monday)
      case WEEK:
        // TODO: DayOfWeek dow = DayOfWeek.of(Integer.parseInt((String)
        // context.getAttribute("NLS_FIRST_DAY_OF_WEEK")));
        return datetime.with(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY));
      case DAY:
        return datetime.truncatedTo(ChronoUnit.DAYS);
      case HOUR:
        return datetime.truncatedTo(ChronoUnit.HOURS);
      case MINUTE:
        return datetime.truncatedTo(ChronoUnit.MINUTES);
      case SECOND:
        return datetime.truncatedTo(ChronoUnit.SECONDS);
      case MILLISECOND:
        return datetime.truncatedTo(ChronoUnit.MILLIS);
      case MICROSECOND:
        return datetime.truncatedTo(ChronoUnit.MICROS);
      case NANOSECOND:
        return datetime.truncatedTo(ChronoUnit.NANOS);
      default:        
        throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, part);
    }
  }

  /**
   * Day of the month (number from 1-31).
   */
  @ScalarFunction(id = "DAY", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/day.html")
  public static Object day(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Long.valueOf(Coerse.toDate(value).getDayOfMonth());
  }

  /**
   * Returns the name of the weekday (in English).
   */
  @ScalarFunction(id = "DAYNAME", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/dayname.html")
  public static Object dayname(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    DayOfWeek weekday = Coerse.toDate(value).getDayOfWeek();
    return weekday.getDisplayName(TextStyle.FULL, Locale.ENGLISH);
  }

  /**
   * Day of the week (Sunday=1 to Saturday=7).
   */
  @ScalarFunction(id = "DAYOFWEEK", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/dayofweek.html")
  public static Object dayofweek(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    DayOfWeek dow = Coerse.toDate(value).getDayOfWeek();
    int result = dow.getValue() + 1;
    if (result == 8)
      result = 1;

    return Long.valueOf(result);
  }

  /**
   * Day of the week (Monday=1 to Sunday=7).
   */
  @ScalarFunction(id = "ISODAYOFWEEK", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/isodayofweek.html")
  public static Object isodayofweek(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    DayOfWeek dow = Coerse.toDate(value).getDayOfWeek();
    return Long.valueOf(dow.getValue());
  }

  /** Day of the year (number from 1-366). */
  @ScalarFunction(id = "DAYOFYEAR", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/dayofyear.html")
  public static Object dayofyear(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Long.valueOf(Coerse.toDate(value).getDayOfYear());
  }

  /** Month of the year (number from 1-12). */
  @ScalarFunction(id = "MONTH", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/month.html")
  public static Object month(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Long.valueOf(Coerse.toDate(value).getMonthValue());
  }

  /** Returns the name of the month (in English). */
  @ScalarFunction(id = "MONTHNAME", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/monthname.html")
  public static Object monthName(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    ZonedDateTime datetime = Coerse.toDate(value);
    Month month = datetime.getMonth();
    return month.getDisplayName(TextStyle.FULL, Locale.ENGLISH);
  }

  /** Quarter of the year (number from 1-4). */
  @ScalarFunction(id = "QUARTER", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/quarter.html")
  public static Object quarter(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Long.valueOf(Coerse.toDate(value).get(IsoFields.QUARTER_OF_YEAR));
  }

  /** The year of a date */
  @ScalarFunction(id = "YEAR", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/year.html")
  public static Object year(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Long.valueOf(Coerse.toDate(value).getYear());
  }

  /** Year of the week ISO semantics */
  @ScalarFunction(id = "ISOYEAR", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/isoyear.html")
  public static Object isoyear(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Long.valueOf(Coerse.toDate(value).get(IsoFields.WEEK_BASED_YEAR));
  }

  /** Week of the year (number from 1-54). */
  @ScalarFunction(id = "WEEK", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/week.html")
  public static Object week(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Long.valueOf(Coerse.toDate(value).get(ChronoField.ALIGNED_WEEK_OF_YEAR));
  }

  /** Week of the year ISO semantics (number from 1-53). */
  @ScalarFunction(id = "ISOWEEK", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/isoweek.html")
  public static Object isoweek(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Long.valueOf(Coerse.toDate(value).get(IsoFields.WEEK_OF_WEEK_BASED_YEAR));
  }

  /** The hour (0-23). @See {@link #MINUTE}, {@link #SECOND} */
  @ScalarFunction(id = "HOUR", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/hour.html")
  public static Object hour(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Long.valueOf(Coerse.toDate(value).getHour());
  }

  /** The minute (0-59). @See {@link #HOUR}, {@link #SECOND} */
  @ScalarFunction(id = "MINUTE", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/minute.html")
  public static Object minute(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Long.valueOf(Coerse.toDate(value).getMinute());
  }

  /** The second (0-59). @See {@link #HOUR}, {@link #MINUTE} */
  @ScalarFunction(id = "SECOND", category = "i18n::Operator.Category.Date",
      documentationUrl = "/docs/second.html")
  public static Object second(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    return Long.valueOf(Coerse.toDate(value).getSecond());
  }

  /** Adds or subtracts a specified number of days to a date or timestamp */
  @ScalarFunction(id = "ADD_DAYS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/add_days.html")
  public static Object add_days(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    ZonedDateTime value = Coerse.toDate(operands[0].eval(context));
    if (value == null)
      return value;
    Object days = operands[1].eval(context);
    if (days == null)
      return null;

    return value.plusDays(Coerse.toInteger(days));
  }

  /** Adds or subtracts a specified number of weeks to a date or timestamp */
  @ScalarFunction(id = "ADD_WEEKS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/add_weeks.html")
  public static Object add_weeks(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    ZonedDateTime value = Coerse.toDate(operands[0].eval(context));
    if (value == null)
      return null;
    Object weeks = operands[1].eval(context);
    if (weeks == null)
      return null;

    return value.plusWeeks(Coerse.toInteger(weeks));
  }

  /** Adds or subtracts a specified number of months to a date or timestamp */
  @ScalarFunction(id = "ADD_MONTHS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/add_months.html")
  public static Object add_months(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    ZonedDateTime value = Coerse.toDate(operands[0].eval(context));
    if (value == null)
      return null;
    Object months = operands[1].eval(context);
    if (months == null)
      return null;

    return value.plusMonths(Coerse.toInteger(months));
  }

  /** Adds or subtracts a specified number of months to a date or timestamp */
  @ScalarFunction(id = "ADD_YEARS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/add_years.html")
  public static Object add_years(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    ZonedDateTime value = Coerse.toDate(operands[0].eval(context));
    if (value == null)
      return null;
    Object years = operands[1].eval(context);
    if (years == null)
      return null;

    return value.plusYears(Coerse.toInteger(years));
  }

  /** Adds or subtracts a specified number of hours to a date or timestamp */
  @ScalarFunction(id = "ADD_HOURS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/add_hours.html")
  public static Object add_hours(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    ZonedDateTime value = Coerse.toDate(operands[0].eval(context));
    if (value == null)
      return value;
    Object hours = operands[1].eval(context);
    if (hours == null)
      return null;

    return value.plusHours(Coerse.toInteger(hours));
  }

  /** Adds or subtracts a specified number of minutes to a date or timestamp */
  @ScalarFunction(id = "ADD_MINUTES", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/add_minutes.html")
  public static Object add_minutes(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    ZonedDateTime value = Coerse.toDate(operands[0].eval(context));
    if (value == null)
      return value;
    Object minutes = operands[1].eval(context);
    if (minutes == null)
      return null;

    return value.plusMinutes(Coerse.toInteger(minutes));
  }

  /**
   * Adds or subtracts a specified number of seconds to a date or timestamp
   */
  @ScalarFunction(id = "ADD_SECONDS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/add_seconds.html")
  public static Object add_seconds(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    ZonedDateTime value = Coerse.toDate(operands[0].eval(context));
    if (value == null)
      return null;
    Object seconds = operands[1].eval(context);
    if (seconds == null)
      return null;

    return value.plusSeconds(Coerse.toInteger(seconds));
  }

  /**
   * Returns number of days between two date values.
   */
  @ScalarFunction(id = "DAYS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/days_between.html")
  public static Object days_between(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    ZonedDateTime startDateTime = Coerse.toDate(v0);
    ZonedDateTime endDateTime = Coerse.toDate(v1);
    return startDateTime.until(endDateTime, ChronoUnit.DAYS);
  }

  /**
   * Returns number of months between two date.
   */
  @ScalarFunction(id = "MONTHS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/months_between.html")
  public static Object months_between(final IExpressionContext context,
      final IExpression[] operands) throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    ZonedDateTime startDateTime = Coerse.toDate(v0);
    ZonedDateTime endDateTime = Coerse.toDate(v1);
    long days = startDateTime.until(endDateTime, ChronoUnit.DAYS);
    return days / 31d;
  }

  /** Returns number of years between two date. */
  @ScalarFunction(id = "YEARS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/years_between.html")
  public static Object years_between(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    ZonedDateTime startDateTime = Coerse.toDate(v0);
    ZonedDateTime endDateTime = Coerse.toDate(v1);
    return startDateTime.until(endDateTime, ChronoUnit.YEARS);
  }

  /** Return the number of hours between two timestamps */
  @ScalarFunction(id = "HOURS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/hours_between.html")
  public static Object hours_between(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    ZonedDateTime startDateTime = Coerse.toDate(v0);
    ZonedDateTime endDateTime = Coerse.toDate(v1);
    return startDateTime.until(endDateTime, ChronoUnit.HOURS);
  }

  /** Return the number of minutes between two timestamps */
  @ScalarFunction(id = "MINUTES_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/minutes_between.html")
  public static Object minutes_between(final IExpressionContext context,
      final IExpression[] operands) throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    ZonedDateTime startDateTime = Coerse.toDate(v0);
    ZonedDateTime endDateTime = Coerse.toDate(v1);
    return startDateTime.until(endDateTime, ChronoUnit.MINUTES);
  }

  /** Return the number of seconds between two timestamps */
  @ScalarFunction(id = "SECONDS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/seconds_between.html")
  public static Object seconds_between(final IExpressionContext context,
      final IExpression[] operands) throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = operands[1].eval(context);
    if (v1 == null)
      return null;

    ZonedDateTime startDateTime = Coerse.toDate(v0);
    ZonedDateTime endDateTime = Coerse.toDate(v1);
    return startDateTime.until(endDateTime, ChronoUnit.SECONDS);
  }

  /** Returns the first day of the date part. */
  @ScalarFunction(id = "FIRST_DAY", category = "i18n::Operator.Category.Date", minArgs = 1, maxArgs = 2, documentationUrl = "/docs/first_day.html")
  public static Object first_day(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    // Default to first day of month
    TemporalAdjuster adjuster = TemporalAdjusters.firstDayOfMonth();    
    
    if (operands.length == 2) {
      Object v1 = operands[1].eval(context);
      if ( v1==null)
        return null;
      DatePart part = Coerse.toDatePart(v1);
      switch(part) {
        case YEAR:
          adjuster = TemporalAdjusters.firstDayOfYear();
          break;
        case QUARTER:          
          adjuster = FirstDayOfQuarter;
          break;
        case MONTH:
          adjuster = TemporalAdjusters.firstDayOfMonth();
          break;
        case WEEK:
          adjuster = TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY);
          break;
        default:
          throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, part);
      }      
    }
    
    // Remove time and adjust
    return Coerse.toDate(value).truncatedTo(ChronoUnit.DAYS).with(adjuster);
  }

  /** Returns the last day of the date part. */
  @ScalarFunction(id = "LAST_DAY", category = "i18n::Operator.Category.Date", minArgs = 1, maxArgs = 2, documentationUrl = "/docs/last_day.html")
  public static Object last_day(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    // Default to last day of month
    TemporalAdjuster adjuster = TemporalAdjusters.lastDayOfMonth();
    if (operands.length == 2) {
      Object v1 = operands[1].eval(context);
      if ( v1==null)
        return null;
      DatePart part = Coerse.toDatePart(v1);
      switch(part) {
        case YEAR:
          adjuster = TemporalAdjusters.lastDayOfYear();
          break;
        case QUARTER:          
          adjuster = LastDayOfQuarter;
          break;
        case MONTH:
          adjuster = TemporalAdjusters.lastDayOfMonth();
          break;    
        case WEEK:
          adjuster = TemporalAdjusters.nextOrSame(DayOfWeek.SUNDAY);
          break;
        default:
          throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, part);
      }      
    }

    // Remove time and adjust
    return Coerse.toDate(value).truncatedTo(ChronoUnit.DAYS).with(adjuster);
  }

  /**
   * Returns the date of the first specified day of week that occurs after the input date.
   */
  @ScalarFunction(id = "NEXT_DAY", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/next_day.html")
  public static Object next_day(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;
    Object dow = operands[1].eval(context);
    if (dow == null)
      return null;

    DayOfWeek dayofweek = DayOfWeek.valueOf(dow.toString().toUpperCase());

    return Coerse.toDate(value).with(TemporalAdjusters.next(dayofweek));
  }

  /**
   * Returns the date of the first specified day of week that occurs before the input date.
   */
  @ScalarFunction(id = "PREVIOUS_DAY", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date", documentationUrl = "/docs/previous_day.html")
  public static Object previous_day(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    Object dow = operands[1].eval(context);
    if (dow == null)
      return null;

    DayOfWeek dayofweek = DayOfWeek.valueOf(dow.toString().toUpperCase());

    return Coerse.toDate(value).with(TemporalAdjusters.previous(dayofweek));
  }

}
