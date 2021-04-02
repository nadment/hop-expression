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

import org.apache.commons.codec.EncoderException;
import org.apache.commons.codec.binary.Hex;
import org.apache.commons.codec.language.Soundex;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.WordUtils;
import org.apache.commons.math3.util.FastMath;
import org.apache.hop.expression.util.DateFormat;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.expression.value.Value;
import org.apache.hop.expression.value.ValueBigNumber;
import org.apache.hop.expression.value.ValueBinary;
import org.apache.hop.expression.value.ValueBoolean;
import org.apache.hop.expression.value.ValueDate;
import org.apache.hop.expression.value.ValueInteger;
import org.apache.hop.expression.value.ValueNumber;
import org.apache.hop.expression.value.ValueString;
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
import java.time.temporal.TemporalAdjusters;
import java.util.Calendar;
import java.util.Locale;
import java.util.regex.Pattern;

/** A <code>Function</code> is a type of operator which has conventional function-call syntax. */

public class Function extends Operator {

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  private static final char[] HEX = "0123456789abcdef".toCharArray();

  /** The maximum size to which the padding can expand. */
  private static final int PAD_LIMIT = 8192;

  private static final Soundex SOUNDEX = new Soundex();

  private final boolean isDeterministic;

  private final Method method;

  private final int minArgs;
  private final int maxArgs;

  protected Function(Kind kind, String name, boolean isAlias, boolean isDeterministic,
      Method method, int min, int max, String category) {
    super(kind, name, 10, 10, isAlias, category);

    this.isDeterministic = isDeterministic;
    this.method = method;
    this.minArgs = min;
    this.maxArgs = max;

  }

  /**
   * Whether the function always returns the same result for the same parameters.
   *
   * @return true if it does
   */
  public boolean isDeterministic() {
    return isDeterministic;
  }

  @Override
  public void write(StringWriter writer, ExpressionCall call, int leftPrec, int rightPrec) {
    switch (kind) {
      case CAST: {
        IExpression[] operands = call.getOperands();
        writer.append(this.getName());
        writer.append('(');
        operands[0].write(writer, leftPrec, rightPrec);
        writer.append(" AS ");
        writer.append(operands[1].toString());
        writer.append(')');
        break;
      }
      case EXTRACT: {
        IExpression[] operands = call.getOperands();
        writer.append(this.getName());
        writer.append('(');
        operands[0].write(writer, leftPrec, rightPrec);
        writer.append(" FROM ");
        writer.append(operands[1].toString());
        writer.append(')');
        break;
      }
      default:
        writer.append(this.getName());
        writer.append('(');
        boolean first = true;
        for (IExpression operand : call.getOperands()) {
          if (!first)
            writer.append(',');
          else
            first = false;
          operand.write(writer, leftPrec, rightPrec);
        }
        writer.append(')');
    }
  }

  @Override
  public void checkNumberOfArguments(int len) throws ExpressionException {
    if (len < minArgs) {
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Expression.NotEnoughArguments", this.getName()));
    }

    if (len > maxArgs) {
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Expression.TooManyNumberOfArguments", this.getName()));
    }
  }

  @Override
  public Value eval(final IExpressionContext context, final IExpression... args)
      throws ExpressionException {

    try {
      Object[] parameters = new Object[2];
      parameters[0] = context;
      parameters[1] = args;
      return (Value) this.method.invoke(null, parameters);
    } catch (Exception e) {
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Expression.FunctionError", this.getName(), e.getMessage()),
          e);
    }
  }

  @Override
  public IExpression optimize(IExpressionContext context, IExpression... operands)
      throws ExpressionException {

    IExpression[] args = new IExpression[operands.length];

    boolean isAllConstant = true;
    for (int index = 0; index < args.length; index++) {
      IExpression operand = operands[index];

      if (operand instanceof ExpressionCall) {
        operand = operand.optimize(context);
      }

      if (!operand.isConstant())
        isAllConstant = false;

      args[index] = operand;
    }

    if (this.isDeterministic() && isAllConstant) {
      return eval(context, args);
    }

    return new ExpressionCall(this, args);
  }


  @ScalarFunction(name = "CURRENT_DATE", alias = {"NOW", "SYSDATE"}, deterministic = false,
      minArgs = 0, maxArgs = 0,category="i18n::Operator.Category.Date")
  public static Value current_date(final IExpressionContext context, final IExpression... args)
      throws ExpressionException {
    return ValueDate.of(context.getCurrentDate());
  }

  // -------------------------------------------------------------
  // CRYPTOGRAPHIC
  // -------------------------------------------------------------

  /**
   * The function calculate the MD5 hash of a data value. The hash will be returned as a 32
   * characters hex-encoded string.
   *
   * @see {@link #SHA1}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
   */
  @ScalarFunction(name = "MD5", category = "i18n::Operator.Category.Cryptographic")
  public static Value md5(final IExpressionContext context, final IExpression... args) {
    return getHash(args[0].eval(context), "MD5");
  }

  /**
   * The function calculate the SHA-1 hash of a data value. The hash will be returned as a 40
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA256}, {@link #SHA384}, {@link #SHA512}
   */
  @ScalarFunction(name = "SHA1", category = "i18n::Operator.Category.Cryptographic")
  public static Value sha1(final IExpressionContext context, final IExpression... args) {
    return getHash(args[0].eval(context), "SHA-1");
  }

  /**
   * The function calculate the SHA-256 hash of a data value. The hash will be returned as a 64
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA384}, {@link #SHA512}
   */
  @ScalarFunction(name = "SHA256", category = "i18n::Operator.Category.Cryptographic")
  public static Value sha256(final IExpressionContext context, final IExpression... args) {
    return getHash(args[0].eval(context), "SHA-256");
  }

  /**
   * The function calculate the SHA-384 hash of a data value. The hash will be returned as a 96
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA512}
   */
  @ScalarFunction(name = "SHA384", category = "i18n::Operator.Category.Cryptographic")
  public static Value sha384(final IExpressionContext context, final IExpression... args) {
    return getHash(args[0].eval(context), "SHA-384");
  }

  /**
   * The function calculate the SHA-512 hash of a data value. The hash will be returned as a 128
   * characters hex-encoded string.
   *
   * @see {@link #MD5}, {@link #SHA1}, {@link #SHA256}, {@link #SHA384}
   */
  @ScalarFunction(name = "SHA512", category = "i18n::Operator.Category.Cryptographic")
  public static Value sha512(final IExpressionContext context, final IExpression... args) {
    return getHash(args[0].eval(context), "SHA-512");
  }


  public static Value bitget(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    Value v1 = args[1].eval(context);
    if (v1.isNull())
      return Value.NULL;

    return ValueBoolean.of((v0.toInteger() & (1L << v1.toInteger())) != 0);
  }

  /**
   * Returns the number of PI.
   */
  @ScalarFunction(name = "PI", minArgs = 0, maxArgs = 0, category = "i18n::Operator.Category.Mathematical")
  public static Value pi(final IExpressionContext context, final IExpression... args) {
    return ValueNumber.PI;
  }

  /** Returns the absolute (positive) value of the numeric value. */
  @ScalarFunction(name = "ABS", category = "i18n::Operator.Category.Mathematical")
  public static Value abs(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return value;

    if (value.isBigNumber()) {
      return ValueBigNumber.of(value.toBigNumber().abs());
    }
    if (value.isNumber()) {
      return ValueNumber.of(FastMath.abs(value.toNumber()));
    }
    return ValueInteger.of(FastMath.abs(value.toInteger()));
  }

  /** Returns the sign of a number. */
  @ScalarFunction(name = "SIGN", category = "i18n::Operator.Category.Mathematical")
  public static Value sign(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return value;
    return ValueInteger.of(value.signum());
  }

  /** Function to converts radians to degrees. */
  @ScalarFunction(name = "DEGREES", category = "i18n::Operator.Category.Mathematical")
  public static Value degrees(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueNumber.of(FastMath.toDegrees(value.toNumber()));
  }

  /** The function converts degrees to radians. */
  @ScalarFunction(name = "RADIANS", category = "i18n::Operator.Category.Mathematical")
  public static Value radians(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueNumber.of(FastMath.toRadians(value.toNumber()));
  }

  /** Returns the exponential value of a numeric expression. */
  @ScalarFunction(name = "EXP", category = "i18n::Operator.Category.Mathematical")
  public static Value exp(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueNumber.of(FastMath.exp(value.toNumber()));
  }

  /** Returns the values rounded to the nearest equal or larger integer. */
  @ScalarFunction(name = "CEIL", alias = "CEILING",
      category = "i18n::Operator.Category.Mathematical")
  public static Value ceil(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    if (value.isInteger())
      return value;
    if (value.isBigNumber()) {
      return ValueBigNumber.of(value.toBigNumber().setScale(0, RoundingMode.CEILING));
    }
    return ValueNumber.of(FastMath.ceil(value.toNumber()));
  }

  /** Returns the values rounded to the nearest equal or smaller integer. */
  @ScalarFunction(name = "FLOOR", category = "i18n::Operator.Category.Mathematical")
  public static Value floor(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    if (value.isInteger())
      return value;
    if (value.isBigNumber()) {
      return ValueBigNumber.of(value.toBigNumber().setScale(0, RoundingMode.FLOOR));
    }
    return ValueNumber.of(FastMath.floor(value.toNumber()));
  }

  /** Returns the values rounded to the nearest integer. */
  @ScalarFunction(name = "ROUND", category = "i18n::Operator.Category.Mathematical")
  public static Value round(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return value;
    return ValueInteger.of(FastMath.round(value.toNumber()));
  }

  @ScalarFunction(name = "RAND", alias = "RANDOM", deterministic = false, minArgs = 0, maxArgs = 1,category="i18n::Operator.Category.Mathematical")
  public static Value rand(final IExpressionContext context, final IExpression... args) {
    if (args.length == 1) {
      Value value = args[0].eval(context);
      context.getRandom().setSeed(value.toInteger());
    }
    return ValueNumber.of(context.getRandom().nextDouble());
  }


  /**
   * Returns the arc cosine, the angle in radians whose cosine is the specified float expression.
   */
  @ScalarFunction(name = "ACOS", category = "i18n::Operator.Category.Trigonometry")
  public static Value acos(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    Double d = value.toNumber();
    if (d < -1.0 || d > 1.0) {
      throw createArgumentOutOfRangeError(value);
    }
    return ValueNumber.of(FastMath.acos(d));
  }

  @ScalarFunction(name = "ACOSH", category = "i18n::Operator.Category.Trigonometry")
  public static Value acosh(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueNumber.of(FastMath.acosh(value.toNumber()));
  }

  @ScalarFunction(name = "ASINH", category = "i18n::Operator.Category.Trigonometry")
  public static Value asinh(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    return ValueNumber.of(FastMath.asinh(value.toNumber()));
  }

  @ScalarFunction(name = "ATAN", category = "i18n::Operator.Category.Trigonometry")
  public static Value atan(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    return ValueNumber.of(FastMath.atan(value.toNumber()));
  }

  @ScalarFunction(name = "ATANH", category = "i18n::Operator.Category.Trigonometry")
  public static Value atanh(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    return ValueNumber.of(FastMath.atanh(value.toNumber()));
  }

  @ScalarFunction(name = "ATAN2", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Trigonometry")
  public static Value atan2(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    Value v1 = args[1].eval(context);
    if (v1.isNull())
      return Value.NULL;

    return ValueNumber.of(FastMath.atan2(v0.toNumber(), v1.toNumber()));
  }

  /** Returns the trigonometric cosine of the specified angle in radians in the specified number. */
  @ScalarFunction(name = "COS", category = "i18n::Operator.Category.Trigonometry")
  public static Value cos(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueNumber.of(FastMath.cos(value.toNumber()));
  }

  /** Returns the trigonometric cosine of the specified angle in radians in the specified number. */
  @ScalarFunction(name = "COSH", category = "i18n::Operator.Category.Trigonometry")
  public static Value cosh(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueNumber.of(FastMath.cosh(value.toNumber()));
  }

  /** Returns the trigonometric cotangent of the angle in radians specified by float expression. */
  @ScalarFunction(name = "COT", category = "i18n::Operator.Category.Trigonometry")
  public static Value cot(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    double d = FastMath.tan(value.toNumber());
    return ValueNumber.ONE.divide(ValueNumber.of(d));
  }

  @ScalarFunction(name = "ASIN", category = "i18n::Operator.Category.Trigonometry")
  public static Value asin(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueNumber.of(FastMath.asin(value.toNumber()));
  }

  /** 
   * Calculates the trigonometric sine of the angle in radians.
   */
  @ScalarFunction(name = "SIN", category = "i18n::Operator.Category.Trigonometry")
  public static Value sin(final IExpressionContext context, final IExpression... args)
      throws ExpressionException {
    Value value = args[0].eval(context);
    if (value.isNull())
      return value;

    return ValueNumber.of(FastMath.sin(value.toNumber()));
  }

  /** 
   * Calculates the hyperbolic sine of its argument.
   */
  @ScalarFunction(name = "SINH", category = "i18n::Operator.Category.Trigonometry")
  public static Value sinh(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueNumber.of(FastMath.sinh(value.toNumber()));
  }

  /** 
   * Calculates the tangent of its argument, the argument should be expressed in radians.
   */
  @ScalarFunction(name = "TAN", category = "i18n::Operator.Category.Trigonometry")
  public static Value tan(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueNumber.of(FastMath.tan(value.toNumber()));
  }

  /** 
   * Calculates the hyperbolic tangent of its argument.
   */
  @ScalarFunction(name = "TANH", category = "i18n::Operator.Category.Trigonometry")
  public static Value tanh(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueNumber.of(FastMath.tanh(value.toNumber()));
  }
  
  /** 
   * Returns the natural logarithm of a numeric value.
   */
  @ScalarFunction(name = "LN", category = "i18n::Operator.Category.Trigonometry")
  public static Value ln(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    if (value.signum() <= 0)
      throw createArgumentOutOfRangeError(value);
    return ValueNumber.of(FastMath.log(value.toNumber()));
  }

  /** 
   * Returns the specified base logarithm of a numeric value.
   */
  @ScalarFunction(name = "LOG", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Trigonometry")
  public static Value log(final IExpressionContext context, final IExpression... args) {
    Value base = args[0].eval(context);

    if (base.isNull())
      return Value.NULL;

    Value value = args[1].eval(context);
    if (value.isNull())
      return Value.NULL;
    if (value.signum() <= 0)
      throw createArgumentOutOfRangeError(value);

    return ValueNumber.of(FastMath.log(value.toNumber()) / Math.log(base.toNumber()));
  }
  
  /** 
   * Returns the base 10 logarithm of a numeric value.
   */
  @ScalarFunction(name = "LOG10", category = "i18n::Operator.Category.Trigonometry")
  public static Value log10(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    if (value.signum() <= 0)
      throw createArgumentOutOfRangeError(value);
    return ValueNumber.of(FastMath.log10(value.toNumber()));
  }
  
  /** 
   * Returns the cubic root of a numeric expression.
   *  @See {@link #SQRT}
   */
  @ScalarFunction(name = "CBRT", category = "i18n::Operator.Category.Mathematical")
  public static Value cbrt(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueNumber.of(FastMath.cbrt(value.toNumber()));
  }

  /** 
   * Returns the square-root of a non-negative numeric expression. 
   * @See {@link #CBRT}
   */
  @ScalarFunction(name = "SQRT", category = "i18n::Operator.Category.Mathematical")
  public static Value sqrt(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    if (value.signum() < 0)
      throw createArgumentOutOfRangeError(value);
    return ValueNumber.of(FastMath.sqrt(value.toNumber()));
  }


  private static Value getHash(Value value, String algorithm) {
    if (value.isNull())
      return value;

    try {
      MessageDigest md = MessageDigest.getInstance(algorithm);
      md.update(value.toBinary());
      String result = Hex.encodeHexString(md.digest());
      return ValueString.of(result);
    } catch (NoSuchAlgorithmException e) {
      throw new ExpressionException("Unknow algorithm: " + algorithm);
    }
  }

  @ScalarFunction(name = "TRANSLATE", minArgs = 3, maxArgs = 3,category="i18n::Operator.Category.Conversion")
  public static Value translate(final IExpressionContext context, final IExpression... args) {
    Value stringValue = args[0].eval(context);
    Value findCharsValue = args[1].eval(context);
    Value replaceCharsValue = args[2].eval(context);

    if (stringValue.isNull() || findCharsValue.isNull()) {
      return Value.NULL;
    }

    String string = stringValue.toString();
    String findChars = findCharsValue.toString();
    String replaceChars = replaceCharsValue.toString();

    // if it stays null, then no replacements have been made
    StringBuilder buffer = null;
    // if shorter than findChars, then characters are removed
    // (if null, we don't access replaceChars at all)
    int replaceSize = replaceChars == null ? 0 : replaceChars.length();

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
    return ValueString.of(buffer == null ? string : buffer.toString());
  }

  /**
   * The function return the ASCII value of the first character in a string. If the string is empty,
   * a value of 0 is returned.
   */
  @ScalarFunction(name = "ASCII", category = "i18n::Operator.Category.String")
  public static Value ascii(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    String string = value.toString();
    int ascii = 0;
    if (string.length() > 0) {
      ascii = string.charAt(0);
    }
    return ValueInteger.of(ascii);
  }
  
  /**
   * The function converts a Unicode code point (including 7-bit ASCII) into the character that
   * matches the input Unicode. If an invalid code point is specified, an error is returned.
   *
   * @see {@link #ASCII}
   */
  @ScalarFunction(name = "CHR", category = "i18n::Operator.Category.String")
  public static Value chr(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    int codePoint = (int) value.toInteger();

    if (!Character.isValidCodePoint(codePoint)) {
      throw new ExpressionException(
          BaseMessages.getString(PKG, "Expression.ArgumentOutOfRange", codePoint));
    }
    return ValueString.of(new String(Character.toChars(codePoint)));
  }

  /** 
   * Returns a string consisting of a the specified number of blank spaces.
   */
  @ScalarFunction(name = "SPACE", category = "i18n::Operator.Category.String")
  public static Value space(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    int length = Math.max(0, (int) value.toInteger());
    char[] chars = new char[length];
    for (int i = length - 1; i >= 0; i--) {
      chars[i] = ' ';
    }

    return ValueString.of(new String(chars));
  }

  /**
   * The function reverses the order of characters in a string value, or of bytes in a binary value.
   */
  @ScalarFunction(name = "REVERSE", category = "i18n::Operator.Category.String")
  public static Value reverse(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    if (value.isBinary()) {
      byte[] data = value.toBinary();
      byte[] result = new byte[data.length];
      for (int i = data.length - 1, j = 0; i >= 0; i--, j++) {
        result[j] = data[i];
      }
      return ValueBinary.of(result);
    }

    StringBuilder builder = new StringBuilder(value.toString()).reverse();
    return ValueString.of(builder.toString());
  }
  
  /** 
   * Returns a string that contains a phonetic representation of the input string.
   */
  @ScalarFunction(name = "SOUNDEX", category = "i18n::Operator.Category.String")
  public static Value soundex(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueString.of(SOUNDEX.soundex(value.toString()));
  }

  // TODO: @ScalarFunction(name ="DIFFERENCE", category="i18n::Operator.Category.String")
  public static Value difference(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    Value v1 = args[0].eval(context);
    if (v1.isNull())
      return Value.NULL;

    try {
      return ValueInteger.of(SOUNDEX.difference(v0.toString(), v1.toString()));
    } catch (EncoderException e) {
      return Value.NULL;
    }
  }
  
  /**
   * The function return the Unicode code point for the first Unicode character in a string. If the
   * string is empty, a value of 0 is returned.
   *
   * @see {@link #CHR}, {@link #ASCII},
   */
  @ScalarFunction(name = "UNICODE", category = "i18n::Operator.Category.String")
  public static Value unicode(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    String string = value.toString();
    int codePoint = 0;
    if (string.length() > 0) {
      codePoint = string.codePointAt(0);
    }
    return ValueInteger.of(codePoint);
  }

  /**
   * The function left-pads a string with another string, to a certain length.
   *
   * @see {@link #RPAD}
   */
  @ScalarFunction(name = "LPAD", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String")
  public static Value lpad(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    String str = v0.toString();

    Value v1 = args[1].eval(context);
    int length = (int) v1.toInteger();

    // If this parameter is omitted, the function will pad spaces
    String pad = null;
    if (args.length == 3) {
      Value v2 = args[2].eval(context);
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

    return ValueString.of(str);
  }

  /**
   * The function right-pads a string with another string, to a certain length.
   *
   * @see {@link #LPAD}
   */
  @ScalarFunction(name = "RPAD", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String")
  public static Value rpad(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    String str = v0.toString();

    Value v1 = args[1].eval(context);
    int length = (int) v1.toInteger();

    // If this parameter is omitted, the function will pad spaces
    String pad = null;
    if (args.length == 3) {
      Value v2 = args[2].eval(context);
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

    return ValueString.of(str);
  }

  /**
   * The function returns TRUE if the first value starts with second value. Both values must be data
   * type string or binary.
   *
   * @see {@link #ENDSWITH}
   */
  @ScalarFunction(name = "STARTSWITH", minArgs = 2, maxArgs = 2,category="i18n::Operator.Category.Conditional")
  public static Value startswith(final IExpressionContext context, final IExpression... args) {

    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    Value v1 = args[1].eval(context);
    if (v1.isNull())
      return Value.NULL;

    if (v0.isBinary()) {
      byte[] data = v0.toBinary();
      byte[] prefix = v1.toBinary();
      if (prefix.length > data.length) {
        return ValueBoolean.TRUE;
      } else {
        int end = prefix.length;
        for (int i = 0; i < end; i++) {
          if (data[i] != prefix[i]) {
            return ValueBoolean.FALSE;
          }
        }
      }
      return ValueBoolean.TRUE;
    }

    return ValueBoolean.of(v0.toString().startsWith(v1.toString()));
  }

  /**
   * The function returns TRUE if the first value ends with second value. Both values must be data
   * type of string or binary.
   *
   * @see {@link #STARTSWITH}
   */
  @ScalarFunction(name = "ENDSWITH", minArgs = 2, maxArgs = 2,category="i18n::Operator.Category.Conditional")
  public static Value endswith(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    Value v1 = args[1].eval(context);
    if (v1.isNull())
      return Value.NULL;

    if (v0.isBinary()) {
      byte[] data = v0.toBinary();
      byte[] suffix = v1.toBinary();
      int startOffset = data.length - suffix.length;

      if (startOffset < 0) {
        return ValueBoolean.FALSE;
      } else {
        int end = startOffset + suffix.length;
        for (int i = startOffset; i < end; i++) {
          if (data[i] != suffix[i]) {
            return ValueBoolean.FALSE;
          }
        }
      }
      return ValueBoolean.TRUE;
    }

    return ValueBoolean.of(v0.toString().endsWith(v1.toString()));
  }

  private static int makeRegexpFlags(String stringFlags) {
    int flags = 0;
    if (stringFlags != null) {
      for (int i = 0; i < stringFlags.length(); ++i) {
        switch (stringFlags.charAt(i)) {
          case 'i':
            flags |= Pattern.CASE_INSENSITIVE;
            break;
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
  public static Value length(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return value;
    return ValueInteger.of(value.toString().length());
  }

  /** 
   * The function convert a string value to upper case.
   * @See {@link #INITCAP}, {@link #UPPER}
   */
  @ScalarFunction(name = "LOWER", alias = "LCASE", category = "i18n::Operator.Category.String")
  public static Value lower(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueString.of(value.toString().toLowerCase(context.getLocale()));
  }
  
  /** 
   * The function convert a string value to lower case.
   * 
   * @See {@link #LOWER}, {@link #INITCAP}
   */
  @ScalarFunction(name = "UPPER", alias = "UCASE", category = "i18n::Operator.Category.String")
  public static Value upper(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueString.of(value.toString().toUpperCase(context.getLocale()));
  }

  @ScalarFunction(name = "INITCAP", category = "i18n::Operator.Category.String")
  public static Value initcap(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    return ValueString.of(WordUtils.capitalizeFully(value.toString()));
  }

  @ScalarFunction(name = "TRIM", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Value trim(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    String string = value.toString();
    String characters = null;

    if (args.length == 2) {
      Value stripChars = args[1].eval(context);
      if (stripChars.isNull())
        return Value.NULL;

      characters = stripChars.toString();
    }

    return ValueString.of(StringUtils.strip(string, characters));
  }

  @ScalarFunction(name = "LTRIM", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Value ltrim(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    String string = value.toString();
    String characters = null;

    if (args.length == 2) {
      Value stripChars = args[1].eval(context);
      if (stripChars.isNull())
        return Value.NULL;
      characters = stripChars.toString();
    }

    return ValueString.of(StringUtils.stripStart(string, characters));
  }

  @ScalarFunction(name = "RTRIM", minArgs = 1, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Value rtrim(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    String string = value.toString();
    String characters = null;

    if (args.length == 2) {
      Value stripChars = args[1].eval(context);
      if (stripChars.isNull())
        return Value.NULL;
      characters = stripChars.toString();
    }

    return ValueString.of(StringUtils.stripEnd(string, characters));
  }

  @ScalarFunction(name = "LEFT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Value left(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    String s = v0.toString();

    Value v1 = args[1].eval(context);
    if (v1.isNull())
      return Value.NULL;
    int count = (int) v1.toInteger();

    if (count < 0) {
      count = 0;
    } else if (count > s.length()) {
      count = s.length();
    }
    return ValueString.of(s.substring(0, count));
  }

  @ScalarFunction(name = "RIGHT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Value right(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    String s = v0.toString();

    Value v1 = args[1].eval(context);
    if (v1.isNull())
      return Value.NULL;
    int count = (int) v1.toInteger();


    if (count < 0) {
      count = 0;
    } else if (count > s.length()) {
      count = s.length();
    }
    return ValueString.of(s.substring(s.length() - count));
  }

  /**
   * Convert a string to a Java literal using the correct escape sequences. The literal is not
   * enclosed in double quotes.
   */
  @ScalarFunction(name = "STRINGENCODE", category = "i18n::Operator.Category.String")
  public static Value stringencode(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    String s = value.toString();

    StringBuilder builder = new StringBuilder(s.length());
    int length = s.length();
    for (int i = 0; i < length; i++) {
      char c = s.charAt(i);
      switch (c) {
        case '\t':
          // HT horizontal tab
          builder.append("\\t");
          break;
        case '\n':
          // LF linefeed
          builder.append("\\n");
          break;
        case '\f':
          // FF form feed
          builder.append("\\f");
          break;
        case '\r':
          // CR carriage return
          builder.append("\\r");
          break;
        case '"':
          // double quote
          builder.append("\\\"");
          break;
        case '\'':
          builder.append('\'');
          break;
        case '\\':
          // backslash
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

    return ValueString.of(builder.toString());
  }

  /**
   * Convert a string to a Java literal using the correct escape sequences. The literal is not
   * enclosed in double quotes.
   */
  @ScalarFunction(name = "STRINGDECODE", category = "i18n::Operator.Category.String")
  public static Value stringdecode(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    String s = value.toString();

    int length = s.length();
    StringBuilder builder = new StringBuilder(length);
    for (int i = 0; i < length; i++) {
      char c = s.charAt(i);
      if (c == '\\') {
        if (i + 1 >= s.length()) {
          throw createFormatException(s, i);
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
            try {
              c = (char) (Integer.parseInt(s.substring(i + 1, i + 5), 16));
            } catch (NumberFormatException e) {
              throw createFormatException(s, i);
            }
            i += 4;
            builder.append(c);
            break;
          
          default:
            if (c >= '0' && c <= '9') {
              try {
                c = (char) (Integer.parseInt(s.substring(i, i + 3), 8));
              } catch (NumberFormatException e) {
                throw createFormatException(s, i);
              }
              i += 2;
              builder.append(c);
            } else {
              throw createFormatException(s, i);
            }
        }
      } else {
        builder.append(c);
      }
    }

    return ValueString.of(builder.toString());
  }

  @ScalarFunction(name = "URLENCODE", category = "i18n::Operator.Category.String")
  public static Value urlencode(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    try {
      return ValueString.of(URLEncoder.encode(value.toString(), StandardCharsets.UTF_8.name()));
    } catch (Exception e) {
      throw new ExpressionException(BaseMessages.getString(PKG, "Error encoding url"), e);
    }
  }

  @ScalarFunction(name = "URLDECODE", category = "i18n::Operator.Category.String")
  public static Value urldecode(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    try {
      return ValueString.of(URLDecoder.decode(value.toString(), StandardCharsets.UTF_8.name()));
    } catch (Exception e) {
      throw new ExpressionException(BaseMessages.getString(PKG, "Error decoding url"), e);
    }
  }
  
  // -------------------------------------------------------------
  // COMPARISON
  // -------------------------------------------------------------

  @ScalarFunction(name = "CONTAINS", minArgs = 2, maxArgs = 2, category = "i18n::Operator.Category.Comparison")
  public static Value contains(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    Value v1 = args[1].eval(context);

    if (v0.isNull() && v1.isNull())
      return ValueBoolean.TRUE;

    if (v0.isNull() || v1.isNull())
      return ValueBoolean.FALSE;

    if (v0.toString().contains(v1.toString()))
      return ValueBoolean.TRUE;

    return ValueBoolean.FALSE;
  }

  /**
   * Compares whether two expressions are equal.
   *
   * <p>The function is NULL-safe, meaning it treats NULLs as known values for comparing equality.
   * Note that this is different from the EQUAL comparison operator (=), which treats NULLs as
   * unknown values.
   */
  @ScalarFunction(name = "EQUAL_NULL", minArgs = 2, maxArgs = 2, category = "i18n::Operator.Category.Comparison")
  public static Value equal_null(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    Value v1 = args[1].eval(context);

    // Treats NULLs as known values
    if (v0.isNull() && v1.isNull()) {
      return ValueBoolean.TRUE;
    }
    if (v0.isNull() || v1.isNull()) {
      return ValueBoolean.FALSE;
    }

    return ValueBoolean.of(v0.compareTo(v1) == 0);
  }

  // -------------------------------------------------------------
  // CONDITIONAL
  // -------------------------------------------------------------
  
  /**
   * The function returns the smallest value that is not NULL, or NULL if all values are NULL.
   *
   * @see {@link #GREATEST}
   */
  @ScalarFunction(name = "LEAST", minArgs = 1, maxArgs = Integer.MAX_VALUE, category = "i18n::Operator.Category.Conditional")
  public static Value least(final IExpressionContext context, final IExpression... args) {
    Value result = Value.NULL;
    for (IExpression operand : args) {
      Value value = operand.eval(context);
      // null is always smaller
      if (value.isNull())
        continue;
      if (result.isNull() || value.compareTo(result) < 0) {
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
  @ScalarFunction(name = "GREATEST", minArgs = 1, maxArgs = Integer.MAX_VALUE, category = "i18n::Operator.Category.Conditional")
  public static Value greatest(final IExpressionContext context, final IExpression... args) {
    Value result = Value.NULL;
    for (IExpression operand : args) {
      Value value = operand.eval(context);
      if (result.compareTo(value) < 0)
        result = value;
    }

    return result;
  }

  /**
   * Single-level if-then-else expression. Similar to CASE, but only allows a single condition.
   */
  @ScalarFunction(name = "IF", minArgs = 3, maxArgs = 3, category = "i18n::Operator.Category.Conditional")
  public static Value iff(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    return args[(value.toBoolean()) ? 1 : 2].eval(context);
  }

  /**
   * The IFNULL function replace the null with value (Alias NVL).
   */
  @ScalarFunction(name = "IFNULL", alias = "NVL", minArgs = 2, maxArgs = 2, category = "i18n::Operator.Category.Conditional")
  public static Value ifnull(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return args[1].eval(context);
    return value;
  }

  /**
   * The COALESCE function returns the first of its arguments that is not null. Null is returned
   * only if all arguments are null.
   */
  @ScalarFunction(name = "COALESCE", minArgs = 1, maxArgs = Integer.MAX_VALUE, category = "i18n::Operator.Category.Conditional")
  public static Value coalesce(final IExpressionContext context, final IExpression... args) {
    for (IExpression operand : args) {
      Value value = operand.eval(context);
      if (!value.isNull())
        return value;
    }

    return Value.NULL;
  }

  @ScalarFunction(name = "NVL2", minArgs = 3, maxArgs = 3, category = "i18n::Operator.Category.Conditional")
  public static Value nvl2(final IExpressionContext context, final IExpression... args) {
    Value condition = args[0].eval(context);

    if (condition.isNull()) {
      return args[2].eval(context);
    }

    return args[1].eval(context);
  }

  /**
   * Compares the select expression to each search expression in order. As soon as a search
   * expression matches the selection expression, the corresponding result expression is returned.
   */
  @ScalarFunction(name = "DECODE", minArgs = 4, maxArgs = Integer.MAX_VALUE,category="i18n::Operator.Category.String")
  public static Value decode(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);

    int index = -1;
    for (int i = 1, len = args.length - 1; i < len; i += 2) {
      Value search = args[i].eval(context);
      if (value.compareTo(search) == 0) {
        index = i + 1;
        break;
      }
    }
    if (index < 0 && args.length % 2 == 0) {
      index = args.length - 1;
    }
    if (index < 0)
      return Value.NULL;

    return args[index].eval(context);
  }

  @ScalarFunction(name = "NULLIF", minArgs = 2, maxArgs = 2,category="i18n::Operator.Category.Conditional")
  public static Value nullif(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    Value compare = args[1].eval(context);

    if (value.compareTo(compare) == 0)
      return Value.NULL;

    return value;
  }

  /**
   * The function repeats a string as many times as specified.
   */
  @ScalarFunction(name = "REPEAT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.String")
  public static Value repeat(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    Value number = args[1].eval(context);
    if (number.isNull())
      return Value.NULL;

    String s = value.toString();
    int count = (int) number.toInteger();
    StringBuilder builder = new StringBuilder(s.length() * count);
    while (count-- > 0) {
      builder.append(s);
    }
    return ValueString.of(builder.toString());
  }

  /**
   * Removes all occurrences of a specified substring, and optionally replaces them with another
   * string.
   */
  @ScalarFunction(name = "REPLACE", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String")
  public static Value replace(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    Value v1 = args[1].eval(context);
    if (v1.isNull())
      return Value.NULL;

    String string = v0.toString();
    String search = v1.toString();

    if (args.length == 3) {
      Value v2 = args[2].eval(context);

      String replacement = v2.toString();
      return ValueString.of(string.replace(search, replacement));
    }

    String result = StringUtils.remove(string, search);
    return ValueString.of(result);
  }

  /**
   * Returns the position in the string that is the first character of a specified occurrence of the
   * substring.
   */
  @ScalarFunction(name = "INSTR", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String")
  public static Value instr(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    Value v1 = args[1].eval(context);

    if (v0.isNull() || v1.isNull()) {
      return Value.NULL;
    }

    String string = v0.toString();
    String pattern = v1.toString();

    // If 3 operands
    int start = 0;
    if (args.length == 3) {
      start = (int) args[2].eval(context).toInteger();

      if (start > 0)
        start -= 1;
      else if (start < 0) {
        return ValueInteger.of(string.lastIndexOf(pattern, string.length() + start) + 1);
      }
    }

    return ValueInteger.of(string.indexOf(pattern, start) + 1);
  }

  /**
   * Returns the portion of the string from string, startingfrom the character/byte specified by
   * start, with optionally limited length.
   */
  @ScalarFunction(name = "SUBSTRING", alias = {"SUBSTR", "MID"}, minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.String")
  public static Value substring(final IExpressionContext context, final IExpression... args) {
    String string = args[0].eval(context).toString();
    int length = string.length();
    int start = (int) args[1].eval(context).toInteger();

    // These compatibility conditions violate the Standard
    if (start == 0) {
      start = 1;
    } else if (start < 0) {
      start = length + start + 1;
    }

    // Only 2 operands
    if (args.length == 2) {
      return ValueString.of(string.substring(start - 1));
    }

    int end = start + (int) args[2].eval(context).toInteger();
    // SQL Standard requires "data exception - substring error" when
    // end < start but expression does not throw it for compatibility
    start = Math.max(start, 1);
    end = Math.min(end, length + 1);
    if (start > length || end <= start) {
      // TODO: option to treatEmptyStringsAsNull
      return Value.NULL;
    }

    return ValueString.of(string.substring(start - 1, end - 1));
  }

  @ScalarFunction(name = "REGEXP_LIKE", minArgs = 2, maxArgs = 2,category="i18n::Operator.Category.Conditional")
  public static Value regexp_like(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull()) {
      return ValueBoolean.FALSE;
    }
    Value v1 = args[1].eval(context);
    if (v1.isNull()) {
      return ValueBoolean.FALSE;
    }
    Pattern pattern = Pattern.compile(v1.toString(), Pattern.DOTALL);
    return ValueBoolean.of(pattern.matcher(v0.toString()).find());
  }

  /** Converts a value of one data type into another data type if the cast succeeds; otherwise, returns null.*/ 
  @ScalarFunction(name = "TRY_CAST", minArgs = 2, maxArgs = 3,category="i18n::Operator.Category.Conversion")
  public static Value try_cast(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    Value type = args[1].eval(context);

    if (value.isNull() || type.isNull())
      return Value.NULL;

    DataType targetType = DataType.of(type.toString());

    if (args.length == 3) {
      // Format can be ValueNull
      Value format = args[2].eval(context);
      try {
        return value.convertTo(context, targetType, format.toString());
      } catch (Exception e) {
        return Value.NULL;
      }
    }

    try {
      return value.convertTo(targetType);
    } catch (Exception e) {
      return Value.NULL;
    }
  }
  
  /** Converts a string or numeric expression to a boolean value. */
  @ScalarFunction(name = "TO_BOOLEAN",category="i18n::Operator.Category.Conversion")
  public static Value to_boolean(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    return value.convertTo(DataType.BOOLEAN);
  }

  /** Converts a string or numeric expression to a boolean value. */
  @ScalarFunction(name = "TRY_TO_BOOLEAN",category="i18n::Operator.Category.Conversion")
  public static Value try_to_boolean(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    try {
      return value.convertTo(DataType.BOOLEAN);
    } catch (Exception e) {
      return Value.NULL;
    }
  }
  
  /** Converts a numeric or date expression to a string value. */
  @ScalarFunction(name = "TO_CHAR", minArgs = 1, maxArgs = 2,category="i18n::Operator.Category.Conversion")
  public static Value to_char(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    String format = null;
    if (args.length > 1) {
      Value v = args[1].eval(context);
      if (!v.isNull())
        format = v.toString();
    }

    switch (value.getType()) {
      case INTEGER:
      case NUMBER:
      case BIGNUMBER:
        // String str = JavaNumberFormat.format(value.toBigNumber(), format,
        // context.getLocale());
        String str = NumberFormat.format(value.toBigNumber(), format, context.getLocale());
        return ValueString.of(str);
      case DATE:
        ZonedDateTime datetime = value.toDate().atZone(context.getZone());

        if (format == null)
          format = "DD-MON-YY HH.MI.SS.FF PM";

        return ValueString.of(DateFormat.format(datetime, format, context.getLocale()));
      case STRING:
        return value;

      case BINARY:
      case BOOLEAN:
      case NONE:
    }

    throw createUnsupportedDataTypeException(value.getType());
  }

  /** Converts a string expression to a number value. */
  @ScalarFunction(name = "TO_NUMBER", minArgs = 1, maxArgs = 2,category="i18n::Operator.Category.Conversion")
  public static Value to_number(final IExpressionContext context, final IExpression... args)
      throws ParseException {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    // No format
    if (args.length == 1) {
      return ValueBigNumber.of(NumberFormat.parse(value.toString(), null, null));
    }

    Value v1 = args[1].eval(context);
    if (args.length == 2) {
      return ValueBigNumber
          .of(NumberFormat.parse(value.toString(), v1.toString(), context.getLocale()));
    }

    // Precision and scale
    int precision = (int) v1.toInteger();
    Value v2 = args[2].eval(context);
    int scale = (int) v2.toInteger();
    return ValueBigNumber.of(NumberFormat.parse(value.toString(), precision, scale));
  }

  @ScalarFunction(name = "TRY_TO_NUMBER", minArgs = 1, maxArgs = 2,category="i18n::Operator.Category.Conversion")
  public static Value try_to_number(final IExpressionContext context, final IExpression... args)
      throws ParseException {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    try {
      // No format
      if (args.length == 1) {
        return ValueBigNumber.of(NumberFormat.parse(value.toString(), null, null));
      }

      // With format
      if (args.length == 2) {
        Value v1 = args[1].eval(context);

        BigDecimal result =
            NumberFormat.parse(value.toString(), v1.toString(), context.getLocale());
        return ValueBigNumber.of(result);
      }

      // Precision and scale
      if (args.length == 3) {
        Value v1 = args[1].eval(context);
        int precision = (int) v1.toInteger();
        Value v2 = args[2].eval(context);
        int scale = (int) v2.toInteger();


        BigDecimal result = NumberFormat.parse(value.toString(), precision, scale);
        return ValueBigNumber.of(result);
      }
    } catch (RuntimeException e) {
      // Ignore
    }

    return Value.NULL;
  }
  
  /** Converts a string expression to a date value. */
  @ScalarFunction(name = "TO_DATE", minArgs = 1, maxArgs = 2,category="i18n::Operator.Category.Conversion")
  public static Value to_date(final IExpressionContext context, final IExpression... args)
      throws ParseException {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    String format = null;
    if (args.length > 1) {
      Value v1 = args[1].eval(context);
      if (!v1.isNull())
        format = v1.toString();
    }

    switch (value.getType()) {
      case DATE:
        return value;
      case STRING:
        Instant instant = DateFormat.parse(value.toString(), format, context.getLocale());
        return ValueDate.of(instant);
    }

    throw createUnsupportedDataTypeException(value.getType());
  }

  @ScalarFunction(name = "TRY_TO_DATE", minArgs = 1, maxArgs = 2,category="i18n::Operator.Category.Conversion")
  public static Value try_to_date(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    String format = null;
    if (args.length > 1) {
      Value v1 = args[1].eval(context);
      if (!v1.isNull())
        format = v1.toString();
    }

    switch (value.getType()) {
      case DATE:
        return value;
      case STRING:
        try {
          Instant instant = DateFormat.parse(value.toString(), format, context.getLocale());
          return ValueDate.of(instant);
        } catch (ParseException | RuntimeException e) {
          // Ignore
        }
    }

    return Value.NULL;
  }

  @ScalarFunction(name = "TRUNCATE", alias = "TRUNC", minArgs = 1, maxArgs = 2,category="i18n::Operator.Category.Mathematical")
  public static Value truncate(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    switch (value.getType()) {
      case INTEGER:
      case NUMBER:
      case BIGNUMBER:
        BigDecimal number = value.toBigNumber();
        int scale = 0;
        if (args.length == 2) {
          Value pattern = args[1].eval(context);
          if (pattern.isNull())
            return Value.NULL;
          scale = (int) pattern.toInteger();
        }

        if (scale > number.scale())
          scale = number.scale();
        return ValueBigNumber
            .of(number.movePointRight(scale).setScale(0, RoundingMode.DOWN).movePointLeft(scale));

      case DATE:
        Instant instant = value.toDate();
        DatePart datePart = DatePart.DAY;

        if (args.length == 2) {
          Value pattern = args[1].eval(context);
          if (pattern.isNull())
            return Value.NULL;
          datePart = DatePart.of(pattern.toString());
        }

        switch (datePart) {
          // First day of the year
          case YEAR: {
            ZonedDateTime datetime =
                ZonedDateTime.ofInstant(instant.truncatedTo(ChronoUnit.DAYS), context.getZone());
            return ValueDate.of(datetime.withDayOfYear(1).toInstant());
          }
          // First day of the month
          case MONTH: {
            ZonedDateTime datetime =
                ZonedDateTime.ofInstant(instant.truncatedTo(ChronoUnit.DAYS), context.getZone());
            return ValueDate.of(datetime.withDayOfMonth(1).toInstant());
          }
          // First day of the quarter
          case QUARTER: {
            ZonedDateTime datetime =
                ZonedDateTime.ofInstant(instant.truncatedTo(ChronoUnit.DAYS), context.getZone());
            int month = (datetime.getMonthValue() / 3) * 3 + 1;
            return ValueDate.of(datetime.withMonth(month).withDayOfMonth(1).toInstant());
          }
          // First day of the week
          case WEEK: {
            ZonedDateTime datetime =
                ZonedDateTime.ofInstant(instant.truncatedTo(ChronoUnit.DAYS), context.getZone());

            final Calendar calendar = Calendar.getInstance(context.getLocale());
            DayOfWeek dow = DayOfWeek.of(calendar.getFirstDayOfWeek());

            return ValueDate.of(datetime.with(TemporalAdjusters.previousOrSame(dow)).toInstant());
          }

          case DAY:
            return ValueDate.of(instant.truncatedTo(ChronoUnit.DAYS));
          case HOUR:
            return ValueDate.of(instant.truncatedTo(ChronoUnit.HOURS));
          case MINUTE:
            return ValueDate.of(instant.truncatedTo(ChronoUnit.MINUTES));
          case SECOND:
            return ValueDate.of(instant.truncatedTo(ChronoUnit.SECONDS));
          case MILLISECOND:
            return ValueDate.of(instant.truncatedTo(ChronoUnit.MILLIS));
          case MICROSECOND:
            return ValueDate.of(instant.truncatedTo(ChronoUnit.MICROS));
          case NANOSECOND:
            return ValueDate.of(instant.truncatedTo(ChronoUnit.NANOS));
        }

        return value;

      case STRING:
      case BOOLEAN:
      default:
        throw new ExpressionException("Truncate data type not implemented");
    }
  }

  // -------------------------------------------------------------
  // DATE AND TIME
  // -------------------------------------------------------------

  /** DATE function */
  @ScalarFunction(name = "DATE", minArgs = 3, maxArgs = 3,
      category = "i18n::Operator.Category.Date")
  public static Value date(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;

    Value v1 = args[1].eval(context);
    if (v1.isNull())
      return Value.NULL;

    Value v2 = args[2].eval(context);
    if (v2.isNull())
      return Value.NULL;

    int year = (int) v0.toInteger();
    int month = (int) v1.toInteger();
    int day = (int) v2.toInteger();

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

    return ValueDate.of(date.atStartOfDay(ZoneOffset.UTC).toInstant());
  }

  /**
   * Function to extract date part: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
   */
  @ScalarFunction(name = "EXTRACT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value extract(final IExpressionContext context, final IExpression... args) {
    Value part = args[0].eval(context);
    if (part.isNull())
      return Value.NULL;

    Value value = args[1].eval(context);
    if (value.isNull())
      return Value.NULL;

    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
    DatePart datePart = DatePart.of(part.toString());

    return ValueInteger.of(datePart.get(dt));
  }

  /** Day of the month (number from 1-31). */
  @ScalarFunction(name = "DAY", alias = "DAYOFMONTH", category = "i18n::Operator.Category.Date")
  public static Value day(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
    return ValueInteger.of(dt.getDayOfMonth());
  }

  /** Returns the name of the weekday (in English). */
  @ScalarFunction(name = "DAYNAME", category = "i18n::Operator.Category.Date")
  public static Value dayname(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    DayOfWeek weekday = ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getDayOfWeek();
    return ValueString.of(weekday.getDisplayName(TextStyle.FULL, Locale.ENGLISH));
  }

  /** Day of the week (Sunday=1 to Saturday=7). */
  @ScalarFunction(name = "DAYOFWEEK", category = "i18n::Operator.Category.Date")
  public static Value dayofweek(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    DayOfWeek dow = ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getDayOfWeek();

    int result = dow.getValue() + 1;
    if (result == 8)
      result = 1;

    return ValueInteger.of(result);
  }

  /** Day of the week (Monday=1 to Sunday=7). */
  @ScalarFunction(name = "DAYOFWEEK_ISO", category = "i18n::Operator.Category.Date")
  public static Value dayofweek_iso(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    DayOfWeek dow = ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getDayOfWeek();
    return ValueInteger.of(dow.getValue());

    // ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
    // return Value.of(DatePart.DAYOFWEEKISO.get(dt));
  }

  /** Day of the year (number from 1-366). */
  @ScalarFunction(name = "DAYOFYEAR", category = "i18n::Operator.Category.Date")
  public static Value dayofyear(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
    return ValueInteger.of(dt.getDayOfYear());
  }

  /** Week of the year (number from 1-54). */
  @ScalarFunction(name = "WEEK", category = "i18n::Operator.Category.Date")
  public static Value week(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
    return ValueInteger.of(dt.get(ChronoField.ALIGNED_WEEK_OF_YEAR));
  }

  /** Week from the beginning of the month (0-5) */
  @ScalarFunction(name = "WEEKOFMONTH", category = "i18n::Operator.Category.Date")
  public static Value weekofmonth(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
    return ValueInteger.of(dt.get(ChronoField.ALIGNED_WEEK_OF_MONTH));
  }

  /** Month of the year (number from 1-12). */
  @ScalarFunction(name = "MONTH", category = "i18n::Operator.Category.Date")
  public static Value month(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
    return ValueInteger.of(dt.getMonthValue());
  }

  /** Returns the name of the month (in English). */
  @ScalarFunction(name = "MONTHNAME", category = "i18n::Operator.Category.Date")
  public static Value monthname(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    Month month = ZonedDateTime.ofInstant(value.toDate(), context.getZone()).getMonth();
    return ValueString.of(month.getDisplayName(TextStyle.FULL, Locale.ENGLISH));
  }
  
  /** Quarter of the year (number from 1-4). */
  @ScalarFunction(name = "QUARTER", category = "i18n::Operator.Category.Date")
  public static Value quarter(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
    return ValueInteger.of(dt.get(IsoFields.QUARTER_OF_YEAR));
  }

  /** The year of a date */
  @ScalarFunction(name = "YEAR", category = "i18n::Operator.Category.Date")
  public static Value year(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone());
    return ValueInteger.of(dt.getYear());
  }

  /** The hour (0-23). @See {@link #MINUTE}, {@link #SECOND} */
  @ScalarFunction(name = "HOUR", category = "i18n::Operator.Category.Date")
  public static Value hour(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    LocalTime time = LocalTime.from(value.toDate().atZone(context.getZone()));
    return ValueInteger.of(time.getHour());
  }

  /** The minute (0-59). @See {@link #HOUR}, {@link #SECOND} */
  @ScalarFunction(name = "MINUTE", category = "i18n::Operator.Category.Date")
  public static Value minute(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    LocalTime time = LocalTime.from(value.toDate().atZone(context.getZone()));
    return ValueInteger.of(time.getMinute());
  }
  
  /** The second (0-59). @See {@link #HOUR}, {@link #MINUTE} */
  @ScalarFunction(name = "SECOND", category = "i18n::Operator.Category.Date")
  public static Value second(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    LocalTime time = LocalTime.from(value.toDate().atZone(context.getZone()));
    return ValueInteger.of(time.getSecond());
  }

  /** Adds or subtracts a specified number of days to a date or timestamp */
  @ScalarFunction(name = "ADD_DAYS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value add_days(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return value;
    Value days = args[1].eval(context);
    if (days.isNull())
      return Value.NULL;

    ZonedDateTime dt =
        ZonedDateTime.ofInstant(value.toDate(), context.getZone()).plusDays(days.toInteger());
    return ValueDate.of(dt.toInstant());
  }

  /** Adds or subtracts a specified number of weeks to a date or timestamp */
  @ScalarFunction(name = "ADD_WEEKS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value add_weeks(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    Value weeks = args[1].eval(context);
    if (weeks.isNull())
      return Value.NULL;

    ZonedDateTime dt =
        ZonedDateTime.ofInstant(value.toDate(), context.getZone()).plusWeeks(weeks.toInteger());
    return ValueDate.of(dt.toInstant());
  }

  /** Adds or subtracts a specified number of months to a date or timestamp */
  @ScalarFunction(name = "ADD_MONTHS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value add_months(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    Value months = args[1].eval(context);
    if (months.isNull())
      return Value.NULL;

    ZonedDateTime dt =
        ZonedDateTime.ofInstant(value.toDate(), context.getZone()).plusMonths(months.toInteger());
    return ValueDate.of(dt.toInstant());
  }

  /** Adds or subtracts a specified number of months to a date or timestamp */
  @ScalarFunction(name = "ADD_YEARS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value add_years(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    Value years = args[1].eval(context);
    if (years.isNull())
      return Value.NULL;

    ZonedDateTime dt =
        ZonedDateTime.ofInstant(value.toDate(), context.getZone()).plusYears(years.toInteger());
    return ValueDate.of(dt.toInstant());
  }
  
  /** Adds or subtracts a specified number of hours to a date or timestamp */
  @ScalarFunction(name = "ADD_HOURS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value add_hours(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return value;
    Value hours = args[1].eval(context);
    if (hours.isNull())
      return Value.NULL;

    ZonedDateTime dt =
        ZonedDateTime.ofInstant(value.toDate(), context.getZone()).plusHours(hours.toInteger());
    return ValueDate.of(dt.toInstant());
  }
  
  /** Adds or subtracts a specified number of minutes to a date or timestamp */
  @ScalarFunction(name = "ADD_MINUTES", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value add_minutes(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return value;
    Value minutes = args[1].eval(context);
    if (minutes.isNull())
      return Value.NULL;

    ZonedDateTime dt =
        ZonedDateTime.ofInstant(value.toDate(), context.getZone()).plusMinutes(minutes.toInteger());
    return ValueDate.of(dt.toInstant());
  }
  
  /** Adds or subtracts a specified number of seconds to a date or timestamp */
  @ScalarFunction(name = "ADD_SECONDS", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value add_seconds(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;
    Value seconds = args[1].eval(context);
    if (seconds.isNull())
      return Value.NULL;

    ZonedDateTime dt =
        ZonedDateTime.ofInstant(value.toDate(), context.getZone()).plusSeconds(seconds.toInteger());
    return ValueDate.of(dt.toInstant());
  }

  @ScalarFunction(name = "DAYS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value days_between(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    Value v1 = args[1].eval(context);
    if (v1.isNull())
      return Value.NULL;

    LocalDate startDate = v0.toDate().atZone(context.getZone()).toLocalDate();
    LocalDate endDate = v1.toDate().atZone(context.getZone()).toLocalDate();
    long days = startDate.until(endDate, ChronoUnit.DAYS);
    return ValueInteger.of(days);
  }

  @ScalarFunction(name = "MONTHS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value months_between(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    Value v1 = args[1].eval(context);
    if (v1.isNull())
      return Value.NULL;

    LocalDate startDate = v0.toDate().atZone(context.getZone()).toLocalDate();
    LocalDate endDate = v1.toDate().atZone(context.getZone()).toLocalDate();
    long days = startDate.until(endDate, ChronoUnit.DAYS);
    return ValueNumber.of(days / 31d);
    // long months = startDate.until(endDate, ChronoUnit.MONTHS);
    // return Value.of(months);
  }

  @ScalarFunction(name = "YEARS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value years_between(final IExpressionContext context, final IExpression... args) {
    Value v0 = args[0].eval(context);
    if (v0.isNull())
      return Value.NULL;
    Value v1 = args[1].eval(context);
    if (v1.isNull())
      return Value.NULL;

    LocalDate startDate = v0.toDate().atZone(context.getZone()).toLocalDate();
    LocalDate endDate = v1.toDate().atZone(context.getZone()).toLocalDate();
    long years = startDate.until(endDate, ChronoUnit.YEARS);
    return ValueInteger.of(years);
  }

  @ScalarFunction(name = "HOURS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value hours_between(final IExpressionContext context, final IExpression... args) {
    Value value1 = args[0].eval(context);
    if (value1.isNull())
      return Value.NULL;
    Value value2 = args[1].eval(context);
    if (value2.isNull())
      return Value.NULL;

    LocalDateTime startDateTime = value1.toDate().atZone(context.getZone()).toLocalDateTime();
    LocalDateTime endDateTime = value2.toDate().atZone(context.getZone()).toLocalDateTime();
    long hours = startDateTime.until(endDateTime, ChronoUnit.HOURS);
    return ValueInteger.of(hours);
  }

  @ScalarFunction(name = "MINUTES_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value minutes_between(final IExpressionContext context, final IExpression... args) {
    Value value1 = args[0].eval(context);
    if (value1.isNull())
      return Value.NULL;
    Value value2 = args[1].eval(context);
    if (value2.isNull())
      return Value.NULL;

    LocalDateTime startDateTime = value1.toDate().atZone(context.getZone()).toLocalDateTime();
    LocalDateTime endDateTime = value2.toDate().atZone(context.getZone()).toLocalDateTime();
    long minutes = startDateTime.until(endDateTime, ChronoUnit.MINUTES);
    return ValueInteger.of(minutes);
  }


  @ScalarFunction(name = "SECONDS_BETWEEN", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value seconds_between(final IExpressionContext context, final IExpression... args) {
    Value value1 = args[0].eval(context);
    if (value1.isNull())
      return Value.NULL;
    Value value2 = args[1].eval(context);
    if (value2.isNull())
      return Value.NULL;

    LocalDateTime startDateTime = value1.toDate().atZone(context.getZone()).toLocalDateTime();
    LocalDateTime endDateTime = value2.toDate().atZone(context.getZone()).toLocalDateTime();
    long seconds = startDateTime.until(endDateTime, ChronoUnit.SECONDS);
    return ValueInteger.of(seconds);
  }

  /** Returns the first day of the month. */
  @ScalarFunction(name = "FIRST_DAY", category = "i18n::Operator.Category.Date")
  public static Value first_day(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
        .with(TemporalAdjusters.firstDayOfMonth());
    return ValueDate.of(dt.toInstant());
  }
  
  /** Returns the last day of the month. */
  @ScalarFunction(name = "LAST_DAY", category = "i18n::Operator.Category.Date")
  public static Value last_day(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
        .with(TemporalAdjusters.lastDayOfMonth());
    return ValueDate.of(dt.toInstant());
  }
  
  /** Returns the date of the first specified day of week that occurs after the input date. */
  @ScalarFunction(name = "NEXT_DAY", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value next_day(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    Value dow = args[1].eval(context);
    if (dow.isNull())
      return Value.NULL;

    DayOfWeek dayofweek = DayOfWeek.valueOf(dow.toString().toUpperCase());
    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
        .with(TemporalAdjusters.next(dayofweek));

    return ValueDate.of(dt.toInstant());
  }
  
  /** Returns the date of the first specified day of week that occurs before the input date. */
  @ScalarFunction(name = "PREVIOUS_DAY", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Date")
  public static Value previous_day(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    Value dow = args[1].eval(context);
    if (dow.isNull())
      return Value.NULL;

    DayOfWeek dayofweek = DayOfWeek.valueOf(dow.toString().toUpperCase());
    ZonedDateTime dt = ZonedDateTime.ofInstant(value.toDate(), context.getZone())
        .with(TemporalAdjusters.previous(dayofweek));

    return ValueDate.of(dt.toInstant());
  }


  private static ExpressionException createFormatException(String s, int i) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Bad format {0} at position {1}", s, i));
  }

  private static ExpressionException createUnsupportedDataTypeException(DataType type) {
    return new ExpressionException(BaseMessages.getString(PKG, "Unsuported data type {0}", type));
  }

  public Method getMethod() {
    return method;
  }
}

