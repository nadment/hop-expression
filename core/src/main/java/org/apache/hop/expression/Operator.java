/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression;

import java.io.InputStreamReader;
import java.io.StringWriter;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;
import org.apache.hop.i18n.BaseMessages;

// TODO: implement RLIKE operator

/**
 * Operators have the precedence levels. An operator on higher levels is evaluated before an
 * operator on a lower level
 *
 * @author Nicolas ADMENT
 */
public class Operator implements Comparable<Operator> {

  protected static final Class<?> PKG = Expression.class; // for i18n purposes

  private static final String JAVA_REGEX_SPECIALS = "\\.[]{}()<>*+-=!?^$|";

  private static final ConcurrentHashMap<Kind, String> docs = new ConcurrentHashMap<>();

  /** Set of functions or alias by name. */
  private static final HashMap<String, Function> FUNCTIONS_BY_NAME = new HashMap<>(256);

  // -------------------------------------------------------------
  // BITWISE OPERATORS
  // -------------------------------------------------------------

  /** Bitwise AND operator "&". */
  public static final Operator BITAND = new Operator(Kind.BITAND, "&", 70, true, true);

  /** Bitwise OR operator "|". */
  public static final Operator BITOR = new Operator(Kind.BITOR, "|", 90, true, true);

  /** Bitwise NOT operator "~". */
  public static final Operator BITNOT = new Operator(Kind.BITNOT, "~", 40, true, true);

  /** Bitwise XOR operator "^". */
  public static final Operator BITXOR = new Operator(Kind.BITXOR, "^", 80, true, true);

  // -------------------------------------------------------------
  // LOGICAL OPERATORS
  // -------------------------------------------------------------

  /**
   * Logical negation <code>NOT</code> operator
   *
   * <p>Syntax of the operator:
   *
   * <ul>
   *   <li><code>field [NOT] TRUE</code>
   *   <li><code>field [NOT] IN (list of values)</code>
   *   <li><code>field [NOT] BETWEEN start AND end</code>
   * </ul>
   */
  public static final Operator BOOLNOT = new Operator(Kind.LOGICAL_NOT, "NOT", 150, false, false);
  /** Logical disjunction <code>OR</code> operator. */
  public static final Operator BOOLOR = new Operator(Kind.LOGICAL_OR, "OR", 180, true, false);
  /** Logical conjunction <code>AND</code> operator. */
  public static final Operator BOOLAND = new Operator(Kind.LOGICAL_AND, "AND", 160, true, false);
  /** Logical <code>XOR</code> operator. */
  public static final Operator BOOLXOR = new Operator(Kind.LOGICAL_XOR, "XOR", 170, true, false);

  /**
   * An operator describing the <code>IS</code> operator.
   *
   * <p>Syntax of the operator:
   *
   * <ul>
   *   <li><code>field IS TRUE</code>
   *   <li><code>field IS FALSE</code>
   *   <li><code>field IS NULL</code>
   * </ul>
   */
  public static final Operator IS = new Operator(Kind.IS, "IS", 140, true, false);

  /**
   * Logical <code>IN</code> operator tests for a value's membership in a list of values. The IN
   * operator is a shorthand for multiple OR conditions.
   *
   * <p>Syntax of the operator:
   *
   * <ul>
   *   <li><code>field [NOT] IN list of values</code>
   * </ul>
   *
   * <p><b>NOTE</b> If the <code>NOT</code> clause is present the {@link
   * org.apache.hop.core.ExpressionParser parser} will generate a equivalent to <code>
   * NOT (field IN list of values ...)</code>
   */
  public static final Operator IN = new Operator(Kind.IN, "IN", 120, true, false);
  /**
   * An operator describing the <code>LIKE</code> operator.
   *
   * <p>Syntax of the operator:
   *
   * <ul>
   *   <li><code>field [NOT] LIKE pattern ESCAPE char</code>
   * </ul>
   *
   * <p><b>NOTE</b> If the <code>NOT</code> clause is present the {@link
   * org.hop.expresssion.ExpressionParser} parser will generate a equivalent to <code>
   * NOT (field LIKE pattern ...)</code>
   */
  public static final Operator LIKE = new Operator(Kind.LIKE, "LIKE", 120, true, false);

  public static final Operator ILIKE = new Operator(Kind.ILIKE, "ILIKE", 120, true, false);

  public static final Operator BETWEEN = new Operator(Kind.BETWEEN, "BETWEEN", 120, true, false);

  // -------------------------------------------------------------
  // COMPARISON OPERATORS
  // -------------------------------------------------------------

  /** Comparison equals operator '<code>=</code>'. */
  public static final Operator EQUALS = new Operator(Kind.EQUAL, "=", 130, true, false);
  /** Comparison not equals operator '<code>!=</code>'. */
  public static final Operator NOT_EQUALS = new Operator(Kind.NOT_EQUALS, "!=", 130, true, true);
  /** Comparison not equals operator '<code><></code>'. */
  public static final Operator LESS_THAN_OR_GREATER_THAN =
      new Operator(Kind.NOT_EQUALS, "<>", 130, true, false);
  /** Comparison less-than operator '<code>&lt;</code>'. */
  public static final Operator LESS_THAN = new Operator(Kind.LESS_THAN, "<", 130, true, false);
  /** Comparison less-than-or-equal operator '<code>&lt;=</code>'. */
  public static final Operator LESS_THAN_OR_EQUAL =
      new Operator(Kind.LESS_THAN_OR_EQUAL, "<=", 130, true, false);
  /** Comparison greater-than operator '<code>&gt;</code>'. */
  public static final Operator GREATER_THAN =
      new Operator(Kind.GREATER_THAN, ">", 130, true, false);
  /** Comparison greater-than-or-equal operator '<code>&gt;=</code>'. */
  public static final Operator GREATER_THAN_OR_EQUAL =
      new Operator(Kind.GREATER_THAN_OR_EQUAL, ">=", 130, true, false);

  // -------------------------------------------------------------
  // ARITHMETIC OPERATORS
  // -------------------------------------------------------------

  /** Arithmetic unary negative operator '<code>-</code>'. */
  public static final Operator NEGATIVE = new Operator(Kind.NEGATIVE, "-", 30, true, false);

  /** Arithmetic power operator '<code>**</code>'. */
  public static final Operator POWER = new Operator(Kind.POWER, "**", 70, true, true);

  /** Arithmetic multiplication operator '<code>*</code>'. */
  public static final Operator MULTIPLY = new Operator(Kind.MULTIPLY, "*", 50, true, true);

  /** Arithmetic division operator '<code>/</code>'. */
  public static final Operator DIVIDE = new Operator(Kind.DIVIDE, "/", 50, true, true);

  /** Arithmetic modulus operator '<code>%</code>'. */
  public static final Operator MODULUS = new Operator(Kind.MOD, "%", 50, true, true);

  /** Arithmetic addition operator '<code>+</code>'. */
  public static final Operator ADD = new Operator(Kind.ADD, "+", 100, true, true);

  /** Arithmetic subtraction operator '<code>-</code>'. */
  public static final Operator SUBTRACT = new Operator(Kind.SUBTRACT, "-", 100, true, true);

  // -------------------------------------------------------------
  // SPECIAL OPERATORS
  // -------------------------------------------------------------

  /** An operator describing the <code>CASE</code> operator. */
  public static final Operator CASE = new Operator(Kind.CASE_WHEN, "CASE", 120, true, false);

  /** String concatenation operator '<code>||</code>'. */
  public static final Operator CONCAT = new Operator(Kind.CONCAT, "||", 110, true, true);

  /** Set of operators. */
  private static final Set<Operator> operators =
      new TreeSet<>(
          Arrays.asList(
              ADD,
              SUBTRACT,
              MULTIPLY,
              DIVIDE,
              POWER,
              BITAND,
              BITOR,
              BITNOT,
              BITXOR,
              MODULUS,
              EQUALS,
              GREATER_THAN,
              GREATER_THAN_OR_EQUAL,
              ILIKE,
              LESS_THAN,
              LESS_THAN_OR_EQUAL,
              LESS_THAN_OR_GREATER_THAN,
              NOT_EQUALS,
              BOOLAND,
              BETWEEN,
              CASE,
              CONCAT,
              IN,
              IS,
              LIKE,
              BOOLNOT,
              BOOLOR,
              BOOLXOR));

  public static Set<Operator> getOperators() {
    return operators;
  }

  // -------------------------------------------------------------
  // FUNCTIONS
  // -------------------------------------------------------------
  static {
    addFunction(Kind.ABS);
    addFunction(Kind.ADD);
    addFunction(Kind.ADD_DAYS);
    addFunction(Kind.ADD_HOURS);
    addFunction(Kind.ADD_MINUTES);
    addFunction(Kind.ADD_MONTHS);
    addFunction(Kind.ADD_SECONDS);
    addFunction(Kind.ADD_WEEKS);
    addFunction(Kind.ADD_YEARS);
    addFunction(Kind.ACOS);
    addFunction(Kind.ACOSH);
    addFunction(Kind.ASCII);
    addFunction(Kind.ASIN);
    addFunction(Kind.ASINH);
    addFunction(Kind.ATAN);
    addFunction(Kind.ATANH);
    addFunction(Kind.ATAN2);
    addFunction(Kind.BITAND);
    addFunction(Kind.BITGET);
    addFunction(Kind.BITNOT);
    addFunction(Kind.BITOR);
    addFunction(Kind.BITXOR);
    addFunction(Kind.CAST);
    addFunction(Kind.CBRT);
    addFunction(Kind.CEIL, "CEILING");
    addFunction(Kind.CHR);
    addFunction(Kind.COALESCE);
    addFunction(Kind.CONCAT);
    addFunction(Kind.COS);
    addFunction(Kind.COSH);
    addFunction(Kind.COT);
    addFunction(Kind.CONTAINS);
    addFunctionNotDeterministic(Kind.CURRENT_DATE, "SYSDATE");
    addFunction(Kind.DATE);
    addFunction(Kind.DAYNAME);
    addFunction(Kind.DAY, "DAYOFMONTH");
    addFunction(Kind.DAYOFWEEK);
    addFunction(Kind.DAYOFWEEK_ISO);
    addFunction(Kind.DAYOFYEAR);
    addFunction(Kind.DAYS_BETWEEN);
    addFunction(Kind.DECODE);
    addFunction(Kind.DEGREES);
    addFunction(Kind.DIVIDE);
    addFunction(Kind.EXTRACT);
    addFunction(Kind.EQUAL_NULL);
    addFunction(Kind.ENDSWITH);
    addFunction(Kind.EXP);
    addFunction(Kind.FIRST_DAY);
    addFunction(Kind.FLOOR);
    addFunction(Kind.GREATEST);
    addFunction(Kind.HOUR);
    addFunction(Kind.HOURS_BETWEEN);
    addFunction(Kind.IF);
    addFunction(Kind.IFNULL, "NVL");
    addFunction(Kind.INITCAP);
    addFunction(Kind.INSTR);
    addFunction(Kind.LAST_DAY);
    addFunction(Kind.LEAST);
    addFunction(Kind.LEFT);
    addFunction(Kind.LENGTH);
    addFunction(Kind.LN);
    addFunction(Kind.LOG);
    addFunction(Kind.LOG10);
    addFunction(Kind.LOWER, "LCASE");
    addFunction(Kind.LPAD);
    addFunction(Kind.LTRIM);
    addFunction(Kind.MD5);
    addFunction(Kind.MINUTE);
    addFunction(Kind.MINUTES_BETWEEN);
    addFunction(Kind.MOD);
    addFunction(Kind.MONTH);
    addFunction(Kind.MONTHNAME);
    addFunction(Kind.MONTHS_BETWEEN);
    addFunction(Kind.MULTIPLY);
    addFunction(Kind.NEXT_DAY);
    addFunction(Kind.NULLIF);
    addFunction(Kind.NVL2);
    addFunction(Kind.PI);
    addFunction(Kind.POWER);
    addFunction(Kind.QUARTER);
    addFunction(Kind.RADIANS);
    addFunctionNotDeterministic(Kind.RAND);
    addFunction(Kind.REGEXP_LIKE);
    addFunction(Kind.REPEAT);
    addFunction(Kind.REPLACE);
    addFunction(Kind.REVERSE);
    addFunction(Kind.RIGHT);
    addFunction(Kind.ROUND);
    addFunction(Kind.RPAD);
    addFunction(Kind.RTRIM);
    addFunction(Kind.SHA1);
    addFunction(Kind.SHA256);
    addFunction(Kind.SHA384);
    addFunction(Kind.SHA512);
    addFunction(Kind.SECOND);
    addFunction(Kind.SECONDS_BETWEEN);
    addFunction(Kind.SIGN);
    addFunction(Kind.SIN);
    addFunction(Kind.SINH);
    addFunction(Kind.SOUNDEX);
    addFunction(Kind.SPACE);
    addFunction(Kind.SQRT);
    addFunction(Kind.STARTSWITH);
    addFunction(Kind.STRINGDECODE);
    addFunction(Kind.STRINGENCODE);
    addFunction(Kind.SUBSTRING, "SUBSTR", "MID");
    addFunction(Kind.SUBTRACT);
    addFunction(Kind.TAN);
    addFunction(Kind.TANH);
    addFunction(Kind.TO_BOOLEAN);
    addFunction(Kind.TO_CHAR);
    addFunction(Kind.TO_DATE);
    addFunction(Kind.TO_NUMBER);
    addFunction(Kind.TRIM);
    addFunction(Kind.TRUNCATE, "TRUNC");
    addFunction(Kind.TRANSLATE);
    addFunction(Kind.UNICODE);
    addFunction(Kind.UPPER, "UCASE");
    addFunction(Kind.URLDECODE);
    addFunction(Kind.URLENCODE);
    addFunction(Kind.WEEK);
    addFunction(Kind.WEEKOFMONTH);
    addFunction(Kind.WEEK_ISO);
    addFunction(Kind.YEAR);
    addFunction(Kind.YEARS_BETWEEN);
  }

  public static Function getFunction(final Kind kind) {
    if (kind == null) return null;

    return getFunction(kind.name());
  }

  public static Function getFunction(final String name) {
    if (name == null) return null;

    return FUNCTIONS_BY_NAME.get(name.toUpperCase());
  }

  /**
   * Register operator like function
   *
   * @param operator
   */
  protected static void register(Operator operator) {
    operators.add(operator);
  }

  protected static void addFunction(Kind kind) {
    createFunction(kind, kind.name(), false, true);
  }

  protected static void addFunction(Kind kind, String... alias) {
    createFunction(kind, kind.name(), false, true);
    for (String name : alias) {
      createFunction(kind, name, true, true);
    }
  }

  protected static void addFunctionNotDeterministic(Kind kind) {
    addFunctionNotDeterministic(kind, kind.name());
  }

  protected static void addFunctionNotDeterministic(Kind kind, String... alias) {
    createFunction(kind, kind.name(), false, false);
    for (String name : alias) {
      createFunction(kind, name, true, false);
    }
  }

  private static void createFunction(
      Kind kind, String name, boolean isAlias, boolean isDeterministic) {
    Function function = new Function(kind, name, isAlias, isDeterministic);
    register(function);
    FUNCTIONS_BY_NAME.put(name, function);
  }

  public static String getHtmlDocumentation(Kind kind) {
    String doc = docs.get(kind);
    if (doc != null) {
      return doc;
    }

    doc = readAsciidoc(kind);
    docs.put(kind, doc);

    return doc;
  }

  private static String readAsciidoc(Kind kind) {
    String file = "/docs/operator/" + kind.name().toLowerCase() + ".html";

    StringWriter writer = new StringWriter();

    try (InputStreamReader is = new InputStreamReader(Expression.class.getResourceAsStream(file))) {
      IOUtils.copy(is, writer);
    } catch (Exception e) {
      writer.append(e.getMessage());
      System.err.println("Warning no documentation : " + kind);
    }

    return writer.toString();
  }

  private String findDescription(Kind kind) {
    String doc = getHtmlDocumentation(kind);

    if (doc == null) return "";

    int beginIndex = doc.indexOf("id=\"preamble\"");
    beginIndex = doc.indexOf("<p>", beginIndex);

    if (beginIndex > 0) {
      int endIndex = doc.indexOf("</p>", beginIndex);

      return doc.substring(beginIndex + 3, endIndex);
    }

    return "";
  }

  protected final Kind kind;

  /** The name of the operator/function. Ex. "OVERLAY" or "TRIM" */
  private final String name;

  private final boolean isAlias;

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

  private final String description;

  /**
   * Creates an operator specifying left and right precedence.
   *
   * @param kind Kind of operator
   * @param name Name of operator
   * @param leftPrecedence Left precedence
   * @param rightPrecedence Right precedence
   */
  protected Operator(
      Kind kind, String name, int leftPrecedence, int rightPrecedence, boolean isAlias) {
    super();
    this.kind = kind;
    this.name = name;
    this.leftPrecedence = leftPrecedence;
    this.rightPrecedence = rightPrecedence;
    this.isAlias = isAlias;
    this.description = findDescription(kind);
  }

  protected Operator(
      Kind kind, String name, int precedence, boolean leftAssociativity, boolean isAlias) {
    this(
        kind,
        name,
        leftPrec(precedence, leftAssociativity),
        rightPrec(precedence, leftAssociativity),
        isAlias);
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

  /** The name of the operator/function */
  public String getName() {
    return name;
  }

  public int getLeftPrecedence() {
    return leftPrecedence;
  }

  public int getRightPrecedence() {
    return rightPrecedence;
  }

  protected URL getUrl(Kind kind) {
    return getClass().getResource("/docs/" + kind.name().toLowerCase() + ".html");
  }

  public boolean isAlias() {
    return isAlias;
  }

  /**
   * Check if the number of arguments is correct.
   *
   * @param len the number of arguments set
   * @return true if the number of arguments is correct
   */
  public boolean checkNumberOfArguments(int len) throws ExpressionException {
    return true;
  }

  public Value eval(IExpressionContext context, Expression... args) throws ExpressionException {
    switch (kind) {
      case BITAND:
        {
          Value left = args[0].eval(context);
          if (left.isNull()) return left;
          Value right = args[1].eval(context);
          if (right.isNull()) return right;

          return Value.of(left.toInteger() & right.toInteger());
        }

      case BITNOT:
        {
          Value value = args[0].eval(context);
          if (value.isNull()) return value;

          return Value.of(~value.toInteger());
        }

      case BITOR:
        {
          Value left = args[0].eval(context);
          if (left.isNull()) return left;
          Value right = args[1].eval(context);
          if (right.isNull()) return right;

          return Value.of(left.toInteger() | right.toInteger());
        }

      case BITXOR:
        {
          Value left = args[0].eval(context);
          if (left.isNull()) return left;
          Value right = args[1].eval(context);
          if (right.isNull()) return right;

          return Value.of(left.toInteger() ^ right.toInteger());
        }

      case BETWEEN:
        {
          Value operand = args[0].eval(context);
          Value start = args[1].eval(context);
          Value end = args[2].eval(context);

          if (operand.isNull() || start.isNull() || end.isNull()) {
            return Value.NULL;
          }

          return Value.of(operand.compareTo(start) >= 0 && operand.compareTo(end) <= 0);
        }

      case CASE_WHEN:
        {
          int index = 0;
          Expression switchExpression = args[0];
          ExpressionList whenList = (ExpressionList) args[1];
          ExpressionList thenList = (ExpressionList) args[2];
          Expression elseExpression = args[3];

          if (switchExpression == null) {
            for (Expression whenOperand : whenList) {
              Value condition = whenOperand.eval(context);
              if (condition.toBoolean() == true) {
                return thenList.get(index).eval(context);
              }
              index++;
            }
          } else {
            Value condition = switchExpression.eval(context);
            for (Expression whenOperand : whenList) {
              Value value = whenOperand.eval(context);
              if (condition.compareTo(value) == 0) {
                return thenList.get(index).eval(context);
              }
              index++;
            }
          }

          return elseExpression.eval(context);
        }

      case CONCAT:
        {
          StringBuilder builder = new StringBuilder();
          for (Expression operand : args) {
            Value value = operand.eval(context);
            if (!value.isNull()) builder.append(value);
          }

          if (builder.length() == 0) return Value.NULL;

          return Value.of(builder.toString());
        }

      case LIKE:
        {
          Value input = args[0].eval(context);
          Value pattern = args[1].eval(context);

          if (input.isNull() || pattern.isNull()) {
            return Value.FALSE;
          }

          String escape = null;
          if (args.length == 3) {
            Value escapeValue = args[2].eval(context);
            escape = escapeValue.toString();
          }

          final String regex = toRegexLike(pattern.toString(), escape);

          Pattern p = Pattern.compile(regex, Pattern.DOTALL);

          return Value.of(p.matcher(input.toString()).matches());
        }

      case ILIKE:
        {
          Value input = args[0].eval(context);
          Value pattern = args[1].eval(context);

          if (input.isNull() || pattern.isNull()) {
            return Value.FALSE;
          }

          String escape = null;
          if (args.length == 3) {
            Value escapeValue = args[2].eval(context);
            escape = escapeValue.toString();
          }

          final String regex = toRegexLike(pattern.toString(), escape);

          Pattern p = Pattern.compile(regex, Pattern.DOTALL | Pattern.CASE_INSENSITIVE);

          return Value.of(p.matcher(input.toString()).matches());
        }

      case NEGATIVE:
        {
          Value value = args[0].eval(context);
          return value.negate();
        }

      case LOGICAL_NOT:
        {
          Value operand = args[0].eval(context);

          if (operand.isNull()) {
            return Value.NULL;
          }

          return Value.of(!operand.toBoolean());
        }

      case LOGICAL_AND:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);
          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }
          return Value.of(left.toBoolean() && right.toBoolean());
        }

      case LOGICAL_OR:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);
          if (left.isNull() && right.isNull()) {
            return Value.NULL;
          }

          return Value.of(left.toBoolean() || right.toBoolean());
        }

      case LOGICAL_XOR:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);
          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }
          return Value.of(
              (left.toBoolean() || right.toBoolean()) && !(left.toBoolean() && right.toBoolean()));
        }

      case ADD:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }
          return left.add(right);
        }

      case SUBTRACT:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }
          return left.subtract(right);
        }

      case MULTIPLY:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }
          return left.multiply(right);
        }

      case DIVIDE:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }
          // prevent a division by zero ..
          if (right.signum() == 0) throw createDivisionByZeroError();

          return left.divide(right);
        }

      case MOD:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }
          // prevent a division by zero ..
          if (right.signum() == 0) throw createDivisionByZeroError();

          return left.remainder(right);
        }

      case POWER: // Same implementation for operator and function
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }
          if (right.signum() < 0) throw new ArithmeticException("Cannot power negative " + right);
          if (right.signum() == 0) return Value.ONE;

          return left.power(right);
        }

      case EQUAL:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          // Treats NULLs as unknown values
          // NULL is not equal ( = ) to anything—not even to another NULL.
          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }

          return Value.of(left.compareTo(right) == 0);
        }

      case LESS_THAN_OR_GREATER_THEN:
      case NOT_EQUALS:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          if (left.isNull() && right.isNull()) {
            return Value.FALSE;
          }
          if (left.isNull() || right.isNull()) {
            return Value.TRUE;
          }

          return Value.of(left.compareTo(right) != 0);
        }

      case LESS_THAN:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }

          return Value.of(left.compareTo(right) < 0);
        }

      case LESS_THAN_OR_EQUAL:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }

          return Value.of(left.compareTo(right) <= 0);
        }

      case GREATER_THAN:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }

          return Value.of(left.compareTo(right) > 0);
        }

      case GREATER_THAN_OR_EQUAL:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          if (left.isNull() || right.isNull()) {
            return Value.NULL;
          }

          return Value.of(left.compareTo(right) >= 0);
        }

      case IS:
        {
          Value left = args[0].eval(context);
          Value right = args[1].eval(context);

          return Value.of(left.equals(right));
        }

      case IN:
        {
          Value left = args[0].eval(context);
          if (left.isNull()) {
            return Value.FALSE;
          }

          ExpressionList list = (ExpressionList) args[1];
          for (Expression expression : list) {
            Value value = expression.eval(context);
            if (left.compareTo(value) == 0) {
              return Value.TRUE;
            }
          }

          return Value.FALSE;
        }

      default:
        throw createInternalError(kind.name());
    }
  }

  /* If leftOperand is an OrNode, then we modify the tree from:
   *
   *                              Or1
   *                           /      \
   *                      Or2              Nodex
   *                   /      \                ...
   *              left2        right2
   *
   *      to:
   *
   *                                    Or1
   *                                 /      \
   *   changeToCNF(left2)          Or2
   *                           /        \
   *              changeToCNF(right2)  changeToCNF(Nodex)
   *
   *  NOTE: We could easily switch places between changeToCNF(left2) and
   *  changeToCNF(right2).
   */

  public Expression optimize(IExpressionContext context, Expression... operands)
      throws ExpressionException {
    switch (kind) {
      case NEGATIVE:
      case LOGICAL_NOT:
        {
          Expression operand = operands[0].optimize(context);

          if (operand.isConstant()) {
            return eval(context, operand);
          } else if (operand.is(this.kind)) {
            // Eliminate double NOT or MINUS
            ExpressionCall call = (ExpressionCall) operand;
            return call.getOperands()[0];
          }

          return new ExpressionCall(this, operand);
        }

      case LOGICAL_OR:
        {
          Expression left = operands[0].optimize(context);
          Expression right = operands[1].optimize(context);

          if (left.isConstant()) {
            Value value = (Value) left;

            if (value.toBoolean()) return Value.TRUE;
            if (!value.toBoolean()) return right;
          }

          if (right.isConstant()) {
            Value value = (Value) right;
            if (value.toBoolean()) return Value.TRUE;
            if (!value.toBoolean()) return left;
          }
          return new ExpressionCall(this, left, right);
        }

      case LOGICAL_AND:
        {
          Expression left = operands[0].optimize(context);
          Expression right = operands[1].optimize(context);

          if (left.isConstant()) {
            Value value = (Value) left;

            if (value.isNull() || (right.isConstant() && ((Value) right).isNull()))
              return Value.NULL;

            if (!value.toBoolean()) return Value.FALSE;
          }

          if (right.isConstant()) {
            Value value = (Value) right;
            if (value.isNull()) return Value.NULL;
            if (!value.toBoolean()) return Value.FALSE;
          }

          if (left.isConstant() && right.isConstant()) {
            return eval(context, left, right);
          }

          return new ExpressionCall(this, left, right);
        }

        // Binary operator
      case CONCAT:
      case CONTAINS:
      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case MOD:
      case LOGICAL_XOR:
      case EQUAL:
      case NOT_EQUALS:
      case LESS_THAN:
      case LESS_THAN_OR_EQUAL:
      case GREATER_THAN:
      case GREATER_THAN_OR_EQUAL:
      case IS:
      case IN:
        {
          Expression left = operands[0].optimize(context);
          Expression right = operands[1].optimize(context);

          if (left.isConstant() && right.isConstant()) {
            return eval(context, left, right);
          }

          return new ExpressionCall(this, left, right);
        }

      case LIKE:
        {
          Expression left = operands[0].optimize(context);
          Expression right = operands[1].optimize(context);

          if (left.isConstant() && right.isConstant()) {
            if (operands.length == 3) {
              Expression escape = operands[2].optimize(context);
              return eval(context, left, right, escape);
            }
            return eval(context, left, right);
          }

          // TODO: optimize NULL LIKE X : convert to NULL
          // TODO: optimize X LIKE NULL : convert to NULL
          // TODO: optimize X LIKE '%' : convert to X IS NOT NULL
          // TODO: optimize X LIKE 'Hello' : convert to X = 'Hello'
          // TODO: optimize the common case of X LIKE 'foo%' to Starts_With(X,'foo')
          // TODO: optimize the common case of X LIKE '%foo' to Ends_With(X,'foo')

          if (operands.length == 3) {
            Expression escape = operands[2].optimize(context);
            return new ExpressionCall(this, left, right, escape);
          }

          return new ExpressionCall(this, left, right);
        }

      case BETWEEN:
        {
          Expression operand = operands[0].optimize(context);
          Expression start = operands[1].optimize(context);
          Expression end = operands[2].optimize(context);

          if (operand.isConstant() && start.isConstant() && end.isConstant()) {
            return eval(context, operand, start, end);
          }

          return new ExpressionCall(this, operand, start, end);
        }

      default:
        System.out.println("Not optimised " + kind);
        return new ExpressionCall(this, operands);
    }
  }

  /**
   * Writes a expression representation of a call to this operator to a writer, including
   * parentheses if the operators on either side are of greater precedence.
   */
  public void unparse(StringWriter writer, ExpressionCall call, int leftPrec, int rightPrec) {
    switch (kind) {
      case BETWEEN:
        {
          Expression[] operands = call.getOperands();
          operands[0].unparse(writer, leftPrec, rightPrec);
          writer.append(' ');
          writer.append("BETWEEN");
          writer.append(' ');
          operands[1].unparse(writer, leftPrec, rightPrec);
          writer.append(" AND ");
          operands[2].unparse(writer, leftPrec, rightPrec);
          break;
        }

      case CASE_WHEN:
        {
          Expression[] operands = call.getOperands();

          Expression switchExpression = operands[0];
          ExpressionList whenList = (ExpressionList) operands[1];
          ExpressionList thenList = (ExpressionList) operands[2];
          Expression elseExpression = operands[3];

          writer.append("CASE ");

          // Form switch expression
          if (switchExpression != null) {
            switchExpression.unparse(writer, 0, 0);
          }

          int index = 0;
          for (Expression whenOperand : whenList) {
            writer.append("WHEN ");
            whenOperand.unparse(writer, 0, 0);
            writer.append(" THEN ");
            Expression thenOperand = thenList.get(index++);
            thenOperand.unparse(writer, 0, 0);
          }
          if (elseExpression != null) {
            elseExpression.unparse(writer, leftPrec, rightPrec);
          }
          writer.append("END");
          break;
        }

      case CONCAT:
        {
          Expression[] operands = call.getOperands();
          operands[0].unparse(writer, leftPrec, rightPrec);
          writer.append(this.getName());
          operands[1].unparse(writer, leftPrec, rightPrec);
          break;
        }

      case LIKE:
        {
          Expression[] operands = call.getOperands();
          operands[0].unparse(writer, leftPrec, rightPrec);
          writer.append(' ');
          writer.append(this.getName());
          writer.append(' ');
          operands[1].unparse(writer, leftPrec, rightPrec);
          if (call.getOperandCount() == 3) {
            writer.append(" ESCAPE ");
            operands[2].unparse(writer, leftPrec, rightPrec);
          }
          break;
        }

      case NEGATIVE:
        {
          Expression[] operands = call.getOperands();
          writer.append(this.getName());
          operands[0].unparse(writer, leftPrec, rightPrec);
          break;
        }

      case LOGICAL_NOT:
        {
          Expression[] operands = call.getOperands();
          writer.append(this.getName());
          writer.append(' ');
          operands[0].unparse(writer, leftPrec, rightPrec);
          break;
        }

      case LOGICAL_AND:
      case LOGICAL_OR:
      case LOGICAL_XOR:
      case EQUAL:
      case NOT_EQUALS:
      case LESS_THAN_OR_GREATER_THEN:
      case LESS_THAN:
      case LESS_THAN_OR_EQUAL:
      case GREATER_THAN:
      case GREATER_THAN_OR_EQUAL:
      case IS:
      case IN:
        {
          Expression[] operands = call.getOperands();
          operands[0].unparse(writer, leftPrec, rightPrec);
          writer.append(' ');
          writer.append(this.getName());
          writer.append(' ');
          operands[1].unparse(writer, leftPrec, rightPrec);
          break;
        }

      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case MOD:
        {
          // case POWER_OPERATOR:
          Expression[] operands = call.getOperands();
          operands[0].unparse(writer, leftPrec, rightPrec);
          writer.append(this.getName());
          operands[1].unparse(writer, leftPrec, rightPrec);
          break;
        }
      default:
        throw createInternalError(kind.name());
    }
  }

  protected final ExpressionException createArgumentOutOfRangeError(Object arg) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Expression.ArgumentOutOfRange", arg));
  }

  protected final ExpressionException createDivisionByZeroError() {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.DivisionByZero"));
  }

  protected final ExpressionException createInternalError(final String error) {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.InternalError", error));
  }

  /** Translates a LIKE pattern to Java regex pattern, with optional escape string. */
  private String toRegexLike(String sqlPattern, CharSequence escapeStr) {
    final char escapeChar;
    if (escapeStr != null) {

      if (escapeStr.length() != 1) {
        throw createInvalidEscapeCharacter(escapeStr.toString());
      }

      escapeChar = escapeStr.charAt(0);
    } else {
      escapeChar = 0;
    }
    return toRegexLike(sqlPattern, escapeChar);
  }

  /** Translates a LIKE pattern to Java regex pattern. */
  private String toRegexLike(String sqlPattern, char escapeChar) {
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
          throw createInvalidEscapeSequence(sqlPattern, i);
        }
        char nextChar = sqlPattern.charAt(i + 1);
        if ((nextChar == '_') || (nextChar == '%') || (nextChar == escapeChar)) {
          if (JAVA_REGEX_SPECIALS.indexOf(nextChar) >= 0) {
            javaPattern.append('\\');
          }
          javaPattern.append(nextChar);
          i++;
        } else {
          throw createInvalidEscapeSequence(sqlPattern, i);
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

  private ExpressionException createInvalidEscapeCharacter(String s) {
    return new ExpressionException("Invalid escape character '" + s + "'");
  }

  private ExpressionException createInvalidEscapeSequence(String s, int i) {
    return new ExpressionException("Invalid escape sequence '" + s + "', " + i);
  }

  @Override
  public int compareTo(Operator o) {

    // Compare kind if same type
    if (this.getClass().equals(o.getClass())) return this.name.compareTo(o.name);

    // Operator first and function last
    return (o instanceof Operator) ? 1 : 0;
  }

  public Kind getKind() {
    return kind;
  }

  public String getDescription() {
    return description;
  }
}
