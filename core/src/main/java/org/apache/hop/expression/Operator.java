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


import org.apache.commons.math3.util.FastMath;
import org.apache.hop.core.util.TranslateUtil;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import org.apache.hop.i18n.BaseMessages;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.MathContext;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Objects;
import java.util.regex.Pattern;

/**
 * Operators have the precedence levels. An operator on higher levels is evaluated before an
 * operator on a lower level
 *
 * @author Nicolas ADMENT
 */
public class Operator implements Comparable<Operator> {

  protected static final Class<?> PKG = Operator.class; // for i18n purposes

  private static final String JAVA_REGEX_SPECIALS = "\\.[]{}()<>*+-=!?^$|";

  private static final double SECONDS_BY_DAY = 24D * 60 * 60;

  // -------------------------------------------------------------
  // BITWISE OPERATORS
  // -------------------------------------------------------------

  /** Bitwise AND operator "&". */
  public static final Operator BITAND =
      new Operator(Kind.BITAND, "&", 70, true, "i18n::Operator.Category.Bitwise");

  /** Bitwise OR operator "|". */
  public static final Operator BITOR =
      new Operator(Kind.BITOR, "|", 90, true, "i18n::Operator.Category.Bitwise");

  /** Bitwise NOT operator "~". */
  public static final Operator BITNOT =
      new Operator(Kind.BITNOT, "~", 40, true, "i18n::Operator.Category.Bitwise");

  /** Bitwise XOR operator "^". */
  public static final Operator BITXOR =
      new Operator(Kind.BITXOR, "^", 80, true, "i18n::Operator.Category.Bitwise");

  // -------------------------------------------------------------
  // LOGICAL OPERATORS
  // -------------------------------------------------------------

  /**
   * Logical negation <code>NOT</code> operator
   *
   * <p>
   * Syntax of the operator:
   *
   * <ul>
   * <li><code>field [NOT] TRUE</code>
   * <li><code>field [NOT] IN (list of values)</code>
   * <li><code>field [NOT] BETWEEN start AND end</code>
   * </ul>
   */
  public static final Operator BOOLNOT =
      new Operator(Kind.LOGICAL_NOT, "NOT", 150, false, "i18n::Operator.Category.Logical");
  /** Logical disjunction <code>OR</code> operator. */
  public static final Operator BOOLOR =
      new Operator(Kind.LOGICAL_OR, "OR", 180, true, "i18n::Operator.Category.Logical");
  /** Logical conjunction <code>AND</code> operator. */
  public static final Operator BOOLAND =
      new Operator(Kind.LOGICAL_AND, "AND", 160, true, "i18n::Operator.Category.Logical");
  /** Logical <code>XOR</code> operator. */
  public static final Operator BOOLXOR =
      new Operator(Kind.LOGICAL_XOR, "XOR", 170, true, "i18n::Operator.Category.Logical");



  // -------------------------------------------------------------
  // COMPARISON OPERATORS
  // -------------------------------------------------------------

  /**
   * An operator describing the <code>IS</code> operator.
   *
   * <p>
   * Syntax of the operator:
   *
   * <ul>
   * <li><code>field IS TRUE</code>
   * <li><code>field IS FALSE</code>
   * <li><code>field IS NULL</code>
   * </ul>
   */
  public static final Operator IS =
      new Operator(Kind.IS, "IS", 140, true, "i18n::Operator.Category.Comparison");

  /**
   * Logical <code>IN</code> operator tests for a value's membership in a list of values. The IN
   * operator is a shorthand for multiple OR conditions.
   *
   * <p>
   * Syntax of the operator:
   *
   * <ul>
   * <li><code>field [NOT] IN list of values</code>
   * </ul>
   *
   * <p>
   * <b>NOTE</b> If the <code>NOT</code> clause is present the parser will generate a equivalent to
   * <code>
   * NOT (field IN list of values ...)</code>
   */
  public static final Operator IN =
      new Operator(Kind.IN, "IN", 120, true, "i18n::Operator.Category.Comparison");
  /**
   * An operator describing the <code>LIKE</code> operator.
   *
   * <p>
   * Syntax of the operator:
   *
   * <ul>
   * <li><code>field [NOT] LIKE pattern ESCAPE char</code>
   * </ul>
   *
   * <p>
   * <b>NOTE</b> If the <code>NOT</code> clause is present the parser will generate a equivalent to
   * <code>
   * NOT (field LIKE pattern ...)</code>
   */
  public static final Operator LIKE =
      new Operator(Kind.LIKE, "LIKE", 120, true, "i18n::Operator.Category.Comparison");

  public static final Operator ILIKE =
      new Operator(Kind.ILIKE, "ILIKE", 120, true, "i18n::Operator.Category.Comparison");

  public static final Operator BETWEEN =
      new Operator(Kind.BETWEEN, "BETWEEN", 120, true, "i18n::Operator.Category.Comparison");

  /** Comparison equals operator '<code>=</code>'. */
  public static final Operator EQUAL =
      new Operator(Kind.EQUAL, "=", 130, true, "i18n::Operator.Category.Comparison");
  /** Comparison not equals operator '<code>!=</code>'. */
  public static final Operator NOT_EQUAL =
      new Operator(Kind.NOT_EQUAL, "!=", 130, true, "i18n::Operator.Category.Comparison");
  /** Comparison not equals operator '<code><></code>'. */
  public static final Operator LESS_THAN_OR_GREATER_THAN =
      new Operator(Kind.NOT_EQUAL, "<>", 130, true, "i18n::Operator.Category.Comparison");
  /** Comparison less than operator '<code>&lt;</code>'. */
  public static final Operator LESS_THAN =
      new Operator(Kind.LESS_THAN, "<", 130, true, "i18n::Operator.Category.Comparison");
  /** Comparison less than or equal operator '<code>&lt;=</code>'. */
  public static final Operator LESS_THAN_OR_EQUAL =
      new Operator(Kind.LESS_THAN_OR_EQUAL, "<=", 130, true, "i18n::Operator.Category.Comparison");
  /** Comparison greater than operator '<code>&gt;</code>'. */
  public static final Operator GREATER_THAN =
      new Operator(Kind.GREATER_THAN, ">", 130, true, "i18n::Operator.Category.Comparison");
  /** Comparison greater than or equal operator '<code>&gt;=</code>'. */
  public static final Operator GREATER_THAN_OR_EQUAL = new Operator(Kind.GREATER_THAN_OR_EQUAL,
      ">=", 130, true, "i18n::Operator.Category.Comparison");

  // -------------------------------------------------------------
  // Hidden operator available as function but used by optimizer
  // -------------------------------------------------------------

  public static final Operator STARTSWITH =
      new Operator(Kind.STARTSWITH, "STARTSWITH", 10, true, "i18n::Operator.Category.Comparison");
  public static final Operator ENDSWITH =
      new Operator(Kind.ENDSWITH, "ENDSWITH", 10, true, "i18n::Operator.Category.Comparison");
  public static final Operator CONTAINS =
      new Operator(Kind.CONTAINS, "CONTAINS", 10, true, "i18n::Operator.Category.Comparison");


  // -------------------------------------------------------------
  // ARITHMETIC OPERATORS
  // -------------------------------------------------------------

  /** Arithmetic unary negative operator '<code>-</code>'. */
  public static final Operator NEGATIVE =
      new Operator(Kind.NEGATIVE, "-", 30, true, "i18n::Operator.Category.Mathematical");

  /** Arithmetic power operator '<code>**</code>'. */
  public static final Operator POWER =
      new Operator(Kind.POWER, "**", 70, true, "i18n::Operator.Category.Mathematical");

  /** Arithmetic multiplication operator '<code>*</code>'. */
  public static final Operator MULTIPLY =
      new Operator(Kind.MULTIPLY, "*", 50, true, "i18n::Operator.Category.Mathematical");

  /** Arithmetic division operator '<code>/</code>'. */
  public static final Operator DIVIDE =
      new Operator(Kind.DIVIDE, "/", 50, true, "i18n::Operator.Category.Mathematical");

  /** Arithmetic modulus operator '<code>%</code>'. */
  public static final Operator MODULUS =
      new Operator(Kind.MOD, "%", 50, true, "i18n::Operator.Category.Mathematical");

  /** Arithmetic addition operator '<code>+</code>'. */
  public static final Operator ADD =
      new Operator(Kind.ADD, "+", 100, true, "i18n::Operator.Category.Mathematical");

  /** Arithmetic subtraction operator '<code>-</code>'. */
  public static final Operator SUBTRACT =
      new Operator(Kind.SUBTRACT, "-", 100, true, "i18n::Operator.Category.Mathematical");

  // -------------------------------------------------------------
  // SPECIAL OPERATORS
  // -------------------------------------------------------------

  /** Cast <code>::</code> operator. */
  public static final Operator CAST =
      new Operator(Kind.CAST, "::", 10, true, "i18n::Operator.Category.Conversion");

  /** An operator describing the <code>CASE</code> operator. */
  public static final Operator CASE =
      new Operator(Kind.CASE_WHEN, "CASE", 120, true, "i18n::Operator.Category.Conditional");

  /** String concatenation operator '<code>||</code>'. */
  public static final Operator CONCAT =
      new Operator(Kind.CONCAT, "||", 110, true, "i18n::Operator.Category.String");

  protected Kind kind;

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
   * Creates an operator specifying left and right precedence.
   *
   * @param kind Kind of operator
   * @param name Name of operator
   * @param leftPrecedence Left precedence
   * @param rightPrecedence Right precedence
   */
  protected Operator(Kind kind, String alias, int leftPrecedence, int rightPrecedence,
      String category) {
    super();
    this.kind = kind;
    this.name = kind.name();
    this.alias = alias;
    this.leftPrecedence = leftPrecedence;
    this.rightPrecedence = rightPrecedence;
    this.isDeterministic = true;
    this.category = TranslateUtil.translate(category, Operator.class);
    this.description = OperatorUtils.findDescription(kind.name());
  }

  /**
   * Creates an function operator specifying left and right precedence.
   *
   * @param name Name of function
   * @param alias Alias of function
   * @param leftPrecedence Left precedence
   * @param rightPrecedence Right precedence
   */
  protected Operator(String name, String alias, int leftPrecedence, int rightPrecedence,
      boolean isDeterministic, String category) {
    super();

    // Some operator has syntax of function CAST, TRY_CAST, CONCAT, CONTAINS, EXTRACT, STARTSWITH,
    // ENDSWITH
    try {
      this.kind = Kind.valueOf(name);
    } catch (Exception e) {
      this.kind = Kind.FUNCTION;
    }

    this.name = name;
    this.alias = alias;
    this.leftPrecedence = leftPrecedence;
    this.rightPrecedence = rightPrecedence;
    this.isDeterministic = isDeterministic;
    this.category = TranslateUtil.translate(category, Operator.class);
    this.description = OperatorUtils.findDescription(name);
  }

  /**
   * Creates an operator specifying precedence and associativity.
   *
   * @param kind Kind of operator
   * @param name Name of operator
   * @param precedence precedence
   * @param leftAssociativity left associativity
   */
  protected Operator(Kind kind, String name, int precedence, boolean leftAssociativity,
      String category) {
    this(kind, name, leftPrec(precedence, leftAssociativity),
        rightPrec(precedence, leftAssociativity), category);
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
    return getClass().getResource("/docs/" + kind.name().toLowerCase() + ".html");
  }

  @Override public boolean equals(Object obj) {
    if (!(obj instanceof Operator)) {
      return false;
    }
    if (!obj.getClass().equals(this.getClass())) {
      return false;
    }
    Operator other = (Operator) obj;
    return name.equals(other.name) && kind == other.kind;
  }
  
  @Override public int hashCode() {
    return Objects.hash(kind, name);
  }
  
  /**
   * Check if the number of arguments is correct.
   *
   * @param len the number of arguments set
   * @throws error if not enough or too many arguments
   */
  public void checkNumberOfArguments(int len) throws ExpressionException {
    // Checked by parser
  }

  public Object eval(IExpressionContext context, IExpression... args) throws ExpressionException {
    switch (kind) {
      case CAST:
        return cast(context, args);
      case BITAND:
        return bitwiseAnd(context, args);
      case BITNOT:
        return bitwiseNot(context, args);
      case BITOR:
        return bitwiseOr(context, args);
      case BITXOR:
        return bitwiseXor(context, args);
      case BETWEEN:
        return between(context, args);
      case CASE_WHEN:
        return caseWhen(context, args);
      case CONCAT:
        return concat(context, args);
      case LIKE:
        return like(context, args);
      case ILIKE:
        return ilike(context, args);
      case NEGATIVE:
        return negative(context, args);
      case LOGICAL_NOT:
        return logicalNot(context, args);
      case LOGICAL_AND:
        return logicalAnd(context, args);
      case LOGICAL_OR:
        return logicalOr(context, args);
      case LOGICAL_XOR:
        return logicalXor(context, args);
      case ADD:
        return add(context, args);
      case SUBTRACT:
        return subtract(context, args);
      case MULTIPLY:
        return multiply(context, args);
      case DIVIDE:
        return divide(context, args);
      case MOD:
        return mod(context, args);
      case POWER:
        return power(context, args);
      case EQUAL:
        return equalTo(context, args);
      case LESS_THAN_OR_GREATER_THAN:
      case NOT_EQUAL:
        return notEqualTo(context, args);
      case LESS_THAN:
        return lessThan(context, args);
      case LESS_THAN_OR_EQUAL:
        return lessThanOrEqual(context, args);
      case GREATER_THAN:
        return greaterThan(context, args);
      case GREATER_THAN_OR_EQUAL:
        return greaterThanOrEqual(context, args);
      case IS:
        return is(context, args);
      case IN:
        return in(context, args);
      case STARTSWITH:
        return startsWith(context, args);
      case ENDSWITH:
        return endsWith(context, args);
      case CONTAINS:
        return contains(context, args);
      default:
        throw errorInternal(kind.name());
    }
  }

  /**
   * Writes a expression representation of a call to this operator to a writer, including
   * parentheses if the operators on either side are of greater precedence.
   */
  public void write(StringWriter writer, ExpressionCall call, int leftPrec, int rightPrec) {
    switch (kind) {

      case BETWEEN:
        call.getOperand(0).write(writer, leftPrec, rightPrec);
        writer.append(' ');
        writer.append("BETWEEN");
        writer.append(' ');
        call.getOperand(1).write(writer, leftPrec, rightPrec);
        writer.append(" AND ");
        call.getOperand(2).write(writer, leftPrec, rightPrec);
        break;

      case CASE_WHEN: {
        writer.append("CASE");

        // Form switch expression
        IExpression switchExpression = call.getOperand(0);
        if (switchExpression != null) {
          writer.append(' ');
          switchExpression.write(writer, 0, 0);
        }

        int index = 0;
        ExpressionList whenList = (ExpressionList) call.getOperand(1);
        ExpressionList thenList = (ExpressionList) call.getOperand(2);
        for (IExpression whenOperand : whenList) {
          writer.append(" WHEN ");
          whenOperand.write(writer, 0, 0);
          IExpression thenOperand = thenList.get(index++);
          writer.append(" THEN ");
          thenOperand.write(writer, 0, 0);
        }

        IExpression elseExpression = call.getOperand(3);
        if (elseExpression != null) {
          writer.append(" ELSE ");
          elseExpression.write(writer, leftPrec, rightPrec);
        }
        writer.append(" END");
        break;
      }

      case CONCAT:
        boolean concatFirst = true;
        for (IExpression operand : call.getOperands()) {
          if (concatFirst)
            concatFirst = false;
          else
            writer.append("||");
          operand.write(writer, leftPrec, rightPrec);
        }
        break;

      case LIKE:
        call.getOperand(0).write(writer, leftPrec, rightPrec);
        writer.append(' ');
        writer.append(this.getName());
        writer.append(' ');
        call.getOperand(1).write(writer, leftPrec, rightPrec);
        if (call.getOperandCount() == 3) {
          writer.append(" ESCAPE ");
          call.getOperand(2).write(writer, leftPrec, rightPrec);
        }
        break;

      case NEGATIVE:
        writer.append('-');
        call.getOperand(0).write(writer, leftPrec, rightPrec);
        break;

      case LOGICAL_NOT:
        writer.append(this.getAlias());
        writer.append(' ');
        call.getOperand(0).write(writer, leftPrec, rightPrec);
        break;

      case LOGICAL_AND:
      case LOGICAL_OR:
      case LOGICAL_XOR:
      case IS:
      case IN:
        call.getOperand(0).write(writer, leftPrec, rightPrec);
        writer.append(' ');
        writer.append(this.getAlias());
        writer.append(' ');
        call.getOperand(1).write(writer, leftPrec, rightPrec);
        break;

      case EQUAL:
      case NOT_EQUAL:
      case LESS_THAN_OR_GREATER_THAN:
      case LESS_THAN:
      case LESS_THAN_OR_EQUAL:
      case GREATER_THAN:
      case GREATER_THAN_OR_EQUAL:
      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case MOD:
        // case POWER_OPERATOR:
        call.getOperand(0).write(writer, leftPrec, rightPrec);
        writer.append(this.getAlias());
        call.getOperand(1).write(writer, leftPrec, rightPrec);
        break;

      case EXTRACT: {
        writer.append("EXTRACT(");
        call.getOperand(0).write(writer, leftPrec, rightPrec);
        writer.append(" FROM ");
        call.getOperand(1).write(writer, leftPrec, rightPrec);
        writer.append(')');
        break;
      }

      case CAST:
      case TRY_CAST:
        writer.append(this.getName());
        writer.append('(');
        call.getOperand(0).write(writer, leftPrec, rightPrec);
        writer.append(" AS ");
        writer.append(call.getOperand(1).toString());
        if (call.getOperandCount() == 3) {
          writer.append(" FORMAT ");
          call.getOperand(2).write(writer, leftPrec, rightPrec);
        }
        writer.append(')');
        break;

      case CONTAINS:
      case STARTSWITH:
      case ENDSWITH:
      case FUNCTION:
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
        break;


      default:
        throw errorInternal(kind.name());
    }
  }

  protected static final ExpressionException errorArgumentOutOfRange(Object arg) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Expression.ArgumentOutOfRange", arg));
  }


  protected static final ExpressionException errorInternal(final String error) {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.InternalError", error));
  }

  /** Translates a LIKE pattern to Java regex pattern, with optional escape string. */
  private static String toRegexLike(String sqlPattern, CharSequence escapeStr) {
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
  private static String toRegexLike(String sqlPattern, char escapeChar) {
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

  protected static ExpressionException errorInvalidEscapeCharacter(String s) {
    return new ExpressionException("Invalid escape character '" + s + "'");
  }

  protected static ExpressionException errorInvalidEscapeSequence(String s, int i) {
    return new ExpressionException("Invalid escape sequence '" + s + "', " + i);
  }

  @Override
  public int compareTo(Operator o) {

    // Compare kind if same type
    if (this.getClass().equals(o.getClass()))
      return this.name.compareTo(o.name);

    // Operator first and function last
    return (o instanceof Operator) ? 1 : 0;
  }

  public Kind getKind() {
    return kind;
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

  public Object between(final IExpressionContext context, final IExpression... args) {
    Object value = args[0].eval(context);
    Object start = args[1].eval(context);
    Object end = args[2].eval(context);

    if (value == null || start == null || end == null) {
      return null;
    }

    return compareTo(value, start) >= 0 && compareTo(value, end) <= 0;
  }

  @ScalarFunction(name = "CAST", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.Conversion")
  public Object cast(final IExpressionContext context, final IExpression... args) {
    Object value = args[0].eval(context);
    if (value == null)
      return null;

    DataType type = (DataType) args[1].eval(context);

    if (args.length == 3) {
      Object format = args[2].eval(context);
      return convertTo(value, type, coerceToString(format));
    }

    return convertTo(value, type);
  }

  /**
   * Converts a Object of one data type into another data type if the cast succeeds; otherwise,
   * returns null.
   */
  @ScalarFunction(name = "TRY_CAST", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.Conversion")
  public Object tryCast(final IExpressionContext context, final IExpression... args) {
    Object value = args[0].eval(context);
    if (value == null) {
      return null;
    }
    try {
      DataType type = (DataType) args[1].eval(context);

      if (args.length == 3) {
        Object format = args[2].eval(context);
        return convertTo(value, type, coerceToString(format));
      }

      return convertTo(value, type);
    } catch (Exception e) {
      return null;
    }
  }

  @ScalarFunction(name = "CONCAT", minArgs = 2, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.String")
  public Object concat(final IExpressionContext context, final IExpression... args) {
    StringBuilder builder = new StringBuilder();
    for (IExpression operand : args) {
      Object value = operand.eval(context);
      if (value != null)
        builder.append(coerceToString(value));
    }

    if (builder.length() == 0)
      return null;

    return builder.toString();
  }

  public Object is(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    Object right = args[1].eval(context);

    if (left == null) {
      if (right == null) {
        return Boolean.TRUE;
      }
      return Boolean.FALSE;
    }

    return left.equals(right);
  }
  
  public Object isNull(final IExpressionContext context, final IExpression... args) {
    Object operand = args[0].eval(context);
    if (operand == null) {
        return Boolean.TRUE;
    }
    return Boolean.FALSE;
  }

  public Object isNotNull(final IExpressionContext context, final IExpression... args) {
    Object operand = args[0].eval(context);
    if (operand != null) {
        return Boolean.TRUE;
    }
    return Boolean.FALSE;
  }

  
  public Object in(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null) {
      return null;
    }

    ExpressionList list = (ExpressionList) args[1];
    for (IExpression expression : list) {
      Object value = expression.eval(context);
      if (compareTo(left, value) == 0) {
        return Boolean.TRUE;
      }
    }

    return Boolean.FALSE;
  }

  public Object ilike(final IExpressionContext context, final IExpression... args) {
    Object v0 = args[0].eval(context);
    if (v0 == null) {
      return null;
    }

    Object v1 = args[1].eval(context);
    if (v1 == null) {
      return null;
    }

    String escape = null;
    if (args.length == 3) {
      Object escapeValue = args[2].eval(context);
      if (escapeValue == null) {
        return null;
      }
      escape = coerceToString(escapeValue);
    }

    final String regex = toRegexLike(coerceToString(v1), escape);

    Pattern p = Pattern.compile(regex, Pattern.DOTALL | Pattern.CASE_INSENSITIVE);

    return p.matcher(coerceToString(v0)).matches();
  }

  public Object like(final IExpressionContext context, final IExpression... args) {
    Object input = args[0].eval(context);
    if (input == null) {
      return null;
    }
    Object pattern = args[1].eval(context);
    if (pattern == null) {
      return null;
    }

    String escape = null;
    if (args.length == 3) {
      Object escapeValue = args[2].eval(context);
      if (escapeValue == null) {
        return null;
      }
      escape = coerceToString(escapeValue);
    }

    final String regex = toRegexLike(coerceToString(pattern), escape);

    Pattern p = Pattern.compile(regex, Pattern.DOTALL);

    return p.matcher(coerceToString(input)).matches();
  }

  public Object caseWhen(final IExpressionContext context, final IExpression... args) {
    int index = 0;
    IExpression switchExpression = args[0];
    ExpressionList whenList = (ExpressionList) args[1];
    ExpressionList thenList = (ExpressionList) args[2];
    IExpression elseExpression = args[3];

    if (switchExpression == null) {
      for (IExpression whenOperand : whenList) {
        Object condition = whenOperand.eval(context);
        if (coerceToBoolean(condition)) {
          return thenList.get(index).eval(context);
        }
        index++;
      }
    } else {
      Object condition = switchExpression.eval(context);
      for (IExpression whenOperand : whenList) {
        Object value = whenOperand.eval(context);
        if (compareTo(condition, value) == 0) {
          return thenList.get(index).eval(context);
        }
        index++;
      }
    }

    return elseExpression.eval(context);
  }

  public Object negative(final IExpressionContext context, final IExpression... args) {
    Object v0 = args[0].eval(context);

    if (v0 == null)
      return null;

    if (v0 instanceof Double) {
      double value = (double) v0;
      if (value == Double.MIN_VALUE) {
        throw errorOverflow(String.valueOf(v0));
      }
      return Double.valueOf(-value);
    }

    if (v0 instanceof Long) {
      long value = (long) v0;
      if (value == Long.MIN_VALUE) {
        throw errorOverflow(String.valueOf(v0));
      }
      return Long.valueOf(-value);
    }

    return coerceToBigNumber(v0).negate();
  }

  @ScalarFunction(name = "ADD", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public Object add(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null)
      return null;
    Object right = args[1].eval(context);
    if (right == null)
      return null;
    if (left instanceof Instant) {
      // Computes fraction of day
      long seconds = (long) (coerceToNumber(right) * SECONDS_BY_DAY);
      return coerceToDate(left).plusSeconds(seconds);
    }
    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return coerceToBigNumber(left).add(coerceToBigNumber(right));
    }
    if (left instanceof Double || right instanceof Double) {
      return coerceToNumber(left) + coerceToNumber(right);
    }
    if (left instanceof Long || right instanceof Long) {
      return coerceToInteger(left) + coerceToInteger(right);
    }

    return coerceToBigNumber(left).add(coerceToBigNumber(right));
  }

  @ScalarFunction(name = "SUBTRACT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public Object subtract(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null)
      return null;
    Object right = args[1].eval(context);
    if (right == null)
      return null;

    if (left instanceof Instant) {
      // If number, subtract fraction of day
      if (right instanceof Number) {
        long seconds = (long) (coerceToNumber(right) * SECONDS_BY_DAY);
        return coerceToDate(left).minusSeconds(seconds);
      }

      // If right operand is date, return difference in fraction of day
      if (right instanceof Instant) {
        return coerceToDate(right).until(coerceToDate(left), ChronoUnit.SECONDS) / SECONDS_BY_DAY;
      }
    }
    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return coerceToBigNumber(left).subtract(coerceToBigNumber(right));
    }
    if (left instanceof Double || right instanceof Double) {
      return coerceToNumber(left) - coerceToNumber(right);
    }
    if (left instanceof Long || right instanceof Long) {
      return coerceToInteger(left) - coerceToInteger(right);
    }

    return coerceToBigNumber(left).subtract(coerceToBigNumber(right));
  }

  @ScalarFunction(name = "MULTIPLY", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public Object multiply(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null)
      return null;
    Object right = args[1].eval(context);
    if (right == null)
      return null;

    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      return coerceToBigNumber(left).multiply(coerceToBigNumber(right));
    }
    if (left instanceof Double || right instanceof Double) {
      return coerceToNumber(left) * coerceToNumber(right);
    }
    if (left instanceof Long || right instanceof Long) {
      return coerceToInteger(left) * coerceToInteger(right);
    }

    return coerceToBigNumber(left).multiply(coerceToBigNumber(right));
  }

  @ScalarFunction(name = "DIVIDE", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public Object divide(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null)
      return null;
    Object right = args[1].eval(context);
    if (right == null)
      return null;

    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      BigDecimal divisor = coerceToBigNumber(right);
      // prevent a division by zero ..
      if (divisor.signum() == 0)
        throw errorDivisionByZero();
      return coerceToBigNumber(left).divide(coerceToBigNumber(right), MathContext.DECIMAL128);
    }
    if (left instanceof Double || right instanceof Double) {
      double divisor = coerceToNumber(right);
      // prevent a division by zero ..
      if (divisor == 0L)
        throw errorDivisionByZero();
      return coerceToNumber(left) / divisor;
    }
    if (left instanceof Long || right instanceof Long) {
      long divisor = coerceToInteger(right);
      // prevent a division by zero ..
      if (divisor == 0L)
        throw errorDivisionByZero();

      return coerceToInteger(left) / divisor;
    }

    return coerceToBigNumber(left).divide(coerceToBigNumber(right));
  }

  @ScalarFunction(name = "MOD", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public Object mod(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null)
      return null;
    Object right = args[1].eval(context);
    if (right == null)
      return null;

    if (left instanceof BigDecimal || right instanceof BigDecimal) {
      BigDecimal divisor = coerceToBigNumber(right);
      // prevent a division by zero ..
      if (divisor.signum() == 0)
        throw errorDivisionByZero();
      return coerceToBigNumber(left).remainder(divisor);
    }
    if (left instanceof Double || right instanceof Double) {
      double divisor = coerceToNumber(right);
      // prevent a division by zero ..
      if (divisor == 0L)
        throw errorDivisionByZero();
      return coerceToNumber(left) % divisor;
    }
    if (left instanceof Long || right instanceof Long) {
      long divisor = coerceToInteger(right);
      // prevent a division by zero ..
      if (divisor == 0L)
        throw errorDivisionByZero();
      return coerceToInteger(left) % divisor;
    }

    return coerceToBigNumber(left).remainder(coerceToBigNumber(right));
  }

  @ScalarFunction(name = "POWER", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public Object power(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    Object right = args[1].eval(context);
    if (left == null || right == null) {
      return null;
    }
    // if (left instanceof BigDecimal ) {
    // return coerceToBigNumber(left).pow(coerceToInteger(right).intValue());
    // }

    Double power = coerceToNumber(right);
    if (power == 0)
      return 1L;
    if (power < 0)
      throw new ArithmeticException("Cannot power negative " + power);

    return FastMath.pow(coerceToNumber(left), coerceToNumber(right));
  }

  // -------------------------------------------------------------
  // BITWISE
  // -------------------------------------------------------------


  @ScalarFunction(name = "BITNOT", category = "i18n::Operator.Category.Bitwise")
  public Object bitwiseNot(final IExpressionContext context, final IExpression... args) {
    Object value = args[0].eval(context);
    if (value == null)
      return value;

    return ~coerceToInteger(value);
  }

  @ScalarFunction(name = "BITAND", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Bitwise")
  public Long bitwiseAnd(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null)
      return null;
    Object right = args[1].eval(context);
    if (right == null)
      return null;
    return coerceToInteger(left) & coerceToInteger(right);
  }

  @ScalarFunction(name = "BITOR", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Bitwise")
  public Object bitwiseOr(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null)
      return null;
    Object right = args[1].eval(context);
    if (right == null)
      return null;
    return coerceToInteger(left) | coerceToInteger(right);
  }

  @ScalarFunction(name = "BITXOR", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Bitwise")
  public Object bitwiseXor(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null)
      return null;
    Object right = args[1].eval(context);
    if (right == null)
      return null;

    return coerceToInteger(left) ^ coerceToInteger(right);
  }

  // -------------------------------------------------------------
  // LOGICAL
  // -------------------------------------------------------------

  public Object logicalNot(final IExpressionContext context, final IExpression... args) {
    Object value = args[0].eval(context);
    if (value == null) {
      return null;
    }
    return coerceToBoolean(value) ? Boolean.FALSE : Boolean.TRUE;
  }

  public Object logicalAnd(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null) {
      return null;
    }
    Object right = args[1].eval(context);
    if (right == null) {
      return null;
    }
    return Boolean.logicalAnd(coerceToBoolean(left), coerceToBoolean(right));
  }

  public Object logicalOr(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    Object right = args[1].eval(context);
    if (left == null) {
      return coerceToBoolean(right);
    }
    if (right == null) {
      return coerceToBoolean(left);
    }
    return Boolean.logicalOr(coerceToBoolean(left), coerceToBoolean(right));
  }

  public Object logicalXor(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null) {
      return null;
    }
    Object right = args[1].eval(context);
    if (right == null) {
      return null;
    }
    return Boolean.logicalXor(coerceToBoolean(left), coerceToBoolean(right));
  }

  public Object equalTo(final IExpressionContext context, final IExpression... args) {
    // Treats NULLs as unknown values
    // NULL is not equal ( = ) to anything—not even to another NULL.
    Object left = args[0].eval(context);
    if (left == null) {
      return null;
    }
    Object right = args[1].eval(context);
    if (right == null) {
      return null;
    }
    return compareTo(left, right) == 0;
  }

  public Object notEqualTo(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null) {
      return null;
    }
    Object right = args[1].eval(context);
    if (right == null) {
      return null;
    }

    return compareTo(left, right) != 0;
  }

  public Object lessThan(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null) {
      return null;
    }
    Object right = args[1].eval(context);
    if (right == null) {
      return null;
    }

    return compareTo(left, right) < 0;
  }

  public Object lessThanOrEqual(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null) {
      return null;
    }
    Object right = args[1].eval(context);
    if (right == null) {
      return null;
    }

    return compareTo(left, right) <= 0;
  }

  public Object greaterThan(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null) {
      return null;
    }
    Object right = args[1].eval(context);
    if (right == null) {
      return null;
    }

    return compareTo(left, right) > 0;
  }

  public Object greaterThanOrEqual(final IExpressionContext context, final IExpression... args) {
    Object left = args[0].eval(context);
    if (left == null) {
      return null;
    }
    Object right = args[1].eval(context);
    if (right == null) {
      return null;
    }
    return compareTo(left, right) >= 0;
  }

  /**
   * The function returns TRUE if the first value starts with second value. Both values must be data
   * type string or binary.
   *
   * @see {@link #ENDSWITH}
   */
  @ScalarFunction(name = "STARTSWITH", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Comparison")
  public Object startsWith(final IExpressionContext context, final IExpression... args) {

    Object v0 = args[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = args[1].eval(context);
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
   * @see {@link #STARTSWITH}
   */
  @ScalarFunction(name = "ENDSWITH", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Comparison")
  public Object endsWith(final IExpressionContext context, final IExpression... args) {
    Object v0 = args[0].eval(context);
    if (v0 == null)
      return null;
    Object v1 = args[1].eval(context);
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
  public Object contains(final IExpressionContext context, final IExpression... args) {
    Object v0 = args[0].eval(context);
    if (v0 == null)
      return null;

    Object v1 = args[1].eval(context);
    if (v1 == null)
      return null;

    if (v0.toString().contains(v1.toString()))
      return Boolean.TRUE;

    return Boolean.FALSE;
  }

  /**
   * Compare this value against another value. If values need to be converted to match the other
   * operands data type, the value with the lower order is converted to the value with the higher
   * order.
   *
   * @param value the other value
   * @return 0 if both values are equal, -1 if this value is smaller, and 1 otherwise
   */
  protected static int compareTo(Object left, Object right) {

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

  public static int compare(byte[] left, byte[] right) {
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

    if (DataType.fromJava(value) == type)
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

  public static Object convertTo(Object value, final DataType type, String pattern) {

    DataType sourceType = DataType.fromJava(value);
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
        if (sourceType == DataType.STRING) {
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

    throw errorUnsupportedConversion(value, DataType.BINARY);
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

    throw errorUnsupportedConversion(value, DataType.BIGNUMBER);
  }

  public static Instant coerceToDate(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof Instant) {
      return (Instant) value;
    }

    throw errorUnsupportedConversion(value, DataType.DATE);
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

  protected static final ExpressionException errorUnsupportedConversion(Object value,
      DataType type) {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.UnsupportedConversion",
        value, DataType.fromJava(value), type));
  }

}
