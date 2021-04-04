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

import org.apache.hop.core.util.TranslateUtil;
import org.apache.hop.expression.value.Value;
import org.apache.hop.expression.value.ValueBoolean;
import org.apache.hop.expression.value.ValueInteger;
import org.apache.hop.expression.value.ValueString;
import org.apache.hop.i18n.BaseMessages;
import java.io.StringWriter;
import java.net.URL;
import java.util.regex.Pattern;

/**
 * Operators have the precedence levels. An operator on higher levels is evaluated before an
 * operator on a lower level
 *
 * @author Nicolas ADMENT
 */
public class Operator implements Comparable<Operator> {

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  private static final String JAVA_REGEX_SPECIALS = "\\.[]{}()<>*+-=!?^$|";

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
   * <b>NOTE</b> If the <code>NOT</code> clause is present the
   * {@link org.apache.hop.core.ExpressionParser parser} will generate a equivalent to <code>
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
   * <b>NOTE</b> If the <code>NOT</code> clause is present the
   * {@link org.hop.expresssion.ExpressionParser} parser will generate a equivalent to <code>
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
  /** Comparison less-than operator '<code>&lt;</code>'. */
  public static final Operator LESS_THAN =
      new Operator(Kind.LESS_THAN, "<", 130, true, "i18n::Operator.Category.Comparison");
  /** Comparison less-than-or-equal operator '<code>&lt;=</code>'. */
  public static final Operator LESS_THAN_OR_EQUAL =
      new Operator(Kind.LESS_THAN_OR_EQUAL, "<=", 130, true, "i18n::Operator.Category.Comparison");
  /** Comparison greater-than operator '<code>&gt;</code>'. */
  public static final Operator GREATER_THAN =
      new Operator(Kind.GREATER_THAN, ">", 130, true, "i18n::Operator.Category.Comparison");
  /** Comparison greater-than-or-equal operator '<code>&gt;=</code>'. */
  public static final Operator GREATER_THAN_OR_EQUAL = new Operator(Kind.GREATER_THAN_OR_EQUAL,
      ">=", 130, true, "i18n::Operator.Category.Comparison");

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



  protected final Kind kind;

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
      String category) {
    super();
    
    // Special syntax
    if ( "CAST".equals(name) ) this.kind = Kind.CAST;
    else if ( "TRY_CAST".equals(name) ) this.kind = Kind.TRY_CAST;
    else if ( "EXTRACT".equals(name) ) this.kind = Kind.EXTRACT;
    else this.kind = Kind.FUNCTION;
    
    this.name = name;
    this.alias = alias;
    this.leftPrecedence = leftPrecedence;
    this.rightPrecedence = rightPrecedence;
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

  protected URL getUrl(Kind kind) {
    return getClass().getResource("/docs/" + kind.name().toLowerCase() + ".html");
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

  public Value eval(IExpressionContext context, IExpression... args) throws ExpressionException {
    switch (kind) {

      case CAST:
        return cast(context, args);
      case BITAND:
        return bitand(context, args);
      case BITNOT:
        return bitnot(context, args);
      case BITOR:
        return bitor(context, args);
      case BITXOR:
        return bitxor(context, args);
      case BETWEEN:
        return between(context, args);
      case CASE_WHEN: 
        return casewhen(context, args);
      case CONCAT:
        return concat(context, args);
      case LIKE:
        return like(context, args);
      case ILIKE: 
        return ilike(context, args);
      case NEGATIVE: {
        Value value = args[0].eval(context);
        return value.negate();
      }

      case LOGICAL_NOT: {
        Value value = args[0].eval(context);

        if (value.isNull()) {
          return Value.NULL;
        }

        return ValueBoolean.of(!value.toBoolean());
      }

      case LOGICAL_AND: {
        Value left = args[0].eval(context);
        Value right = args[1].eval(context);
        if (left.isNull() || right.isNull()) {
          return Value.NULL;
        }
        return ValueBoolean.of(left.toBoolean() && right.toBoolean());
      }

      case LOGICAL_OR: {
        Value left = args[0].eval(context);
        Value right = args[1].eval(context);
        if (left.isNull() && right.isNull()) {
          return Value.NULL;
        }

        return ValueBoolean.of(left.toBoolean() || right.toBoolean());
      }

      case LOGICAL_XOR: {
        Value left = args[0].eval(context);
        Value right = args[1].eval(context);
        if (left.isNull() || right.isNull()) {
          return Value.NULL;
        }
        return ValueBoolean.of(
            (left.toBoolean() || right.toBoolean()) && !(left.toBoolean() && right.toBoolean()));
      }

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
      case EQUAL: {
        Value left = args[0].eval(context);
        Value right = args[1].eval(context);

        // Treats NULLs as unknown values
        // NULL is not equal ( = ) to anything—not even to another NULL.
        if (left.isNull() || right.isNull()) {
          return Value.NULL;
        }

        return ValueBoolean.of(left.compareTo(right) == 0);
      }

      case LESS_THAN_OR_GREATER_THEN:
      case NOT_EQUAL: {
        Value left = args[0].eval(context);
        Value right = args[1].eval(context);

        if (left.isNull() || right.isNull()) {
          return Value.NULL;
        }

        return ValueBoolean.of(left.compareTo(right) != 0);
      }

      case LESS_THAN: {
        Value left = args[0].eval(context);
        Value right = args[1].eval(context);

        if (left.isNull() || right.isNull()) {
          return Value.NULL;
        }

        return ValueBoolean.of(left.compareTo(right) < 0);
      }

      case LESS_THAN_OR_EQUAL: {
        Value left = args[0].eval(context);
        Value right = args[1].eval(context);

        if (left.isNull() || right.isNull()) {
          return Value.NULL;
        }

        return ValueBoolean.of(left.compareTo(right) <= 0);
      }

      case GREATER_THAN: {
        Value left = args[0].eval(context);
        Value right = args[1].eval(context);

        if (left.isNull() || right.isNull()) {
          return Value.NULL;
        }

        return ValueBoolean.of(left.compareTo(right) > 0);
      }

      case GREATER_THAN_OR_EQUAL: {
        Value left = args[0].eval(context);
        Value right = args[1].eval(context);

        if (left.isNull() || right.isNull()) {
          return Value.NULL;
        }

        return ValueBoolean.of(left.compareTo(right) >= 0);
      }

      case IS: {
        Value left = args[0].eval(context);
        Value right = args[1].eval(context);

        return ValueBoolean.of(left.equals(right));
      }

      case IN: {
        Value left = args[0].eval(context);
        if (left.isNull()) {
          return Value.NULL;
        }

        ExpressionList list = (ExpressionList) args[1];
        for (IExpression expression : list) {
          Value value = expression.eval(context);
          if (left.compareTo(value) == 0) {
            return ValueBoolean.TRUE;
          }
        }

        return ValueBoolean.FALSE;
      }

      default:
        throw createInternalError(kind.name());
    }
  }

  /*
   * If leftOperand is an OrNode, then we modify the tree from:
   *
   * Or1 / \ Or2 Nodex / \ ... left2 right2
   *
   * to:
   *
   * Or1 / \ changeToCNF(left2) Or2 / \ changeToCNF(right2) changeToCNF(Nodex)
   *
   * NOTE: We could easily switch places between changeToCNF(left2) and changeToCNF(right2).
   */

  public IExpression optimize(IExpressionContext context, IExpression... operands)
      throws ExpressionException {
    switch (kind) {
      case NEGATIVE:
      case LOGICAL_NOT: {
        IExpression operand = operands[0].optimize(context);

        if (operand.isConstant()) {
          return eval(context, operand);
        } else if (operand.is(this.kind)) {
          // Eliminate double NOT or MINUS
          ExpressionCall call = (ExpressionCall) operand;
          return call.getOperands()[0];
        }

        return new ExpressionCall(this, operand);
      }

      case LOGICAL_OR: {
        IExpression left = operands[0].optimize(context);
        IExpression right = operands[1].optimize(context);

        if (left.isConstant()) {
          Value value = (Value) left;

          if (value.toBoolean())
            return ValueBoolean.TRUE;
          if (!value.toBoolean())
            return right;
        }

        if (right.isConstant()) {
          Value value = (Value) right;
          if (value.toBoolean())
            return ValueBoolean.TRUE;
          if (!value.toBoolean())
            return left;
        }
        return new ExpressionCall(this, left, right);
      }

      case LOGICAL_AND: {
        IExpression left = operands[0].optimize(context);
        IExpression right = operands[1].optimize(context);

        if (left.isConstant()) {
          Value value = (Value) left;

          if (value.isNull() || (right.isConstant() && ((Value) right).isNull()))
            return Value.NULL;

          if (!value.toBoolean())
            return ValueBoolean.FALSE;
        }

        if (right.isConstant()) {
          Value value = (Value) right;
          if (value.isNull())
            return Value.NULL;
          if (!value.toBoolean())
            return ValueBoolean.FALSE;
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
      case NOT_EQUAL:
      case LESS_THAN:
      case LESS_THAN_OR_EQUAL:
      case GREATER_THAN:
      case GREATER_THAN_OR_EQUAL:
      case IS:
      case IN: {
        IExpression left = operands[0].optimize(context);
        IExpression right = operands[1].optimize(context);

        if (left.isConstant() && right.isConstant()) {
          return eval(context, left, right);
        }

        return new ExpressionCall(this, left, right);
      }

      case LIKE: {
        IExpression left = operands[0].optimize(context);
        IExpression right = operands[1].optimize(context);

        if (left.isConstant() && right.isConstant()) {
          if (operands.length == 3) {
            IExpression escape = operands[2].optimize(context);
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
          IExpression escape = operands[2].optimize(context);
          return new ExpressionCall(this, left, right, escape);
        }

        return new ExpressionCall(this, left, right);
      }

      case BETWEEN: {
        IExpression operand = operands[0].optimize(context);
        IExpression start = operands[1].optimize(context);
        IExpression end = operands[2].optimize(context);

        if (operand.isConstant() && start.isConstant() && end.isConstant()) {
          return eval(context, operand, start, end);
        }

        return new ExpressionCall(this, operand, start, end);
      }

      default:
        // System.out.println("Not optimised " + kind);
        return new ExpressionCall(this, operands);
    }
  }

  /**
   * Writes a expression representation of a call to this operator to a writer, including
   * parentheses if the operators on either side are of greater precedence.
   */
  public void write(StringWriter writer, ExpressionCall call, int leftPrec, int rightPrec) {
    switch (kind) {
      case BETWEEN: {
        IExpression[] operands = call.getOperands();
        operands[0].write(writer, leftPrec, rightPrec);
        writer.append(' ');
        writer.append("BETWEEN");
        writer.append(' ');
        operands[1].write(writer, leftPrec, rightPrec);
        writer.append(" AND ");
        operands[2].write(writer, leftPrec, rightPrec);
        break;
      }

      case CASE_WHEN: {
        IExpression[] operands = call.getOperands();

        IExpression switchExpression = operands[0];
        ExpressionList whenList = (ExpressionList) operands[1];
        ExpressionList thenList = (ExpressionList) operands[2];
        IExpression elseExpression = operands[3];

        writer.append("CASE");

        // Form switch expression
        if (switchExpression != null) {
          writer.append(' ');
          switchExpression.write(writer, 0, 0);
        }

        int index = 0;
        for (IExpression whenOperand : whenList) {
          writer.append(" WHEN ");
          whenOperand.write(writer, 0, 0);
          IExpression thenOperand = thenList.get(index++);
          writer.append(" THEN ");          
          thenOperand.write(writer, 0, 0);         
        }
        if (elseExpression != null) {
          writer.append(" ELSE ");
          elseExpression.write(writer, leftPrec, rightPrec);
        }
        writer.append(" END");
        break;
      }
      
      case CONCAT: {
        IExpression[] operands = call.getOperands();
        operands[0].write(writer, leftPrec, rightPrec);
        writer.append(this.getAlias());
        operands[1].write(writer, leftPrec, rightPrec);
        break;
      }

      case LIKE: {
        IExpression[] operands = call.getOperands();
        operands[0].write(writer, leftPrec, rightPrec);
        writer.append(' ');
        writer.append(this.getName());
        writer.append(' ');
        operands[1].write(writer, leftPrec, rightPrec);
        if (call.getOperandCount() == 3) {
          writer.append(" ESCAPE ");
          operands[2].write(writer, leftPrec, rightPrec);
        }
        break;
      }

      case NEGATIVE: {
        IExpression[] operands = call.getOperands();
        writer.append('-');
        operands[0].write(writer, leftPrec, rightPrec);
        break;
      }

      case LOGICAL_NOT: {
        IExpression[] operands = call.getOperands();
        writer.append(this.getAlias());
        writer.append(' ');
        operands[0].write(writer, leftPrec, rightPrec);
        break;
      }

      case LOGICAL_AND:
      case LOGICAL_OR:
      case LOGICAL_XOR:
      case IS:
      case IN: {
        IExpression[] operands = call.getOperands();
        operands[0].write(writer, leftPrec, rightPrec);
        writer.append(' ');
        writer.append(this.getAlias());
        writer.append(' ');
        operands[1].write(writer, leftPrec, rightPrec);
        break;
      }

      case CAST:{
        // case POWER_OPERATOR:
        IExpression[] operands = call.getOperands();
        operands[0].write(writer, leftPrec, rightPrec);
        writer.append(this.getAlias());
        writer.append(operands[1].toString());
        break;
      }
        
      case EQUAL:
      case NOT_EQUAL:
      case LESS_THAN_OR_GREATER_THEN:
      case LESS_THAN:
      case LESS_THAN_OR_EQUAL:
      case GREATER_THAN:
      case GREATER_THAN_OR_EQUAL:
      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case MOD: {
        // case POWER_OPERATOR:
        IExpression[] operands = call.getOperands();
        operands[0].write(writer, leftPrec, rightPrec);
        writer.append(this.getAlias());
        operands[1].write(writer, leftPrec, rightPrec);
        break;
      }
      default:
        throw createInternalError(kind.name());
    }
  }

  protected static final ExpressionException createArgumentOutOfRangeError(Object arg) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Expression.ArgumentOutOfRange", arg));
  }


  protected static final ExpressionException createInternalError(final String error) {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.InternalError", error));
  }

  /** Translates a LIKE pattern to Java regex pattern, with optional escape string. */
  private static String toRegexLike(String sqlPattern, CharSequence escapeStr) {
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

  private static ExpressionException createInvalidEscapeCharacter(String s) {
    return new ExpressionException("Invalid escape character '" + s + "'");
  }

  private static ExpressionException createInvalidEscapeSequence(String s, int i) {
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

  public static Value between(final IExpressionContext context, final IExpression... args) {
    Value operand = args[0].eval(context);
    Value start = args[1].eval(context);
    Value end = args[2].eval(context);

    if (operand.isNull() || start.isNull() || end.isNull()) {
      return Value.NULL;
    }

    return ValueBoolean.of(operand.compareTo(start) >= 0 && operand.compareTo(end) <= 0);
  }

  @ScalarFunction(name = "CAST", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.Conversion")
  public static Value cast(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return Value.NULL;

    Value type = args[1].eval(context);
    DataType targetType = DataType.of(type.toString());

    if (args.length == 3) {
      // Format can be ValueNull
      Value format = args[2].eval(context);
      return value.convertTo(context, targetType, format.toString());
    }

    return value.convertTo(targetType);
  }

  /**
   * Converts a value of one data type into another data type if the cast succeeds; otherwise,
   * returns null.
   */
  @ScalarFunction(name = "TRY_CAST", minArgs = 2, maxArgs = 3,
      category = "i18n::Operator.Category.Conversion")
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
  
  @ScalarFunction(name = "CONCAT", minArgs = 2, maxArgs = Integer.MAX_VALUE,
      category = "i18n::Operator.Category.String")
  public static Value concat(final IExpressionContext context, final IExpression... args) {
    StringBuilder builder = new StringBuilder();
    for (IExpression operand : args) {
      Value value = operand.eval(context);
      if (!value.isNull())
        builder.append(value);
    }

    if (builder.length() == 0)
      return Value.NULL;

    return ValueString.of(builder.toString());
  }

  public static Value ilike(final IExpressionContext context, final IExpression... args) {
    Value input = args[0].eval(context);
    if (input.isNull()) {
      return Value.NULL;
    }
    Value pattern = args[1].eval(context);
    if (pattern.isNull()) {
      return Value.NULL;
    }

    String escape = null;
    if (args.length == 3) {
      Value escapeValue = args[2].eval(context);
      if (escapeValue.isNull()) {
        return Value.NULL;
      }
      escape = escapeValue.toString();
    }

    final String regex = toRegexLike(pattern.toString(), escape);

    Pattern p = Pattern.compile(regex, Pattern.DOTALL | Pattern.CASE_INSENSITIVE);

    return ValueBoolean.of(p.matcher(input.toString()).matches());
  }
  
  public static Value like(final IExpressionContext context, final IExpression... args) {
    Value input = args[0].eval(context);
    if (input.isNull()) {
      return Value.NULL;
    }
    Value pattern = args[1].eval(context);
    if (pattern.isNull()) {
      return Value.NULL;
    }

    String escape = null;
    if (args.length == 3) {
      Value escapeValue = args[2].eval(context);
      if (escapeValue.isNull()) {
        return Value.NULL;
      }
      escape = escapeValue.toString();
    }

    final String regex = toRegexLike(pattern.toString(), escape);

    Pattern p = Pattern.compile(regex, Pattern.DOTALL);

    return ValueBoolean.of(p.matcher(input.toString()).matches());
  }
  
  public static Value casewhen(final IExpressionContext context, final IExpression... args) {
    int index = 0;
    IExpression switchExpression = args[0];
    ExpressionList whenList = (ExpressionList) args[1];
    ExpressionList thenList = (ExpressionList) args[2];
    IExpression elseExpression = args[3];

    if (switchExpression == null) {
      for (IExpression whenOperand : whenList) {
        Value condition = whenOperand.eval(context);
        if (condition.toBoolean()) {
          return thenList.get(index).eval(context);
        }
        index++;
      }
    } else {
      Value condition = switchExpression.eval(context);
      for (IExpression whenOperand : whenList) {
        Value value = whenOperand.eval(context);
        if (condition.compareTo(value) == 0) {
          return thenList.get(index).eval(context);
        }
        index++;
      }
    }

    // implicit ELSE NULL case
    if ( elseExpression==null ) return Value.NULL;
    
    return elseExpression.eval(context);
  }
  
  


  @ScalarFunction(name = "ADD", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public static Value add(final IExpressionContext context, final IExpression... args) {
    Value left = args[0].eval(context);
    Value right = args[1].eval(context);

    return left.add(right);
  }

  @ScalarFunction(name = "SUBTRACT", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public static Value subtract(final IExpressionContext context, final IExpression... args) {
    Value left = args[0].eval(context);
    Value right = args[1].eval(context);

    return left.subtract(right);
  }

  @ScalarFunction(name = "MULTIPLY", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public static Value multiply(final IExpressionContext context, final IExpression... args) {
    Value left = args[0].eval(context);
    Value right = args[1].eval(context);

    return left.multiply(right);
  }

  @ScalarFunction(name = "DIVIDE", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public static Value divide(final IExpressionContext context, final IExpression... args) {
    Value left = args[0].eval(context);
    Value right = args[1].eval(context);

    return left.divide(right);
  }

  @ScalarFunction(name = "MOD", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public static Value mod(final IExpressionContext context, final IExpression... args) {
    Value left = args[0].eval(context);
    Value right = args[1].eval(context);

    return left.remainder(right);
  }

  @ScalarFunction(name = "POWER", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Mathematical")
  public static Value power(final IExpressionContext context, final IExpression... args) {
    Value left = args[0].eval(context);
    Value right = args[1].eval(context);

    return left.power(right);
  }

  // -------------------------------------------------------------
  // BITWISE
  // -------------------------------------------------------------


  @ScalarFunction(name = "BITNOT", category = "i18n::Operator.Category.Bitwise")
  public static Value bitnot(final IExpressionContext context, final IExpression... args) {
    Value value = args[0].eval(context);
    if (value.isNull())
      return value;

    return ValueInteger.of(~value.toInteger());
  }

  @ScalarFunction(name = "BITAND", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Bitwise")
  public static Value bitand(final IExpressionContext context, final IExpression... args) {
    Value left = args[0].eval(context);
    if (left.isNull())
      return left;
    Value right = args[1].eval(context);
    if (right.isNull())
      return right;

    return ValueInteger.of(left.toInteger() & right.toInteger());
  }

  @ScalarFunction(name = "BITOR", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Bitwise")
  public static Value bitor(final IExpressionContext context, final IExpression... args) {
    Value left = args[0].eval(context);
    if (left.isNull())
      return left;
    Value right = args[1].eval(context);
    if (right.isNull())
      return right;

    return ValueInteger.of(left.toInteger() | right.toInteger());
  }

  @ScalarFunction(name = "BITXOR", minArgs = 2, maxArgs = 2,
      category = "i18n::Operator.Category.Bitwise")
  public static Value bitxor(final IExpressionContext context, final IExpression... args) {
    Value left = args[0].eval(context);
    if (left.isNull())
      return left;
    Value right = args[1].eval(context);
    if (right.isNull())
      return right;

    return ValueInteger.of(left.toInteger() ^ right.toInteger());
  }



}
