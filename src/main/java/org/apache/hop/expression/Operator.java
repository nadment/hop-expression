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

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.Objects;
import org.apache.commons.io.IOUtils;
import org.apache.hop.core.logging.ILogChannel;
import org.apache.hop.core.logging.LogChannel;
import org.apache.hop.core.util.TranslateUtil;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.IReturnTypeInference;
import org.apache.hop.expression.type.Type;

/**
 * Operators may be binary, unary, functions, special syntactic constructs like CASE ... WHEN ...
 * END, or even internally generated constructs like implicit type conversions.
 *
 * <p>Operators have the precedence levels. An operator on higher levels is evaluated before an
 * operator on a lower level
 */
public abstract class Operator {
  /**
   * A {@code MathContext} object with a precision 32 digits, and a rounding mode of {@link
   * RoundingMode#HALF_EVEN}.
   */
  public static final MathContext MATH_CONTEXT = new MathContext(32, RoundingMode.HALF_EVEN);

  private static final ILogChannel LOG = new LogChannel("Expression");

  /** The unique identifier of the operator/function. Example: "COS" or "TRIM" */
  private final String id;

  /**
   * The symbol of the operator or name of function. Example: id is "TRUNCATE" but alias name is
   * "TRUNC"
   */
  private final String name;

  /**
   * The precedence with which this operator binds to the expression to the left. This is less than
   * the right precedence if the operator is left-associative.
   */
  private final int leftPrec;

  /**
   * The precedence with which this operator binds to the expression to the right. This is greater
   * than the left precedence if the operator is left-associative.
   */
  private final int rightPrec;

  /** Used to infer the return type of call to this operator. */
  private final IReturnTypeInference returnTypeInference;

  /** Used to validate operand types. */
  private final IOperandTypeChecker operandTypeChecker;

  private final String category;
  private final String documentationUrl;
  private final String documentation;
  private final String description;

  /**
   * Create a new operator for use in expressions.
   *
   * @param id The unique identifier of operator
   * @param name The symbol of the operator or name of function
   * @param precedence The precedence value of the operator
   * @param associativity Set if the operator is left or right associative
   * @param category The category to group operator
   */
  protected Operator(
      String id,
      String name,
      int precedence,
      Associativity associativity,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String category,
      String documentationUrl) {
    this.id = Objects.requireNonNull(id, "id");
    this.name = Objects.requireNonNull(name, "name");
    this.leftPrec = leftPrecedence(precedence, associativity);
    this.rightPrec = rightPrecedence(precedence, associativity);
    this.returnTypeInference = Objects.requireNonNull(returnTypeInference, "return type inference");
    this.operandTypeChecker = operandTypeChecker;
    this.category = TranslateUtil.translate(category, IExpression.class);
    this.documentationUrl = documentationUrl;
    this.documentation = loadDocumentation();
    this.description = findDocumentationDescription();
  }

  protected Operator(
      String id,
      int precedence,
      Associativity associativity,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String category,
      String documentationUrl) {
    this(
        id,
        id,
        precedence,
        associativity,
        returnTypeInference,
        operandTypeChecker,
        category,
        documentationUrl);
  }

  private static int leftPrecedence(int precedence, Associativity associativity) {
    if (associativity == Associativity.LEFT) {
      ++precedence;
    }
    return precedence;
  }

  private static int rightPrecedence(int precedence, Associativity associativity) {
    if (associativity == Associativity.RIGHT) {
      ++precedence;
    }
    return precedence;
  }

  protected static MethodHandle findMethodHandle(final Class<?> clazz, final String methodName) {
    try {
      MethodHandles.Lookup lookup = MethodHandles.publicLookup();
      MethodType methodType = MethodType.methodType(Object.class, IExpression[].class);
      return lookup.findVirtual(clazz, methodName, methodType);
    } catch (NoSuchMethodException | IllegalAccessException e) {
      throw new ExpressionException(ErrorCode.INTERNAL_ERROR, clazz);
    }
  }

  protected static MethodHandle findStaticMethodHandle(
      final Class<?> clazz, final String methodName) {
    try {
      MethodHandles.Lookup lookup = MethodHandles.publicLookup();
      MethodType methodType = MethodType.methodType(Object.class, IExpression[].class);
      return lookup.findStatic(clazz, methodName, methodType);
    } catch (NoSuchMethodException | IllegalAccessException e) {
      throw new ExpressionException(ErrorCode.INTERNAL_ERROR, clazz);
    }
  }

  /**
   * Casts and returns this expression as a {@link Call} if it is of kind {@code CALL}
   *
   * @return this instance cast to a class
   */
  protected static Call call(final IExpression expression) {
    return (Call) expression;
  }

  /**
   * Casts and returns this expression as a {@link Identifier} if it is of kind {@code IDENTIFIER}
   *
   * @return this instance cast to a class
   */
  protected static Identifier identifier(final IExpression expression) {
    return (Identifier) expression;
  }

  /**
   * Casts and returns this expression as a {@link Array} if it is of kind {@code ARRAY}
   *
   * @return this instance cast to a class
   */
  protected static Array array(final IExpression expression) {
    return (Array) expression;
  }

  /** The unique identifier of the operator */
  public String getId() {
    return id;
  }

  /** The name of the operator */
  public String getName() {
    return name;
  }

  public int getLeftPrec() {
    return leftPrec;
  }

  public int getRightPrec() {
    return rightPrec;
  }

  /**
   * Returns whether the operator always returns the same result for the same parameters.
   *
   * @return {@code true} if this is a deterministic function
   */
  public boolean isDeterministic() {
    return true;
  }

  /**
   * Returns whether a call to this operator is not sensitive to the operands order. An operator is
   * symmetrical if the call returns the same result when the operands are permuted.
   */
  public boolean isSymmetrical() {
    return false;
  }

  /**
   * Returns whether this function is an aggregate function.
   *
   * @return {@code true} if this is an aggregate function.
   */
  public boolean isAggregate() {
    return false;
  }

  /** Return type inference strategy. */
  public final IReturnTypeInference getReturnTypeInference() {
    return this.returnTypeInference;
  }

  /** Returns a strategy to validate operand types expected by this operator. */
  public IOperandTypeChecker getOperandTypeChecker() {
    return operandTypeChecker;
  }

  /**
   * Returns a constraint on the number of operands expected by this operator.
   *
   * @return acceptable range
   */
  public IOperandCountRange getOperandCountRange() {
    return operandTypeChecker.getOperandCountRange();
  }

  public String getDocumentationUrl() {
    return this.documentationUrl;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (!obj.getClass().equals(this.getClass())) {
      return false;
    }

    Operator other = (Operator) obj;
    return id.equals(other.id) && name.equals(other.name);
  }

  /**
   * Check if it's the same operator.
   *
   * @param other the operator to compare
   * @return true if operator are the same
   */
  public boolean is(Operator other) {
    if (other == null) return false;
    return id.equals(other.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, name);
  }

  /**
   * Returns the operator that is the logical inverse of this operator.
   *
   * <p>For example, {@code Operators.IS_TRUE} returns {@code Operators.IS_NOT_TRUE}, and vice
   * versa.
   *
   * <p>By default, returns {@code null}, which means there is no inverse operator.
   */
  public Operator not() {
    return null;
  }

  /**
   * Returns the operator that has the same effect as this operator if its arguments are reversed.
   *
   * <p>By default, returns {@code null}, which means there is no inverse operator.
   */
  public Operator reverse() {
    return null;
  }

  /**
   * Get the category of this operator.
   *
   * @return the category
   */
  public String getCategory() {
    return category;
  }

  /** Get the description of this operator. */
  public String getDescription() {
    return description;
  }

  /**
   * Evaluate the result of operator with operands.
   *
   * @return the result of the evaluation
   */
  public Object eval(final IExpression[] operands) {
    throw new UnsupportedOperationException(ErrorCode.INTERNAL_ERROR.message());
  }

  /**
   * Check the number of operands expected.
   *
   * @param call The call to check
   */
  public void checkOperandCount(final Call call) {
    if (operandTypeChecker == null) return;

    IOperandCountRange operandCountRange = operandTypeChecker.getOperandCountRange();
    if (!operandCountRange.isValid(call.getOperandCount())) {
      if (call.getOperandCount() < operandCountRange.getMin()) {
        throw new ExpressionParseException(call.getPosition(), ErrorCode.NOT_ENOUGH_ARGUMENT, this);
      }
      if (call.getOperandCount() > operandCountRange.getMax()) {
        throw new ExpressionParseException(call.getPosition(), ErrorCode.TOO_MANY_ARGUMENT, this);
      }
    }
  }

  /**
   * Check operand types expected
   *
   * @param call The call to check
   * @return whether check succeeded
   */
  public boolean checkOperandTypes(final Call call) {
    if (operandTypeChecker != null) {
      return operandTypeChecker.checkOperandTypes(call);
    }
    return true;
  }

  /**
   * Check operand types expected
   *
   * @param call The call to check
   * @param throwOnFailure whether to throw an exception if check fails (otherwise returns false in
   *     that case)
   * @return whether check succeeded
   */
  public boolean checkOperandTypes(final Call call, boolean throwOnFailure) {

    boolean success = checkOperandTypes(call);
    if (success) {
      return true;
    }

    if (throwOnFailure) {
      throw new ExpressionParseException(call.getPosition(), ErrorCode.ILLEGAL_ARGUMENT, this);
    }

    return false;
  }

  /**
   * Infers the return type of invocation of this operator; only called after the number and types
   * of operands have already been validated.
   *
   * @return inferred return type
   */
  public Type inferReturnType(Call call) {
    return returnTypeInference.inferReturnType(call);
  }

  /**
   * Derives the operand type of call for this operator.
   *
   * @param call The call to coerce
   */
  public boolean coerceOperandsType(Call call) {

    // Inferring the return type
    inferReturnType(call);

    return false;
  }

  /**
   * Compile and optimize the call
   *
   * @param context The context against which the call will be compiled.
   * @param call The call to compile
   * @return compiled The compiled expression
   * @throws ExpressionException if an error occurs.
   */
  public IExpression compile(final IExpressionContext context, final Call call)
      throws ExpressionException {
    return call;
  }

  public abstract void unparse(StringWriter writer, IExpression[] operands);

  public String getDocumentation() {
    return this.documentation;
  }

  @Override
  public String toString() {
    return id;
  }

  /**
   * Normalize reversible operator
   *
   * <ul>
   *   <li>swap term to identifier operator literal 1>A → A<1
   *   <li>ordering identifiers by name with case-sensitive B>A → A<B
   *   <li>ordering the low-cost operand to the left
   * </ul>
   *
   * @param call The call to normalize
   * @return normalized call
   */
  protected Call normalizeReversiblePredicate(Call call) {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Nothing to normalize
    if (left.is(Kind.IDENTIFIER) && (right.is(Kind.LITERAL) || right.is(Kind.CALL))) {
      return call;
    }

    // Normalize identifier to the left
    if (right.is(Kind.IDENTIFIER)) {
      // Swap terms and reverse operator 1>A → A<1
      if (left.is(Kind.LITERAL)) {
        return call.reverse();
      }
      // Swap terms and reverse operator B>A → A<B
      if (left.is(Kind.IDENTIFIER)
          && identifier(left).getName().compareTo(identifier(right).getName()) > 0) {
        return call.reverse();
      }
    }

    // Normalize operator by moving the low-cost operand to the left
    if (left.getCost() > right.getCost()) {
      return call.reverse();
    }

    // If same cost order with textual representation
    //    if (left.toString().compareTo(right.toString()) > 0) {
    //      System.out.println("Reverse term by textual");
    //      return reverse(call);
    //    }

    return call;
  }

  /**
   * Normalize symmetrical operator
   *
   * <ul>
   *   <li>swap term to identifier operator literal 1=A → A=1
   *   <li>ordering identifiers by name (case-sensitive) B=A → A=B
   *   <li>ordering the low-cost operand to the left
   * </ul>
   *
   * @param call The call to normalize
   * @return normalized call
   */
  protected Call normalizeSymmetricalPredicate(Call call) {
    IExpression left = call.getOperand(0);
    IExpression right = call.getOperand(1);

    // Nothing to normalize
    if (left.is(Kind.IDENTIFIER) && (right.is(Kind.LITERAL) || right.is(Kind.CALL))) {
      return call;
    }

    if (right.is(Kind.IDENTIFIER)) {

      // Swap terms 1=A → A=1
      if (left.is(Kind.LITERAL)) {
        return call.swap();
      }
      // Swap terms B=A → A=B
      if (left.is(Kind.IDENTIFIER)
          && identifier(left).getName().compareTo(identifier(right).getName()) > 0) {
        return call.swap();
      }
    }

    // Normalize operator by moving the low-cost operand to the left
    if (left.getCost() > right.getCost()) {
      return call.swap();
    }

    // If same cost order with textual representation
    //    if (left.toString().compareTo(right.toString()) > 0) {
    //      System.out.println("Swap term by textual");
    //      return swap(call);
    //    }

    return call;
  }

  private String loadDocumentation() {
    try (StringWriter writer = new StringWriter()) {
      InputStream is = this.getClass().getResourceAsStream(documentationUrl);
      if (is != null) {
        InputStreamReader reader = new InputStreamReader(is);
        IOUtils.copy(reader, writer);
      }
      return writer.toString();
    } catch (Exception e) {
      LOG.logError("Missing operator documentation: {0}", id);
      return null;
    }
  }

  private String findDocumentationDescription() {
    if (documentation == null) {
      return "";
    }

    int beginIndex = documentation.indexOf("id=\"preamble\"");
    beginIndex = documentation.indexOf("<p>", beginIndex);

    if (beginIndex > 0) {
      int endIndex = documentation.indexOf("</p>", beginIndex);

      return documentation.substring(beginIndex + 3, endIndex);
    }

    return "";
  }

  /**
   * Associativity determines how operators of the same precedence are resolved to avoid ambiguity.
   */
  public enum Associativity {
    LEFT,
    RIGHT
  }
}
