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

import java.io.StringWriter;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.Objects;
import org.apache.hop.core.util.TranslateUtil;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.IReturnTypeInference;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.util.Documentations;

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

  /** The unique identifier of the operator/function. Ex. "COS" or "TRIM" */
  private final String id;

  /** The symbol of the operator or name of function. Ex. "id=TRUNCATE" but alias name is "TRUNC" */
  private final String name;

  /**
   * The precedence with which this operator binds to the expression to the left. This is less than
   * the right precedence if the operator is left-associative.
   */
  private final int leftPrec;

  /**
   * The precedence with which this operator binds to the expression to the right. This is great
   * than the left precedence if the operator is left-associative.
   */
  private final int rightPrec;

  /** Used to infer the return type of a call to this operator. */
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
   * @param isLeftAssociative Set to true if the operator is left associative, false if it is right
   *     associative
   * @param isDeterministic Set to true if the operator always returns the same result for the same
   *     parameters
   * @param category The category to group operator
   */
  protected Operator(
      String id,
      String name,
      int precedence,
      boolean isLeftAssociative,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String category,
      String documentationUrl) {
    this.id = Objects.requireNonNull(id, "id");
    this.name = Objects.requireNonNull(name, "name");
    this.leftPrec = leftPrecedence(precedence, isLeftAssociative);
    this.rightPrec = rightPrecedence(precedence, isLeftAssociative);
    this.returnTypeInference = Objects.requireNonNull(returnTypeInference, "return type inference");
    this.operandTypeChecker = operandTypeChecker;
    this.category = TranslateUtil.translate(category, IExpression.class);
    this.documentationUrl = documentationUrl;
    this.documentation = Documentations.loadDocumention(id, documentationUrl);
    this.description = Documentations.findDocumentionDescription(documentation);
  }

  protected Operator(
      String id,
      int precedence,
      boolean isLeftAssociative,
      IReturnTypeInference returnTypeInference,
      IOperandTypeChecker operandTypeChecker,
      String category,
      String documentationUrl) {
    this(
        id,
        id,
        precedence,
        isLeftAssociative,
        returnTypeInference,
        operandTypeChecker,
        category,
        documentationUrl);
  }

  private static int leftPrecedence(int precedence, boolean isLeftAssociative) {
    if (isLeftAssociative) {
      ++precedence;
    }
    return precedence;
  }

  private static int rightPrecedence(int precedence, boolean isLeftAssociative) {
    if (!isLeftAssociative) {
      ++precedence;
    }
    return precedence;
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
   * Returns whethe the operator always returns the same result for the same parameters.
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
   * @return {@code true} if this is a aggregate function.
   */
  public boolean isAggregate() {
    return false;
  }

  /**
   * Return type inference strategy.
   *
   * @return
   */
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
   * @param other
   * @return
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
   * @return
   */
  public String getCategory() {
    return category;
  }

  /**
   * Get the description of this operator.
   *
   * @return
   */
  public String getDescription() {
    return description;
  }

  /**
   * Evaluate the result of operator with operands.
   *
   * @return
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
        throw new ExpressionException(call.getPosition(), ErrorCode.NOT_ENOUGH_ARGUMENT, this);
      }
      if (call.getOperandCount() > operandCountRange.getMax()) {
        throw new ExpressionException(call.getPosition(), ErrorCode.TOO_MANY_ARGUMENT, this);
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
   * @param throwOnFailure whether to throw an exception if check fails(otherwise returns false in
   *     that case)
   * @return whether check succeeded
   */
  public boolean checkOperandTypes(final Call call, boolean throwOnFailure) {

    boolean success = checkOperandTypes(call);
    if (success) {
      return true;
    }

    if (throwOnFailure) {
      throw new ExpressionException(call.getPosition(), ErrorCode.ILLEGAL_ARGUMENT_TYPE, this);
    }

    return false;
  }

  /**
   * Infers the return type of an invocation of this operator; only called after the number and
   * types of operands have already been validated.
   *
   * @return inferred return type
   */
  public Type inferReturnType(Call call) {
    return returnTypeInference.inferReturnType(call);
  }

  /**
   * Derives the operands type of a call for this operator.
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
   * @param call The call to compiled
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

  protected static MethodHandle findMethodHandle(final Class<?> clazz, final String methodName) {
    try {
      MethodHandles.Lookup lookup = MethodHandles.publicLookup();
      MethodType methodType = MethodType.methodType(Object.class, IExpression[].class);
      return lookup.findVirtual(clazz, methodName, methodType);
    } catch (NoSuchMethodException | IllegalAccessException e) {
      throw new ExpressionException(0, ErrorCode.INTERNAL_ERROR, clazz);
    }
  }

  protected static MethodHandle findStaticMethodHandle(
      final Class<?> clazz, final String methodName) {
    try {
      MethodHandles.Lookup lookup = MethodHandles.publicLookup();
      MethodType methodType = MethodType.methodType(Object.class, IExpression[].class);
      return lookup.findStatic(clazz, methodName, methodType);
    } catch (NoSuchMethodException | IllegalAccessException e) {
      throw new ExpressionException(0, ErrorCode.INTERNAL_ERROR, clazz);
    }
  }
}
