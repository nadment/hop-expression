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
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.Objects;
import lombok.Getter;
import org.apache.hop.core.util.TranslateUtil;
import org.apache.hop.expression.type.ComparableOperandTypeChecker;
import org.apache.hop.expression.type.IOperandCountRange;
import org.apache.hop.expression.type.IOperandTypeChecker;
import org.apache.hop.expression.type.IReturnTypeInference;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.util.DocumentationUtil;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Operators may be binary, unary, functions, special syntactic constructs like CASE ... WHEN ...
 * END, or even internally generated constructs like implicit type conversions.
 *
 * <p>Operators have the precedence levels. An operator with a lower precedence value binds more
 * tightly and is evaluated before an operator with a higher precedence value.
 */
@NullMarked
public abstract class Operator {
  /**
   * A {@code MathContext} object with precision 32 digits, and a rounding mode of {@link
   * RoundingMode#HALF_EVEN}.
   */
  public static final MathContext MATH_CONTEXT = new MathContext(32, RoundingMode.HALF_EVEN);

  /** The unique identifier of the operator/function. Example: "COS" or "TRIM" */
  @Getter private final String id;

  /**
   * The symbol of the operator or name of the function. Example: id is "TRUNCATE" but the alias
   * name is "TRUNC"
   */
  @Getter private final String name;

  /** The precedence value of the operator, a lower value binds more tightly. */
  @Getter private final int precedence;

  /** How operators of the same precedence are grouped. */
  @Getter private final Associativity associativity;

  /** Used to infer the return type of call to this operator. */
  @Getter private final IReturnTypeInference returnTypeInference;

  /** The strategy to validate operand types expected by this operator. */
  @Getter private final @Nullable IOperandTypeChecker operandTypeChecker;

  /** The category of this operator. */
  @Getter private final String category;

  @Getter private final String documentationUrl;

  @Getter private final @Nullable String documentation;

  /** Get the description of this operator. */
  @Getter private final @Nullable String description;

  /**
   * Create a new operator for use in expressions.
   *
   * @param id The unique identifier of operator
   * @param name The symbol of the operator or name of the function
   * @param precedence The precedence value of the operator
   * @param associativity Set if the operator is left, right or not associative
   * @param category The category to group operator
   */
  protected Operator(
      String id,
      String name,
      int precedence,
      Associativity associativity,
      IReturnTypeInference returnTypeInference,
      @Nullable IOperandTypeChecker operandTypeChecker,
      String category,
      String documentationUrl) {
    this.id = Objects.requireNonNull(id, "id");
    this.name = Objects.requireNonNull(name, "name");
    this.precedence = precedence;
    this.associativity = Objects.requireNonNull(associativity, "associativity");
    this.returnTypeInference = Objects.requireNonNull(returnTypeInference, "return type inference");
    this.operandTypeChecker = operandTypeChecker;
    this.category = TranslateUtil.translate(category, IExpression.class);
    this.documentationUrl = documentationUrl;
    this.documentation = DocumentationUtil.loadDocumentation(this.getClass(), id, documentationUrl);
    this.description = DocumentationUtil.findDocumentationDescription(this.documentation);
  }

  protected Operator(
      String id,
      int precedence,
      Associativity associativity,
      IReturnTypeInference returnTypeInference,
      @Nullable IOperandTypeChecker operandTypeChecker,
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

  /**
   * Returns the precedence limit for the operand to the left of this operator: an operand with a
   * greater or equal precedence must be enclosed in parentheses. A left-associative operator
   * accepts an operand of the same precedence: A-B-C.
   *
   * @return the precedence limit for the left operand
   */
  public int getLeftPrecedence() {
    return (associativity == Associativity.LEFT) ? precedence + 1 : precedence;
  }

  /**
   * Returns the precedence limit for the operand to the right of this operator: an operand with a
   * greater or equal precedence must be enclosed in parentheses. A right-associative operator
   * accepts an operand of the same precedence.
   *
   * @return the precedence limit for the right operand
   */
  public int getRightPrecedence() {
    return (associativity == Associativity.RIGHT) ? precedence + 1 : precedence;
  }

  protected static MethodHandle findMethodHandle(Class<?> clazz, String methodName) {
    try {
      MethodHandles.Lookup lookup = MethodHandles.publicLookup();
      MethodType methodType = MethodType.methodType(Object.class, IExpression[].class);
      return lookup.findVirtual(clazz, methodName, methodType);
    } catch (NoSuchMethodException | IllegalAccessException e) {
      throw new ExpressionException(ErrorCode.INTERNAL_ERROR, clazz);
    }
  }

  protected static MethodHandle findStaticMethodHandle(Class<?> clazz, String methodName) {
    try {
      MethodHandles.Lookup lookup = MethodHandles.publicLookup();
      MethodType methodType = MethodType.methodType(Object.class, IExpression[].class);
      return lookup.findStatic(clazz, methodName, methodType);
    } catch (NoSuchMethodException | IllegalAccessException e) {
      throw new ExpressionException(ErrorCode.INTERNAL_ERROR, clazz);
    }
  }

  /**
   * Raises an error if the integer part of the number exceeds the maximum precision of the NUMBER
   * type.
   *
   * @param number the result of an arithmetic operation
   * @return the number unchanged
   * @throws ExpressionException with {@link ErrorCode#ARITHMETIC_OVERFLOW} if the number overflows
   */
  protected BigDecimal checkNumberOverflow(BigDecimal number) throws ExpressionException {
    // The integer part of zero has no digit
    if (number.signum() != 0
        && number.precision() - number.scale() > TypeName.NUMBER.getMaxPrecision()) {
      throw new ExpressionException(ErrorCode.ARITHMETIC_OVERFLOW, getName());
    }
    return number;
  }

  /**
   * Casts and returns this expression as a {@link Call} if it is of kind {@code CALL}
   *
   * @return this instance cast to a class
   */
  protected static Call call(IExpression expression) {
    return (Call) expression;
  }

  /**
   * Casts and returns this expression as a {@link Array} if it is of kind {@code ARRAY}
   *
   * @return this instance cast to a class
   */
  protected static Array array(IExpression expression) {
    return (Array) expression;
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
   * Returns whether a call to this operator is not sensitive to the operand order. An operator is
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

  /**
   * Returns a constraint on the number of operands expected by this operator.
   *
   * @return acceptable range, or {@code null} if this operator has no operand type checker
   */
  public @Nullable IOperandCountRange getOperandCountRange() {
    return operandTypeChecker == null ? null : operandTypeChecker.getOperandCountRange();
  }

  @Override
  public boolean equals(@Nullable Object other) {
    if (other instanceof Operator operator) {
      return id.equals(operator.id) && name.equals(operator.name);
    }
    return false;
  }

  /**
   * Check if it's the same operator.
   *
   * @param other the operator to compare
   * @return true if the operator is the same
   */
  public boolean is(@Nullable Operator other) {
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
  public @Nullable Operator not() {
    return null;
  }

  /**
   * Returns the operator that has the same effect as this operator if its arguments are reversed.
   *
   * <p>By default, returns {@code null}, which means there is no inverse operator.
   */
  public @Nullable Operator reverse() {
    return null;
  }

  /**
   * Evaluate the result of the operator with operands.
   *
   * @return the result of the evaluation
   */
  public @Nullable Object eval(IExpression[] operands) {
    throw new UnsupportedOperationException(ErrorCode.INTERNAL_ERROR.message());
  }

  /**
   * Validates the operands of a call to this operator, before the number and the types of the
   * operands are checked.
   *
   * <p>By default, each operand is validated in the context. A function with a {@link Lambda}
   * operand overrides this method to give the type of the lambda parameter.
   *
   * @param context the context against which the operands are validated
   * @param call the call to validate
   * @throws ExpressionException if an operand is not valid
   */
  public void validateOperands(IExpressionContext context, Call call) throws ExpressionException {
    for (IExpression operand : call.getOperands()) {
      operand.validate(context);
    }
  }

  /**
   * Check the number of operands expected.
   *
   * @param call The call to check
   */
  public void checkOperandCount(Call call) {
    if (operandTypeChecker == null) return;

    IOperandCountRange operandCountRange = operandTypeChecker.getOperandCountRange();
    if (!operandCountRange.isValid(call.getOperandCount())) {
      if (call.getOperandCount() < operandCountRange.getMin()) {
        throw new ParseExpressionException(
            call.getPosition(), ErrorCode.NOT_ENOUGH_ARGUMENTS, this);
      }
      if (call.getOperandCount() > operandCountRange.getMax()) {
        throw new ParseExpressionException(call.getPosition(), ErrorCode.TOO_MANY_ARGUMENTS, this);
      }
    }
  }

  /**
   * Check operand types expected
   *
   * @param call The call to check
   * @return whether check succeeded
   */
  public boolean checkOperandTypes(Call call) {
    if (operandTypeChecker == null) return true;
    return operandTypeChecker.checkOperandTypes(call);
  }

  /**
   * Check operand types expected
   *
   * @param call The call to check
   * @param throwOnFailure whether to throw an exception if the check fails (otherwise returns false
   *     in that case)
   * @return whether check succeeded
   */
  public boolean checkOperandTypes(Call call, boolean throwOnFailure) {

    boolean success = checkOperandTypes(call);
    if (success) {
      return true;
    }

    if (throwOnFailure) {
      // A comparison like x = NULL is always NULL, the user most likely meant x IS NULL
      if (operandTypeChecker instanceof ComparableOperandTypeChecker
          && Arrays.stream(call.getOperands())
              .anyMatch(operand -> operand.getType().isName(TypeName.UNKNOWN))) {
        throw new ParseExpressionException(call.getPosition(), ErrorCode.COMPARISON_WITH_NULL);
      }
      throw new ParseExpressionException(call.getPosition(), ErrorCode.ILLEGAL_ARGUMENT_TYPE, this);
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
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    return call;
  }

  /**
   * Writes the source of a call to this operator with its operands, the reverse of the parsing.
   *
   * <p>The written source must be parsed again into the same call, with the same syntax as the one
   * read by the parser. The parentheses around the call, required by the precedence of the
   * surrounding operators, are written by {@link Call#unparse(StringWriter, int, int)}.
   *
   * @param writer the target writer
   * @param operands the operands of the call
   */
  public abstract void unparse(StringWriter writer, IExpression[] operands);

  @Override
  public String toString() {
    return id;
  }

  /**
   * Normalize reversible operator
   *
   * <ul>
   *   <li>swap term to identifier operator literal 1&gt;A → A&lt;1
   *   <li>ordering identifiers by name with case-sensitive B&gt;A → A&lt;B
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
    if (left instanceof Identifier && (right instanceof Literal || right instanceof Call)) {
      return call;
    }

    // Normalize identifier to the left
    if (right instanceof Identifier rightIdentifier) {
      // Swap terms and reverse operator 1>A → A<1
      if (left instanceof Literal) {
        return call.reverse();
      }
      // Swap terms and reverse operator B>A → A<B
      if (left instanceof Identifier leftIdentifier
          && leftIdentifier.getName().compareTo(rightIdentifier.getName()) > 0) {
        return call.reverse();
      }
    }

    // Normalize operator by moving the low-cost operand to the left
    if (left.getCost() > right.getCost()) {
      return call.reverse();
    }

    // If the same cost order with textual representation
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
    if (left instanceof Identifier && (right instanceof Literal || right instanceof Call)) {
      return call;
    }

    if (right instanceof Identifier rightIdentifier) {

      // Swap terms 1=A → A=1
      if (left instanceof Literal) {
        return call.swap();
      }
      // Swap terms B=A → A=B
      if (left instanceof Identifier leftIdentifier
          && leftIdentifier.getName().compareTo(rightIdentifier.getName()) > 0) {
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

  /**
   * Unparses an operand that immediately follows a sign. Parentheses are added if the operand
   * starts with a minus sign, because "--" starts a single line comment and "+-" is hard to read:
   * A-(-B), -(-1), +(-1).
   *
   * @param writer Target writer
   * @param operand The operand to unparse
   * @param leftPrec The precedence limit imposed by the operator to the left of the operand
   */
  protected static void unparseAfterSign(StringWriter writer, IExpression operand, int leftPrec) {
    StringWriter buffer = new StringWriter();
    operand.unparse(buffer, leftPrec, 0);
    String text = buffer.toString();
    if (text.startsWith("-")) {
      writer.append('(').append(text).append(')');
    } else {
      writer.append(text);
    }
  }

  /**
   * Associativity determines how operators of the same precedence are resolved to avoid ambiguity.
   */
  public enum Associativity {
    /** Operators of the same precedence are grouped from the left: a-b-c → (a-b)-c */
    LEFT,
    /** Operators of the same precedence are grouped from the right */
    RIGHT,
    /** Operators of the same precedence cannot be chained without parentheses: (a=b)=c */
    NONE
  }
}
