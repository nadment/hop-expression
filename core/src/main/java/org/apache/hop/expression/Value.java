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

import java.lang.ref.SoftReference;
import java.math.BigDecimal;
import java.time.Instant;
import org.apache.hop.expression.value.ValueBigNumber;
import org.apache.hop.expression.value.ValueBinary;
import org.apache.hop.expression.value.ValueBoolean;
import org.apache.hop.expression.value.ValueDate;
import org.apache.hop.expression.value.ValueInteger;
import org.apache.hop.expression.value.ValueNull;
import org.apache.hop.expression.value.ValueNumber;
import org.apache.hop.expression.value.ValueString;
import org.apache.hop.i18n.BaseMessages;

/**
 * An value represents a boolean, numeric, string, date or timestamp constant, or the value NULL.
 *
 * <p>
 * Value is an immutable type.
 *
 * @author Nicolas ADMENT
 */
public abstract class Value implements IExpression, Comparable<Value> {

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  // TODO: move to Const.OBJECT_CACHE_SIZE];
  private static int OBJECT_CACHE_SIZE = 1024;

  private static SoftReference<Value[]> softCache;

  /** Boolean value of TRUE. */
  public static final Value TRUE = new ValueBoolean(true);

  /** Boolean value of FALSE. */
  public static final Value FALSE = new ValueBoolean(false);

  /** Unknown value of NULL. */
  public static final Value NULL = new ValueNull();

  /** Numeric literal value of PI. */
  public static final Value PI = new ValueNumber(Math.PI);


  /**
   * Get the boolean value for the given boolean.
   *
   * @param b the boolean
   * @return the value
   */
  public static Value of(boolean value) {
    return value ? Value.TRUE : Value.FALSE;
  }

  /**
   * Get the value for the given binary.
   *
   * @param b the binary
   * @return the value
   */
  public static Value of(byte[] value) {
    if (value == null || value.length == 0)
      return NULL;
    return new ValueBinary(value);
  }

  /**
   * Get the integer value for the given long.
   *
   * @param b the boolean
   * @return the value
   */
  public static Value of(Long value) {
    if (value == null)
      return Value.NULL;
    if (value == 0L)
      return ValueInteger.ZERO;
    if (value == 1L)
      return ValueInteger.ONE;

    return new ValueInteger(value);
  }

  /** Returns an integer Value that equals to {@code value}. */
  public static Value of(Integer value) {
    if (value == null)
      return Value.NULL;
    if (value == 0)
      return ValueInteger.ZERO;
    if (value == 1)
      return ValueInteger.ONE;

    return new ValueInteger(value.longValue());
  }

  /** Returns an big number Value that equals to {@code value}. */
  public static Value of(BigDecimal value) {
    if (value == null)
      return Value.NULL;
    if (BigDecimal.ZERO.equals(value))
      return ValueBigNumber.ZERO;
    if (BigDecimal.ONE.equals(value))
      return ValueBigNumber.ONE;

    return Value.cache(new ValueBigNumber(value));
  }

  public static Value of(Double value) {
    if (value == null || value.isNaN())
      return Value.NULL;
    if (value == 0.0)
      return ValueNumber.ZERO;
    if (value == 1.0)
      return ValueNumber.ONE;

    return new ValueNumber(value);
  }

  /** Returns an string Value that equals to {@code value}. */
  public static Value of(String value) {
    if (value == null)
      return Value.NULL;
    return new ValueString(value);
  }

  public static Value of(Instant value) {
    if (value == null)
      return Value.NULL;
    return new ValueDate(value);
  }

  @Override
  public Kind getKind() {
    return Kind.VALUE;
  }

  /**
   * Returns the type of this Value
   *
   * @return the type of this Value
   */
  public abstract DataType getType();

  /**
   * Returns the value object
   *
   * @return the value object
   */
  public abstract Object getObject();

  @Override
  public int getCost() {
    return 1;
  }

  /**
   * Compare this value against another value of the same data type.
   *
   * @param value the other value
   * @return 0 if both values are equal, -1 if this value is smaller, and 1 otherwise
   * @see compareTo
   */
  public abstract int compare(Value value);

  /**
   * Compare this value against another value. If values need to be converted to match the other
   * operands data type, the value with the lower order is converted to the value with the higher
   * order.
   *
   * @param value the other value
   * @return 0 if both values are equal, -1 if this value is smaller, and 1 otherwise
   */
  public int compareTo(Value right) {
    Value left = this;

    // If not the same data type;
    if (left.getType() != right.getType()) {
      if (left.isNull())
        return -1;
      if (right.isNull())
        return 1;

      // The lower order data type is converted
      if (left.getType().compareTo(right.getType()) > 0)
        right = right.convertTo(left.getType());
      else
        left = left.convertTo(right.getType());
    }

    return left.compare(right);
  }

  /**
   * Convert a value to the specified type.
   *
   * @param type the type of the returned value
   * @return the converted value
   */
  public Value convertTo(final DataType type) {

    if (this.getType() == type)
      return this;

    // System.out.println("Convert " + this.toString() + " from " + this.getType() + " to " +
    // targetType);

    switch (type) {
      case BOOLEAN:
        return new ValueBoolean(this.toBoolean());
      case INTEGER:
        return new ValueInteger(this.toInteger());
      case NUMBER:
        return new ValueNumber(this.toNumber());
      case BIGNUMBER:
        return new ValueBigNumber(this.toBigNumber());
      case STRING:
        return new ValueString(this.toString());
      case DATE:
        return new ValueDate(this.toDate());
      case BINARY:
        return new ValueBinary(this.toBinary());
      case NONE:
        return NULL;
      default:
        throw createUnsupportedConversionError(type);
    }
  }

  /**
   * Convert a value to the specified type.
   *
   * @param targetType the type of the returned value
   * @return the converted value
   */
  public Value convertTo(final IExpressionContext context, final DataType targetType,
      String format) {
    throw createUnsupportedConversionError(targetType);
  }

  /**
   * Check if the two values have the same hash code. No data conversion is made; this method
   * returns false if the other object is not of the same class. For some values, compareTo may
   * return 0 even if equals return false.
   * 
   * Example: ValueDecimal 0 and 0.0
   *
   * @param other the other value
   * @return true if they are equal
   */
  @Override
  public abstract boolean equals(Object other);

  @Override
  public abstract int hashCode();
  
  @Override
  public boolean isConstant() {
    return true;
  }

  /**
   * Returns -1 if the value is smaller than 0, 0 if zero, and otherwise 1.
   *
   * @return
   */
  public int signum() throws ExpressionException {
    throw new ExpressionException("Arithmetic SIGNUM error");
  }

  public Value negate() throws ExpressionException {
    throw new ExpressionException("Arithmetic NEGATE error");
  }

  /**
   * Checks whether or not the value is a String.
   *
   * @return true if the value is a String.
   */
  public boolean isString() {
    return this.getType() == DataType.STRING;
  }

  /**
   * Checks whether or not this Value is Numeric A Value is numeric if it is either of type
   * BigNumber, Number or Integer
   *
   * @return true if the value is either of type Number or Integer
   */
  public boolean isNumeric() {
    return this.isInteger() || this.isNumber() || this.isBigNumber();
  }

  /**
   * Checks whether or not this value is a boolean
   *
   * @return true if this value has type boolean.
   */
  public boolean isBoolean() {
    return this.getType() == DataType.BOOLEAN;
  }

  /**
   * Checks whether or not this value is an Integer
   *
   * @return true if this value is an integer
   */
  public boolean isInteger() {
    return this.getType() == DataType.INTEGER;
  }

  /**
   * Checks whether or not the value is a Number
   *
   * @return true is this value is a number
   */
  public boolean isNumber() {
    return this.getType() == DataType.NUMBER;
  }

  /**
   * Checks whether or not the value is a Big Number
   *
   * @return true is this value is a big number
   */
  public boolean isBigNumber() {
    return this.getType() == DataType.BIGNUMBER;
  }

  /**
   * Checks whether or not this value is a Date
   *
   * @return true if the value is a Date
   */
  public boolean isDate() {
    return this.getType() == DataType.DATE;
  }

  /**
   * Checks whether or not this value is of type Binary
   *
   * @return true if this value has type Binary
   */
  public boolean isBinary() {
    return this.getType() == DataType.BINARY;
  }

  /**
   * Checks if the value is null.
   *
   * @return true if the Value is null.
   */
  public boolean isNull() {
    return false;
  }

  /**
   * Get the value as a string.
   *
   * @return the string
   */
  public abstract String toString() throws ExpressionException;

  /**
   * Get the value as a boolean.
   *
   * @return the boolean value
   */
  public boolean toBoolean() throws ExpressionException {
    throw createUnsupportedConversionError(DataType.BOOLEAN);
  }

  public byte[] toBinary() throws ExpressionException {
    throw createUnsupportedConversionError(DataType.BINARY);
  }

  public long toInteger() throws ExpressionException {
    throw createUnsupportedConversionError(DataType.INTEGER);
  }

  public Instant toDate() throws ExpressionException {
    throw createUnsupportedConversionError(DataType.DATE);
  }

  public double toNumber() throws ExpressionException {
    throw createUnsupportedConversionError(DataType.NUMBER);
  }

  public BigDecimal toBigNumber() throws ExpressionException {
    throw createUnsupportedConversionError(DataType.BIGNUMBER);
  }

  /**
   * Add a value and return the result.
   *
   * @param v the value to add
   * @return the result
   */
  public Value add(Value v) {
    throw createUnsupportedOperationError("+");
  }

  /**
   * Subtract a value and return the result.
   *
   * @param v the value to subtract
   * @return the result
   */
  public Value subtract(Value v) {
    throw createUnsupportedOperationError("-");
  }

  /**
   * Divide by a value and return the result.
   *
   * @param v the divisor
   * @return the result
   */
  public Value divide(Value v) {
    throw createUnsupportedOperationError("/");
  }

  /**
   * Multiply with a value and return the result.
   *
   * @param v the value to multiply with
   * @return the result
   */
  public Value multiply(Value v) {
    throw createUnsupportedOperationError("*");
  }

  /**
   * Power with a value and return the result.
   *
   * @param v the value to power with
   * @return the result
   */
  public Value power(Value v) {
    throw createUnsupportedOperationError("^");
  }

  /**
   * Take the modulus with a value and return the result.
   *
   * @param v the value to take the modulus with
   * @return the result
   */
  public Value remainder(Value v) {
    throw createUnsupportedOperationError("%");
  }

  protected final ExpressionException createUnsupportedConversionError(DataType type) {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.UnsupportedConversion",
        this.toString(), this.getType(), type));
  }

  protected final ExpressionException createUnsupportedOperationError(String operation) {
    return new ExpressionException(BaseMessages.getString(PKG,
        "Expression.UnsupportedOperationWithDataType", operation, this.getType()));
  }

  /**
   * Creates new instance of the ExpressionException for value conversion error.
   *
   * @param targetType Target data type.
   * @return instance of the DbException.
   */
  protected final ExpressionException createConversionError(DataType to) {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Expression.ValueConversionError", this.getType(), to));
  }

  protected final ExpressionException createOverflowError() {
    return new ExpressionException(
        BaseMessages.getString(PKG, "Expression.Overflow", this.toString()));
  }

  @Override
  public IExpression optimize(IExpressionContext context) throws ExpressionException {
    return this;
  }

  /**
   * Check if a value is in the cache that is equal to this value. If yes, this value should be used
   * to save memory. If the value is not in the cache yet, it is added.
   *
   * @param v the value to look for
   * @return the value in the cache or the value passed
   */
  static Value cache(Value v) {
    int hash = v.hashCode();
    Value[] cache;
    if (softCache == null || (cache = softCache.get()) == null) {
      cache = new Value[OBJECT_CACHE_SIZE];
      softCache = new SoftReference<>(cache);
    }
    int index = hash & (OBJECT_CACHE_SIZE - 1); //
    Value cached = cache[index];
    if (cached != null && cached.getType() == v.getType() && v.equals(cached)) {
      return cached;
    }
    cache[index] = v;

    return v;
  }

  /**
   * Clear the value cache. Used for testing.
   */
  public static void clearCache() {
    softCache = null;
  }
}
