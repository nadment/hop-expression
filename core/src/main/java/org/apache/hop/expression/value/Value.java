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
package org.apache.hop.expression.value;

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.i18n.BaseMessages;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.time.Instant;

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

  /** Unknown value of NULL. */
  public static final Value NULL = new Unknown();

  /** Implementation of NULL. NULL is not a regular data type. */
  private static final class Unknown extends Value {

    public Unknown() {
      // No value
    }

    @Override
    public DataType getType() {
      return DataType.NONE;
    }

    @Override
    public Object getObject() {
      return null;
    }

    @Override
    public boolean toBoolean() {
      return false;
    }

    @Override
    public String toString() {
      return null;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    public boolean equals(Object other) {
      return other == this;
    }

    @Override
    public boolean isNull() {
      return true;
    }

    @Override
    public Value negate() {
      return this;
    }

    @Override
    public Value eval(final IExpressionContext context) throws ExpressionException {
      return this;
    }

    public void write(StringWriter writer, int leftPrec, int rightPrec) {
      writer.append("NULL");
    }

    @Override
    public int compare(final Value v) {
      if (v.isNull())
        return 0;

      // null is always smaller
      return -1;
    }

    @Override
    public Value add(final Value v) {
      return this;
    }

    @Override
    public Value subtract(final Value v) {
      return this;
    }

    @Override
    public Value divide(final Value v) {
      return this;
    }

    @Override
    public Value multiply(Value v) {
      return this;
    }

    @Override
    public Value remainder(final Value v) {
      return this;
    }
    
    @Override
    public Value power(final Value v) {
      return this;
    }

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
  @Override
  public int compareTo(Value right) {
    Value left = this;

    // If not the same data type
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

    switch (type) {
      case BOOLEAN:
        return ValueBoolean.of(this.toBoolean());
      case INTEGER:
        return ValueInteger.of(this.toInteger());
      case NUMBER:
        return ValueNumber.of(this.toNumber());
      case BIGNUMBER:
        return ValueBigNumber.of(this.toBigNumber());
      case STRING:
        return ValueString.of(this.toString());
      case DATE:
        return ValueDate.of(this.toDate());
      case BINARY:
        return ValueBinary.of(this.toBinary());
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

  protected final ExpressionException createDivisionByZeroError() {
    return new ExpressionException(BaseMessages.getString(PKG, "Expression.DivisionByZero"));
  }

  @Override
  public IExpression optimize(IExpressionContext context) throws ExpressionException {
    return this;
  }
}
