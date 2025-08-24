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
import java.util.Arrays;
import java.util.Collection;
import java.util.NoSuchElementException;
import java.util.stream.Stream;
import org.apache.hop.expression.type.ArrayType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.Types;

/** An array is an immutable-ordered list of expressions. */
public final class Array implements IExpression, Iterable<IExpression> {

  public static final Array EMPTY = new Array();
  private final IExpression[] values;
  private Type type;

  public Array(IExpression... expressions) {
    this(Types.ARRAY, expressions);
  }

  public Array(Type type, IExpression... expressions) {
    this.type = type;
    this.values = expressions;
  }

  public Array(Collection<IExpression> expressions) {
    this(Types.ARRAY, expressions);
  }

  public Array(Type type, Collection<IExpression> expressions) {
    this.type = type;
    this.values = expressions.toArray(new IExpression[0]);
  }

  @Override
  public Kind getKind() {
    return Kind.ARRAY;
  }

  @Override
  public Type getType() {
    return type;
  }

  @Override
  public int getCost() {
    int cost = 2;
    for (IExpression expression : values) {
      cost += expression.getCost();
    }
    return cost;
  }

  @Override
  public Object getValue() {
    return this;
  }

  @Override
  public <T> T getValue(final Class<T> clazz) {
    return type.convert(this, clazz);
  }

  public IExpression get(int index) {
    return values[index];
  }

  public boolean isEmpty() {
    return values.length == 0;
  }

  public int size() {
    return values.length;
  }

  public Array slice(int form, int to) {
    return new Array(type, Arrays.copyOfRange(values, form, to));
  }

  /** Prepends an element to the beginning of an array. */
  public Array prepend(IExpression element) {
    int size = size();
    IExpression[] expressions = new IExpression[size + 1];
    System.arraycopy(values, 0, expressions, 1, size);
    expressions[0] = element;
    return new Array(type, expressions);
  }

  /** Appends an element to the end of an array. */
  public Array append(IExpression element) {
    int size = size();
    IExpression[] expressions = new IExpression[size + 1];
    System.arraycopy(values, 0, expressions, 0, size);
    expressions[size] = element;
    return new Array(type, expressions);
  }

  @Override
  public <E> E accept(IExpressionVisitor<E> visitor) {
    return visitor.visitArray(this);
  }

  @Override
  public boolean equals(Object other) {
    if (other instanceof Array array) {
      return Arrays.equals(values, array.values);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(values);
  }

  @Override
  public String toString() {
    StringWriter writer = new StringWriter();
    unparse(writer, 0, 0);
    return writer.toString();
  }

  @Override
  public void unparse(final StringWriter writer, int leftPrec, int rightPrec) {
    writer.append('[');
    this.unparseValues(writer);
    writer.append(']');
  }

  /**
   * Format values with a comma separator <code>value1,value2,...</code>
   *
   * @param writer
   */
  public void unparseValues(final StringWriter writer) {
    boolean first = true;
    for (IExpression expression : values) {
      if (first) first = false;
      else {
        writer.append(',');
      }
      expression.unparse(writer, 0, 0);
    }
  }

  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {

    // Empty array
    if (values.length == 0) {
      type = ArrayType.of(Types.ANY);
      return;
    }

    // Validate all elements
    for (IExpression expression : this) {
      expression.validate(context);
    }

    type = ArrayType.of(Types.getLeastRestrictive(this));

    // Don't validate multi-dimensional array
    if (type.is(TypeName.ARRAY)) {
      return;
    }

    // All elements of an array should be coercible
    for (IExpression expression : this) {
      if (!type.getElementType().isCoercible(expression.getType())) {
        throw new ExpressionException(ErrorCode.ILLEGAL_ARGUMENT, expression);
      }
    }
  }

  public Stream<IExpression> stream() {
    return Stream.of(values);
  }

  @Override
  public java.util.Iterator<IExpression> iterator() {
    return new ArrayIterator();
  }

  @Override
  public boolean isConstant() {
    for (IExpression expression : values) {
      if (!expression.isConstant()) {
        return false;
      }
    }
    return true;
  }

  /** Returns {@code true} if this array contains the specified elements. */
  public boolean contains(IExpression element) {
    for (IExpression expression : values) {
      if (element.equals(expression)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns {@code true} if this array contains all the elements of the specified collection.
   *
   * @param collection to be checked for containment in this array
   * @return {@code true} if this array contains all the elements of the specified collection
   */
  public boolean containsAll(Collection<IExpression> collection) {
    for (IExpression expression : collection) {
      if (!contains(expression)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Iterator implementation used to efficiently expose the contents of an Array as read-only iterator.
   */
  private class ArrayIterator implements java.util.Iterator<IExpression> {

    private int index;

    public ArrayIterator() {
      index = 0;
    }

    @Override
    public boolean hasNext() {
      return index < values.length;
    }

    @Override
    public IExpression next() {
      if (index >= values.length) {
        throw new NoSuchElementException();
      }
      return values[index++];
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException();
    }
  }
}
