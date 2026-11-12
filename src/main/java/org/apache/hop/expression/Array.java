/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** An array is an immutable-ordered list of expressions. */
@NullMarked
public final class Array implements IExpression, Iterable<IExpression> {

  private final IExpression[] values;
  private Type type;

  public Array(IExpression... expressions) {
    this(ArrayType.ARRAY, expressions);
  }

  public Array(Type type, IExpression... expressions) {
    this.type = type;
    this.values = expressions;
  }

  public Array(Collection<IExpression> expressions) {
    this(ArrayType.ARRAY, expressions);
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
  public @Nullable <T> T getValue(final Class<T> clazz) {
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
    return new Array(typeWith(element), expressions).coerce();
  }

  /** Appends an element to the end of an array. */
  public Array append(IExpression element) {
    int size = size();
    IExpression[] expressions = new IExpression[size + 1];
    System.arraycopy(values, 0, expressions, 0, size);
    expressions[size] = element;
    return new Array(typeWith(element), expressions).coerce();
  }

  /**
   * Appends the elements of an array to the end of an array, the elements are converted to the most
   * general type of the elements of the two arrays.
   *
   * @param array the array to append
   * @return a new array
   */
  public Array concat(Array array) {
    int size = size();
    IExpression[] expressions = Arrays.copyOf(values, size + array.size());
    System.arraycopy(array.values, 0, expressions, size, array.size());
    Type elementType = type.getElementType();
    Type otherElementType = array.type.getElementType();
    if (otherElementType != null) {
      elementType = Types.getLeastRestrictive(elementType, otherElementType);
    }
    return new Array(elementType == null ? type : ArrayType.of(elementType, false), expressions)
        .coerce();
  }

  /**
   * Returns an array with the evaluated elements of this array. The elements of an array are
   * expressions evaluated each time they are read, so a function that tests the value of an element
   * and returns it, like a filter or a sort, must evaluate the elements once: the value of an
   * element like {@code RANDOM()} or the parameter of a lambda changes between two evaluations.
   *
   * @return this array if its elements are constant, otherwise a new array of constant elements
   */
  public Array evaluate() {
    if (isConstant()) {
      return this;
    }
    IExpression[] elements = new IExpression[values.length];
    for (int i = 0; i < values.length; i++) {
      Object value = values[i].getValue();
      // A nested array is an expression, not a literal value
      elements[i] =
          value instanceof Array array ? array.evaluate() : new Literal(value, values[i].getType());
    }
    return new Array(type, elements);
  }

  /** Converts the elements of this new array to its element type. */
  private Array coerce() {
    Type elementType = type.getElementType();
    for (int i = 0; i < values.length; i++) {
      values[i] = coerce(values[i], elementType);
    }
    return this;
  }

  /** The type of this array with one more element, which can widen the type of the elements. */
  private Type typeWith(IExpression element) {
    return ArrayType.of(Types.getLeastRestrictive(type.getElementType(), element.getType()), false);
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
  public void unparse(StringWriter writer, int leftPrec, int rightPrec) {
    // An empty array is written with its type, it has no element to give its type when parsed
    if (values.length == 0 && hasElementType()) {
      writer.append("CAST([] AS ");
      writer.append(type.getLiteral());
      writer.append(')');
      return;
    }
    writer.append('[');
    this.unparseValues(writer);
    writer.append(']');
  }

  /** Returns whether the type of the elements is known, an empty array constructor has none. */
  private boolean hasElementType() {
    Type elementType = type.getElementType();
    return elementType != null
        && !elementType.isName(TypeName.UNKNOWN)
        && !elementType.isName(TypeName.ANY);
  }

  /**
   * Format values with a comma separator <code>value1,value2,...</code>
   *
   * @param writer
   */
  public void unparseValues(StringWriter writer) {
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

    // An empty array has no element to give its type, it is typed by a cast or by the function
    // that returns it
    if (values.length == 0) {
      if (!hasElementType()) {
        throw new ExpressionException(ErrorCode.EMPTY_ARRAY_NOT_TYPED);
      }
      return;
    }

    // Validate all elements
    for (IExpression expression : this) {
      expression.validate(context);
    }

    Type elementType = Types.getLeastRestrictive(this);
    // An array constructor is never NULL
    type = ArrayType.of(elementType, false);

    // All elements of an array should be coercible to the element type, a string literal is
    // converted to the element type like in a comparison. The elements of a multi-dimensional array
    // are arrays, with elements coercible to the same type.
    for (IExpression expression : this) {
      if (!expression.getType().isCoercible(elementType) && !Types.isStringLiteral(expression)) {
        throw new ExpressionException(
            ErrorCode.ARRAY_ELEMENT_TYPE_MISMATCH, expression, elementType.getName());
      }
    }

    // Convert the elements to the element type, so [1,2.5] holds two numbers
    coerce();
  }

  /**
   * Converts an element to the element type of the array if it is of another type, a literal
   * element is converted immediately. The elements of a nested array constructor are converted to
   * the element type of the nested array, so {@code [[1],[2.5]]} holds two arrays of numbers.
   *
   * @param element the element
   * @param elementType the element type of the array
   * @return the element of the element type
   * @throws ExpressionException if a literal element can't be converted
   */
  private static IExpression coerce(IExpression element, @Nullable Type elementType) {
    // The elements of a nested array constructor are converted, an array returned by a function is
    // converted when its elements are read
    if (element instanceof Array array && elementType instanceof ArrayType) {
      if (array.type.equals(elementType)) {
        return array;
      }
      return new Array(elementType, array.values.clone()).coerce();
    }

    if (elementType == null
        || elementType.isName(TypeName.ANY)
        || elementType.isName(TypeName.UNKNOWN)
        || elementType.isName(TypeName.ARRAY)
        || element.getType().isName(TypeName.ARRAY)
        || element.getType().isName(elementType.getName())
        || element.isNull()) {
      return element;
    }

    Call cast = Types.cast(element, elementType);
    // Another constant like 1+2 is not compiled yet, its cast is evaluated by the compiler
    if (element instanceof Literal) {
      return new Literal(cast.getValue(), elementType);
    }
    return cast;
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

  @Override
  public boolean isDeterministic() {
    for (IExpression expression : values) {
      if (!expression.isDeterministic()) {
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
   * Iterator implementation used to efficiently expose the contents of an Array as a read-only
   * iterator.
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
