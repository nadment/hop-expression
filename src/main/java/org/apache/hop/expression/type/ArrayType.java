/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.hop.expression.type;

import java.util.Objects;
import org.apache.hop.expression.Array;
import org.apache.hop.expression.IExpression;
import org.jspecify.annotations.NonNull;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public final class ArrayType extends Type {
  public static final ArrayType ARRAY = new ArrayType(UnknownType.UNKNOWN, true);

  /** The maximum allowed cardinality of the array. */
  public static final int MAX_ARRAY_CARDINALITY = Integer.MAX_VALUE;

  private final Type elementType;

  ArrayType(Type elementType, boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, PRECISION_NOT_SPECIFIED, nullable);
    this.elementType = Objects.requireNonNull(elementType);
  }

  /**
   * Create an ARRAY data type with the specified element type.
   *
   * @param type the type of elements
   * @return ARRAY data type
   */
  public static ArrayType of(final Type type) {
    return new ArrayType(type, true);
  }

  /**
   * Create an ARRAY data type with the specified element type and nullability.
   *
   * @param type the type of elements
   * @param nullable whether the array can be NULL, an array constructor is never NULL
   * @return ARRAY data type
   */
  public static ArrayType of(final Type type, boolean nullable) {
    return new ArrayType(type, nullable);
  }

  @Override
  public ArrayType withNullability(boolean nullable) {
    if (nullable == this.isNullable()) {
      return this;
    }
    return new ArrayType(elementType, nullable);
  }

  @Override
  public TypeName getName() {
    return TypeName.ARRAY;
  }

  @Override
  public TypeComparability getComparability() {
    // An array without element, like [], is comparable with any array
    if (elementType.isName(TypeName.ANY)) {
      return TypeComparability.ALL;
    }
    return elementType.getComparability();
  }

  /**
   * {@inheritDoc}
   *
   * <p>An array type always has an element type, it is never null.
   */
  @Override
  public @NonNull Type getElementType() {
    return elementType;
  }

  /**
   * {@inheritDoc}
   *
   * <p>An array is coercible to another array if its elements are coercible to the type of the
   * elements of the other array, so {@code ARRAY<INTEGER>} is coercible to {@code ARRAY<NUMBER>}
   * but not to {@code ARRAY<DATE>} or {@code ARRAY<ARRAY<INTEGER>>}.
   */
  @Override
  public boolean isCoercible(Type type) {
    if (type instanceof ArrayType arrayType) {
      // The type of the elements is not known, like the array of a NULL literal
      return arrayType.elementType.isName(TypeName.UNKNOWN)
          || elementType.isCoercible(arrayType.elementType);
    }
    return super.isCoercible(type);
  }

  /**
   * {@inheritDoc}
   *
   * <p>Two arrays are equal if they have the same number of elements and their elements are equal
   * in the same order, two NULL elements are equal.
   */
  @Override
  public boolean compareEqual(final Object left, final Object right) {
    if (left instanceof Array array1 && right instanceof Array array2) {
      int size = array1.size();
      if (size != array2.size()) {
        return false;
      }
      for (int i = 0; i < size; i++) {
        if (!elementType.compareEqualNull(getElement(array1, i), getElement(array2, i))) {
          return false;
        }
      }
      return true;
    }
    return super.compareEqual(left, right);
  }

  /**
   * {@inheritDoc}
   *
   * <p>Two arrays are compared element by element, the first different element gives the result. A
   * NULL element is greater than any other element. If all the elements are equal, the array with
   * fewer elements is the lesser.
   */
  @Override
  public int compare(final Object left, final Object right) {
    if (left instanceof Array array1 && right instanceof Array array2) {
      int size = Math.min(array1.size(), array2.size());
      for (int i = 0; i < size; i++) {
        Object element1 = getElement(array1, i);
        Object element2 = getElement(array2, i);
        if (element1 == null || element2 == null) {
          if (element1 == element2) continue;
          return element1 == null ? 1 : -1;
        }
        int result = elementType.compare(element1, element2);
        if (result != 0) {
          return result;
        }
      }
      return Integer.compare(array1.size(), array2.size());
    }
    return super.compare(left, right);
  }

  /**
   * Returns the value of an element converted to the element type of this array type, so arrays
   * with elements of different types like INTEGER and NUMBER are compared.
   */
  private @Nullable Object getElement(Array array, int index) {
    IExpression element = array.get(index);
    if (elementType.isName(TypeName.ANY) || elementType.isName(TypeName.UNKNOWN)) {
      return element.getValue();
    }
    return element.getValue(elementType.getName().getJavaClass());
  }

  @Override
  protected String generateSignature() {
    // The nullability of the array follows the nullability of its elements, like
    // ARRAY<INTEGER NOT NULL> NOT NULL
    String signature = "ARRAY<" + elementType.getSignature() + ">";
    return isNullable() ? signature : signature + NOT_NULL_SUFFIX;
  }
}
