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

import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import java.util.Objects;

public final class ArrayType extends Type {

  /**
   * The maximum allowed cardinality of array.
   */
  public static final int MAX_ARRAY_CARDINALITY = Integer.MAX_VALUE;
  
  private final Type elementType;

  /**
   * Create an ARRAY data type with the specified element type.
   *
   * @param elementType
   *            the type of elements
   * @return ARRAY data type
   */
  public static ArrayType of(final Type type) {   
    return new ArrayType(type, true);
  }
  
  protected ArrayType(Type elementType, boolean nullable) {
    super(PRECISION_NOT_SPECIFIED, PRECISION_NOT_SPECIFIED, nullable);
    this.elementType = Objects.requireNonNull(elementType);
    this.signature = generateSignature();
    
    // TODO: Protect array from array
//    if ( elementType.is(TypeId.ARRAY)) {
//      throw new ExpressionException(ErrorCode.ILLEGAL_ARGUMENT_TYPE, elementType);
//    }
  }

  @Override
  public ArrayType withNullability(boolean nullable) {
    return new ArrayType(elementType, nullable);
  }

  @Override
  public TypeId getId() {
    return TypeId.ARRAY;
  }

  @Override
  public TypeComparability getComparability() {
    return elementType.getComparability();
  } 
  
  public Type getElementType() {
    return elementType;
  }

  @Override
  public Object cast(final Object value) throws ConversionException {
    throw new ConversionException(ErrorCode.INTERNAL_ERROR);
  }

  @Override
  public Object cast(final Object value, final String pattern) throws ConversionException {
    throw new ConversionException(ErrorCode.INTERNAL_ERROR);
  }
  
  @Override
  protected String generateSignature() {
    StringBuilder builder = new StringBuilder();
    builder.append(elementType.generateSignature());
    builder.append("[]");
    return builder.toString();
  }
}
