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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.exception.ConversionException;

public final class IntervalType extends Type {

  public static final IntervalType YEAR_TO_MONTH = new IntervalType(TypeName.YEAR_TO_MONTH);
  
  public static final IntervalType DAY_TO_SECOND = new IntervalType(TypeName.DAY_TO_SECOND);

  private final String string;

  private IntervalType(TypeName name) {
    super(name);
    this.string = name.toString().replace('_', ' ').intern();
  }

  @Override
  public String toString() {
    return string;
  }
  @Override
  public Object cast(final Object value) throws ConversionException {
    throw new ConversionException(ExpressionError.INTERNAL_ERROR);
  }

  @Override
  public Object cast(final Object value, final String pattern) throws ConversionException {
    throw new ConversionException(ExpressionError.INTERNAL_ERROR);
  }
}

