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
package org.apache.hop.expression.util;

import java.math.BigDecimal;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.type.TypeName;

public final class BooleanConversion extends Conversion<Boolean> {

  public static Boolean convert(final Boolean value) {
    return value;
  }

  public static Boolean convert(final Long value) {
    if (value == null) return null;
    return value != 0;
  }

  public static Boolean convert(final BigDecimal value) {
    if (value == null) return null;
    return value.signum() != 0;
  }

  public static Boolean convert(final String value) {
    if (value == null) return null;
    switch (value.length()) {
      case 1:
        if (value.equals("1") || value.equalsIgnoreCase("t") || value.equalsIgnoreCase("y")) {
          return true;
        }
        if (value.equals("0") || value.equalsIgnoreCase("f") || value.equalsIgnoreCase("n")) {
          return false;
        }
        break;
      case 2:
        if (value.equalsIgnoreCase("on")) {
          return true;
        }
        if (value.equalsIgnoreCase("no")) {
          return false;
        }
        break;
      case 3:
        if (value.equalsIgnoreCase("yes")) {
          return true;
        }
        if (value.equalsIgnoreCase("off")) {
          return false;
        }
        break;
      case 4:
        if (value.equalsIgnoreCase("true")) {
          return true;
        }
        break;
      case 5:
        if (value.equalsIgnoreCase("false")) {
          return false;
        }
        break;
      default:
        break;
    }

    throw new ConversionException(
        ErrorCode.CONVERSION_ERROR, TypeName.STRING, TypeName.BOOLEAN, value);
  }

  @Override
  public Class<Boolean> getConvertedType() {
    return Boolean.class;
  }

  @Override
  public TypeName getTypeName() {
    return TypeName.BOOLEAN;
  }
}
