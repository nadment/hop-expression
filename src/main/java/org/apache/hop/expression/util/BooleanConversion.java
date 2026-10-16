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
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.type.TypeName;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@NullMarked
public final class BooleanConversion {

  private BooleanConversion() {
    // Utility class
  }

  public static Boolean convert(Boolean value) {
    return value;
  }

  public static @Nullable Boolean convert(@Nullable Long value) {
    if (value == null) return null;
    return value != 0;
  }

  public static @Nullable Boolean convert(@Nullable BigDecimal value) {
    if (value == null) return null;
    return value.signum() != 0;
  }

  /**
   * Convert String value to Boolean, the leading and trailing spaces are ignored like PostgreSQL.
   *
   * @param value the string to convert
   * @return Boolean
   * @throws ExpressionException - When the conversion is not possible
   */
  public static @Nullable Boolean convert(@Nullable String value) {
    if (value == null) return null;
    String str = value.strip();
    switch (str.length()) {
      case 1:
        if (str.equals("1") || str.equalsIgnoreCase("t") || str.equalsIgnoreCase("y")) {
          return true;
        }
        if (str.equals("0") || str.equalsIgnoreCase("f") || str.equalsIgnoreCase("n")) {
          return false;
        }
        break;
      case 2:
        if (str.equalsIgnoreCase("on")) {
          return true;
        }
        if (str.equalsIgnoreCase("no")) {
          return false;
        }
        break;
      case 3:
        if (str.equalsIgnoreCase("yes")) {
          return true;
        }
        if (str.equalsIgnoreCase("off")) {
          return false;
        }
        break;
      case 4:
        if (str.equalsIgnoreCase("true")) {
          return true;
        }
        break;
      case 5:
        if (str.equalsIgnoreCase("false")) {
          return false;
        }
        break;
      default:
        break;
    }

    throw new ExpressionException(
        ErrorCode.CONVERSION_ERROR, TypeName.STRING, TypeName.BOOLEAN, value);
  }
}
