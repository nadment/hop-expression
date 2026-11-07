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

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;
import lombok.Getter;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.expression.type.AnyType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.util.DateConversion;
import org.apache.hop.expression.util.TimestampConversion;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Expression representing a named column in an input row. */
@NullMarked
public class Field extends Identifier {

  private final IRowExpressionContext context;

  /** The IValueMeta */
  @Getter private final IValueMeta valueMeta;

  /**
   * The class of the values stored in the row when they can be read without conversion, otherwise
   * {@code null}.
   */
  private final @Nullable Class<?> nativeClass;

  public Field(IRowExpressionContext context, Type type, IValueMeta valueMeta, int ordinal) {
    super(0, valueMeta.getName());
    this.context = context;
    this.type = type;
    this.valueMeta = valueMeta;
    this.ordinal = ordinal;
    this.nativeClass = findNativeClass(valueMeta);
  }

  /**
   * Finds the class of the values stored in the row, when they can be used without conversion.
   *
   * @param valueMeta the value meta of the field
   * @return the class, or {@code null} if a value must be converted when read
   */
  private static @Nullable Class<?> findNativeClass(IValueMeta valueMeta) {
    if (!valueMeta.isStorageNormal()) {
      return null;
    }
    return switch (valueMeta.getType()) {
      case IValueMeta.TYPE_BOOLEAN -> Boolean.class;
      case IValueMeta.TYPE_INTEGER -> Long.class;
      case IValueMeta.TYPE_BIGNUMBER -> BigDecimal.class;
      case IValueMeta.TYPE_BINARY -> byte[].class;
        // A string with a trim type is trimmed when read
      case IValueMeta.TYPE_STRING ->
          valueMeta.getTrimType() == IValueMeta.TRIM_TYPE_NONE ? String.class : null;
      default -> null;
    };
  }

  @Override
  public @Nullable Object getValue() {
    @Nullable Object[] row = context.getRow();

    if (row == null) {
      throw new ExpressionException(ErrorCode.CONTEXT_ERROR);
    }

    // Fast path, the value is read without conversion
    if (nativeClass != null) {
      Object value = row[ordinal];
      if (value == null || nativeClass.isInstance(value)) {
        return value;
      }
    }

    return convertValue(row);
  }

  /** Reads the value from the row with the conversion defined by the value meta. */
  private @Nullable Object convertValue(@Nullable Object[] row) {
    try {
      switch (valueMeta.getType()) {
        case IValueMeta.TYPE_BOOLEAN:
          return valueMeta.getBoolean(row[ordinal]);
        case IValueMeta.TYPE_TIMESTAMP:
          Timestamp timestamp = (Timestamp) valueMeta.getNativeDataType(row[ordinal]);
          if (timestamp == null) return null;
          return TimestampConversion.convert(timestamp);
        case IValueMeta.TYPE_DATE:
          Date date = valueMeta.getDate(row[ordinal]);
          if (date == null) return null;
          return DateConversion.convert(date);
        case IValueMeta.TYPE_STRING:
          return valueMeta.getString(row[ordinal]);
        case IValueMeta.TYPE_INTEGER:
          return valueMeta.getInteger(row[ordinal]);
        case IValueMeta.TYPE_NUMBER, IValueMeta.TYPE_BIGNUMBER:
          return valueMeta.getBigNumber(row[ordinal]);
        case IValueMeta.TYPE_JSON:
          return valueMeta.getNativeDataType(row[ordinal]);
        case IValueMeta.TYPE_BINARY:
          return valueMeta.getBinary(row[ordinal]);
        case IValueMeta.TYPE_INET:
          {
            return valueMeta.getNativeDataType(row[ordinal]);
          }
        default:
          break;
      }
    } catch (Exception e) {
      // Keep the reason given by the value meta
      throw new ExpressionException(
          e,
          ErrorCode.CONVERSION_ERROR,
          valueMeta.getTypeDesc().toUpperCase(Locale.ROOT),
          AnyType.ANY,
          row[ordinal]);
    }

    // Internal error, the type of the value meta is not supported
    throw new ExpressionException(
        ErrorCode.CONVERSION_ERROR,
        valueMeta.getTypeDesc().toUpperCase(Locale.ROOT),
        AnyType.ANY,
        row[ordinal]);
  }

  /**
   * Reads the value from the row, then converts it from the data type of the field to the specified
   * Java type.
   */
  @Override
  public @Nullable <T> T getValue(Class<T> clazz) {
    return type.convert(getValue(), clazz);
  }

  @Override
  public int hashCode() {
    return Objects.hash(valueMeta, ordinal);
  }

  @Override
  public boolean equals(@Nullable Object other) {
    if (other instanceof Field field) {
      return this.valueMeta.equals(field.valueMeta) && this.ordinal == field.ordinal;
    }
    return false;
  }
}
