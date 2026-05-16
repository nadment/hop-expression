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
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.Objects;
import lombok.Getter;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.expression.type.AnyType;
import org.apache.hop.expression.type.Type;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Expression representing a named column in an input row. */
@NullMarked
public class Field extends Identifier {

  private final IRowExpressionContext context;

  /** The IValueMeta */
  @Getter private final IValueMeta valueMeta;

  public Field(IRowExpressionContext context, Type type, IValueMeta valueMeta, int ordinal) {
    super(0, valueMeta.getName());
    this.context = context;
    this.type = type;
    this.valueMeta = valueMeta;
    this.ordinal = ordinal;
  }

  @Override
  public @Nullable Object getValue() {
    Object[] row = context.getRow();

    if (row == null) {
      throw new ExpressionException(ErrorCode.CONTEXT_ERROR);
    }

    try {
      switch (valueMeta.getType()) {
        case IValueMeta.TYPE_BOOLEAN:
          return valueMeta.getBoolean(row[ordinal]);
        case IValueMeta.TYPE_TIMESTAMP:
          Timestamp timestamp = (Timestamp) valueMeta.getNativeDataType(row[ordinal]);
          if (timestamp == null) return null;
          return timestamp.toLocalDateTime().atZone(ZoneOffset.UTC);
        case IValueMeta.TYPE_DATE:
          Date date = valueMeta.getDate(row[ordinal]);
          if (date == null) return null;
          return ZonedDateTime.ofInstant(date.toInstant(), ZoneOffset.UTC);
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
          // Internal error
      }
    } catch (Exception e) {
      // Ignore
    }

    throw new ExpressionException(
        ErrorCode.CONVERSION_ERROR,
        valueMeta.getTypeDesc().toUpperCase(),
        AnyType.ANY,
        row[ordinal]);
  }

  @Override
  public @Nullable <T> T getValue(Class<T> clazz) {
    Object[] row = context.getRow();
    if (row == null) {
      throw new ExpressionException(ErrorCode.CONTEXT_ERROR);
    }

    try {
      switch (valueMeta.getType()) {
        case IValueMeta.TYPE_BOOLEAN:
          {
            Boolean value = valueMeta.getBoolean(row[ordinal]);
            return type.convert(value, clazz);
          }

        case IValueMeta.TYPE_TIMESTAMP:
          {
            Timestamp timestamp = (Timestamp) valueMeta.getNativeDataType(row[ordinal]);
            if (timestamp == null) {
              return null;
            }
            return type.convert(timestamp.toLocalDateTime().atZone(ZoneOffset.UTC), clazz);
          }

        case IValueMeta.TYPE_DATE:
          {
            Date date = valueMeta.getDate(row[ordinal]);
            if (date == null) {
              return null;
            }
            return type.convert(date.toInstant().atZone(ZoneOffset.UTC), clazz);
          }

        case IValueMeta.TYPE_STRING:
          {
            String value = valueMeta.getString(row[ordinal]);
            return type.convert(value, clazz);
          }

        case IValueMeta.TYPE_INTEGER:
          {
            Long value = valueMeta.getInteger(row[ordinal]);
            return type.convert(value, clazz);
          }

        case IValueMeta.TYPE_NUMBER:
          {
            Double value = valueMeta.getNumber(row[ordinal]);
            if (value == null) {
              return null;
            }
            if (clazz == Long.class) {
              return clazz.cast(value.longValue());
            }
            if (clazz == BigDecimal.class) {
              return clazz.cast(BigDecimal.valueOf(value));
            }
            if (clazz == Boolean.class) {
              return clazz.cast(value != 0);
            }
            if (clazz == String.class) {
              return clazz.cast(String.valueOf(value));
            }
            break;
          }

        case IValueMeta.TYPE_BIGNUMBER:
          {
            BigDecimal value = valueMeta.getBigNumber(row[ordinal]);
            return type.convert(value, clazz);
          }

        case IValueMeta.TYPE_BINARY:
          {
            byte[] value = valueMeta.getBinary(row[ordinal]);
            return type.convert(value, clazz);
          }

        case IValueMeta.TYPE_JSON, IValueMeta.TYPE_INET:
          {
            Object value = row[ordinal];
            return type.convert(value, clazz);
          }

        default:
          // Internal error
      }
    } catch (ExpressionException e) {
      throw e;
    } catch (Exception e) {
      // Ignore
    }
    throw new ExpressionException(
        ErrorCode.CONVERSION_ERROR, valueMeta.getTypeDesc().toUpperCase(), clazz, row[ordinal]);
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
