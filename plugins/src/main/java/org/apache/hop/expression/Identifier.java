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

import static java.util.Objects.requireNonNull;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaJson;
import org.apache.hop.expression.exception.ConversionException;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeId;
import org.apache.hop.expression.type.Types;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.Objects;

/**
 * Expression representing a named column in an input row.
 */
public class Identifier implements IExpression {
  
  private IRowExpressionContext context;

  // The name of the identifier
  private final String name;
  // The data type when resolved or UNKNOWN if unresolved.
  private Type type;
  // The position in the expression source
  private int position;  
  // The index in IRowMeta when resolved or -1 if unresolved.
  private int ordinal;
  // The IValueMeta when resolved or null if unresolved.
  private IValueMeta valueMeta;

  
  public Identifier(final String name) {
    this(0, name);
  }
  
  public Identifier(int position, final String name) {
    this.name = requireNonNull(name, "name");
    this.type = Types.UNKNOWN;
    this.position = position;
    this.context = null;
    this.valueMeta = null;
    this.ordinal = -1;
  }

  @Override
  public Kind getKind() {
    return Kind.IDENTIFIER;
  }

  /**
   * Return the name of the identifier.
   * 
   * @return
   */
  public String getName() {
    return name;
  }

  @Override
  public Type getType() {
    return type;
  }

  @Override
  public int getCost() {
    return 2;
  }

  /**
   * Return the index in IRowMeta when resolved or -1 if unresolved identifier.
   * 
   * @return
   */
  public int getIndex() {
    return ordinal;
  }

  @Override
  public Object getValue() {
    Object[] row = context.getRow();
    if (row == null) {
      throw new ExpressionException(position, ErrorCode.CONTEXT_ERROR);
    }

    try {
      switch (valueMeta.getType()) {
        case IValueMeta.TYPE_BOOLEAN:
          return valueMeta.getBoolean(row[ordinal]);
        case IValueMeta.TYPE_TIMESTAMP:
          Timestamp timestamp = (Timestamp) valueMeta.getNativeDataType(row[ordinal]);
          if (timestamp == null)
            return null;
          return timestamp.toLocalDateTime().atZone(ZoneId.systemDefault());
        case IValueMeta.TYPE_DATE:
          Date date = valueMeta.getDate(row[ordinal]);
          if (date == null)
            return null;
          return ZonedDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
        case IValueMeta.TYPE_STRING:          
          return valueMeta.getString(row[ordinal]);
        case IValueMeta.TYPE_INTEGER:
          return valueMeta.getInteger(row[ordinal]);
        case IValueMeta.TYPE_NUMBER:
        case IValueMeta.TYPE_BIGNUMBER:
          return valueMeta.getBigNumber(row[ordinal]);
        case ValueMetaJson.TYPE_JSON:
          return valueMeta.getNativeDataType(row[ordinal]);
        case IValueMeta.TYPE_BINARY:
          return valueMeta.getBinary(row[ordinal]);
        default:
          // Internal error
      }
    } catch (Exception e) {
      // Ignore
    }
    throw new ExpressionException(position, ErrorCode.CONVERSION_ERROR, valueMeta.getTypeDesc().toUpperCase(), row[ordinal]);
  }

  @Override
  public <T> T getValue(Class<T> clazz) {
    Object[] row = context.getRow();
    if (row == null) {
      throw new ExpressionException(position, ErrorCode.CONTEXT_ERROR);
    }

    try {
      switch (valueMeta.getType()) {
        case IValueMeta.TYPE_BOOLEAN: {
          Boolean value = valueMeta.getBoolean(row[ordinal]);
          return type.convert(value, clazz);
        }

        case IValueMeta.TYPE_TIMESTAMP: {
          Timestamp timestamp = (Timestamp) valueMeta.getNativeDataType(row[ordinal]);
          if (timestamp == null) {
            return null;
          }
          return clazz.cast(timestamp.toLocalDateTime().atZone(ZoneId.systemDefault()));
        }

        case IValueMeta.TYPE_DATE: {
          Date date = valueMeta.getDate(row[ordinal]);
          if (date == null) {
            return null;
          }
          return clazz.cast(date.toInstant().atZone(ZoneId.systemDefault()));
        }

        case IValueMeta.TYPE_STRING: {
          String value = valueMeta.getString(row[ordinal]);
          return type.convert(value, clazz);
        }

        case IValueMeta.TYPE_INTEGER: {
          Long value = valueMeta.getInteger(row[ordinal]);
          return type.convert(value, clazz);
        }

        case IValueMeta.TYPE_NUMBER: {
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

        case IValueMeta.TYPE_BIGNUMBER: {
          BigDecimal value = valueMeta.getBigNumber(row[ordinal]);
          return type.convert(value, clazz);
        }

        case ValueMetaJson.TYPE_JSON: {
          Object value = row[ordinal];
          return type.convert(value, clazz);
        }

        case IValueMeta.TYPE_BINARY: {
          byte[] value = valueMeta.getBinary(row[ordinal]);
          return type.convert(value, clazz);
        }

        default:
           // Internal error
      }
    } catch (ConversionException e) {
      throw e;
    } catch (Exception e) {
      // Ignore
    }
    throw new ExpressionException(position, ErrorCode.CONVERSION_ERROR, valueMeta.getTypeDesc().toUpperCase(), row[ordinal], clazz);
  }
  
  /**
   * Validate a identifier in the context.
   * A identifier is valid only if the context is IRowExpressionContext.
   * <ul>
   * <li>Resolve index in IRowMeta</li>
   * <li>Determine data type of a value in row.</li>
   * </ul>
   */
  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {
    if (context instanceof IRowExpressionContext) {
      this.context = (IRowExpressionContext) context;
      IRowMeta rowMeta = this.context.getRowMeta();
      this.ordinal  = rowMeta.indexOfValue(name);
      if (ordinal >= 0) {
        this.valueMeta = rowMeta.getValueMeta(ordinal);
        this.type = createDataType(valueMeta);
      }
    }

    if (valueMeta == null) {
      throw new ExpressionException(position, ErrorCode.UNRESOLVED_IDENTIFIER, name);
    }
  }
  
  protected Type createDataType(final IValueMeta meta) {
    switch (meta.getType()) {
      case IValueMeta.TYPE_BOOLEAN:
        return Types.BOOLEAN;
      case IValueMeta.TYPE_DATE:
      case IValueMeta.TYPE_TIMESTAMP:
        return Types.DATE;
      case IValueMeta.TYPE_STRING:
        return StringType.of(meta.getLength());
      case IValueMeta.TYPE_INTEGER:
        return IntegerType.of(meta.getLength());
      case IValueMeta.TYPE_NUMBER:
      case IValueMeta.TYPE_BIGNUMBER:
        return NumberType.of(meta.getLength(), meta.getPrecision());
      case ValueMetaJson.TYPE_JSON:
        return Types.JSON;
      case IValueMeta.TYPE_BINARY:
        return BinaryType.of(meta.getLength());
      default:
        throw new ExpressionException(position, ErrorCode.UNSUPPORTED_VALUEMETA, getName(),
            meta.getTypeDesc());
    }
  }
  
  @Override
  public <E> E accept(IExpressionVisitor<E> visitor) {
    return visitor.visitIdentifier(this);
  }

  @Override
  public void unparse(StringWriter writer) {
    // If identifier name contains space or is a reserved word or a function name
    if (name.indexOf(' ') >= 0 || ExpressionParser.isReservedWord(name)
        || FunctionRegistry.isFunction(name) || TypeId.of(name) != null
        || TimeUnit.of(name) != null) {
      writer.append('\"');
      writer.append(name);
      writer.append('\"');
    } else {
      writer.append(name);
    }
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, type, ordinal);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if (o == null)
      return false;
    if (getClass() != o.getClass())
      return false;

    Identifier other = (Identifier) o;
    return this.name.equals(other.name) && this.type.equals(other.type)
        && this.ordinal == other.ordinal;
  }

  @Override
  public Identifier asIdentifier() {
    return this;
  }

  @Override
  public String toString() {
    return this.name;
  }
}
