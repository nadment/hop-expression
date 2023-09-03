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
import static org.apache.hop.expression.Expressions.createDataType;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaJson;
import org.apache.hop.expression.exception.ConversionException;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.BooleanType;
import org.apache.hop.expression.type.IntegerType;
import org.apache.hop.expression.type.JsonType;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.UnknownType;
import org.apache.hop.expression.util.NumberFormat;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Date;
import java.util.Objects;
import com.fasterxml.jackson.databind.JsonNode;

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

  public Identifier(final String name) {
    this(0, name);
  }
  
  public Identifier(int position, final String name) {
    this.name = requireNonNull(name, "name");
    this.type = UnknownType.UNKNOWN;
    this.position = position;
    this.context = null;
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
    IRowMeta rowMeta = context.getRowMeta();
    Object[] row = context.getRow();
    if (row == null) {
      throw new ExpressionException(position, ExpressionError.CONTEXT_ERROR);
    }
    
    IValueMeta valueMeta = rowMeta.getValueMeta(ordinal);
    if (valueMeta == null) {
      throw new ExpressionException(position, ExpressionError.UNRESOLVED_IDENTIFIER, name);
    }
    
    try {
      switch (valueMeta.getType()) {
        case IValueMeta.TYPE_BOOLEAN:
          return rowMeta.getBoolean(row, ordinal);
        case IValueMeta.TYPE_TIMESTAMP:
          Timestamp timestamp = (Timestamp) valueMeta.getNativeDataType(row[ordinal]);
          if (timestamp == null)
            return null;
          return timestamp.toLocalDateTime().atZone(ZoneId.systemDefault());
        case IValueMeta.TYPE_DATE:
          Date date = rowMeta.getDate(row, ordinal);
          if (date == null)
            return null;
          return ZonedDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
        case IValueMeta.TYPE_STRING:
          return rowMeta.getString(row, ordinal);
        case IValueMeta.TYPE_INTEGER:
          return rowMeta.getInteger(row, ordinal);
        case IValueMeta.TYPE_NUMBER:
        case IValueMeta.TYPE_BIGNUMBER:
          return rowMeta.getBigNumber(row, ordinal);
        case ValueMetaJson.TYPE_JSON:
          return valueMeta.getNativeDataType(row[ordinal]);
        case IValueMeta.TYPE_BINARY:
          return rowMeta.getBinary(row, ordinal);
        default:
          throw new ExpressionException(position, ExpressionError.UNSUPPORTED_VALUEMETA, getName(),
              valueMeta.getTypeDesc());
      }
    } catch (Exception e) {
      // Ignore
    }
    throw new ExpressionException(position, ExpressionError.CONVERSION_ERROR, valueMeta.getTypeDesc().toUpperCase(), row[ordinal]);
  }

  @Override
  public <T> T getValue(Class<T> clazz) {
    IRowMeta rowMeta = context.getRowMeta();
    Object[] row = context.getRow();
    if (row == null) {
      throw new ExpressionException(position, ExpressionError.CONTEXT_ERROR);
    }

    IValueMeta valueMeta = rowMeta.getValueMeta(ordinal);
    if (valueMeta == null) {
      throw new ExpressionException(position, ExpressionError.UNRESOLVED_IDENTIFIER, name);
    }

    try {

      switch (valueMeta.getType()) {
        case IValueMeta.TYPE_BOOLEAN: {
          Boolean value = rowMeta.getBoolean(row, ordinal);
          if (value == null) {
            return null;
          }
          if (clazz == Boolean.class) {
            return clazz.cast(value);
          }
          if (clazz == String.class) {
            return clazz.cast(value ? "TRUE" : "FALSE");
          }
          // if (clazz == Long.class) {
          // return clazz.cast(value ? 1L : 0L);
          // }
          // if (clazz == BigDecimal.class) {
          // return clazz.cast(value ? BigDecimal.ONE : BigDecimal.ZERO);
          // }
          break;
        }

        case IValueMeta.TYPE_TIMESTAMP: {
          Date value = rowMeta.getDate(row, ordinal);
          if (value == null) {
            return null;
          }
          if (clazz == LocalDateTime.class) {
            return clazz.cast(LocalDateTime.ofInstant(value.toInstant(), ZoneId.systemDefault()));
          }

          return clazz.cast(value.toInstant().atZone(ZoneId.systemDefault()));
        }

        case IValueMeta.TYPE_DATE: {
          Date value = rowMeta.getDate(row, ordinal);
          if (value == null) {
            return null;
          }
          if (clazz == ZonedDateTime.class) {
            return clazz.cast(value.toInstant().atZone(ZoneId.systemDefault()));
          }

          return clazz.cast(LocalDateTime.ofInstant(value.toInstant(), ZoneId.systemDefault()));
        }

        case IValueMeta.TYPE_STRING: {
          String value = rowMeta.getString(row, ordinal);
          if (value == null) {
            return null;
          }
          if (clazz == String.class) {
            return clazz.cast(value);
          }
          if (clazz == Boolean.class) {
            return clazz.cast(BooleanType.convert(value));
          }
          if (clazz == Long.class) {
            return clazz.cast(IntegerType.convert(value));
          }
          if (clazz == BigDecimal.class) {
            return clazz.cast(NumberType.convert(value));
          }
          if (clazz == byte[].class) {
            return clazz.cast(BinaryType.convert(value));
          }
          if (clazz == JsonNode.class) {
            return clazz.cast(JsonType.convert(value));
          }
          break;
        }

        case IValueMeta.TYPE_INTEGER: {
          Long value = rowMeta.getInteger(row, ordinal);
          if (value == null) {
            return null;
          }
          if (clazz == Long.class) {
            return clazz.cast(value);
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

        case IValueMeta.TYPE_NUMBER: {
          Double value = rowMeta.getNumber(row, ordinal);
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
          BigDecimal value = rowMeta.getBigNumber(row, ordinal);
          if (value == null) {
            return null;
          }
          if (clazz == BigDecimal.class) {
            return clazz.cast(value);
          }
          if (clazz == Long.class) {
            return clazz.cast(value.longValue());
          }
          if (clazz == Boolean.class) {
            return clazz.cast(value.unscaledValue() != BigInteger.ZERO);
          }
          if (clazz == String.class) {
            return clazz.cast(NumberFormat.of("TM").format(value));
          }
          break;
        }

        case ValueMetaJson.TYPE_JSON: {
          Object value = row[ordinal];
          if (value == null) {
            return null;
          }
          if (clazz.isInstance(value)) {
            return clazz.cast(value);
          }
          if (clazz == String.class) {
            return clazz.cast(StringType.convert((JsonNode) value));
          }
          break;
        }

        case IValueMeta.TYPE_BINARY: {
          byte[] value = rowMeta.getBinary(row, ordinal);
          if (value == null) {
            return null;
          }
          if (clazz == byte[].class) {
            return clazz.cast(value);
          }
          if (clazz == String.class) {
            return clazz.cast(new String(value, StandardCharsets.UTF_8));
          }
          break;
        }

        default:
          throw new ExpressionException(position, ExpressionError.UNSUPPORTED_VALUEMETA, getName(),
              valueMeta.getTypeDesc());

      }
    } catch (ConversionException e) {
      throw e;
    } catch (Exception e) {
      // Ignore
    }
    throw new ExpressionException(position, ExpressionError.CONVERSION_ERROR, valueMeta.getTypeDesc().toUpperCase(), row[ordinal], clazz);
  }
  
  /**
   * Validate a identifier in the context.
   * 
   * <ul>
   * <li>Resolve index in IRowMeta</li>
   * <li>Determine data type of a value in row.</li>
   * </ul>
   */
  @Override
  public void validate(final IExpressionContext context) throws ExpressionException {
    compile(context);
  }

  /**
   * Compile a identifier.
   * 
   * <ul>
   * <li>Resolve index in IRowMeta</li>
   * <li>Determine data type of a value in row.</li>
   * </ul>
   */
  @Override
  public IExpression compile(final IExpressionContext ctx) throws ExpressionException {

    
    if (ctx instanceof IRowExpressionContext) {
      this.context = (IRowExpressionContext) ctx;
      IRowMeta rowMeta = this.context.getRowMeta();

      this.ordinal  = rowMeta.indexOfValue(name);
      if (ordinal < 0) {
        throw new ExpressionException(position, ExpressionError.UNRESOLVED_IDENTIFIER, name);
      }
      this.type = createDataType(rowMeta.getValueMeta(ordinal));
      
      return this;       
    }
    
    throw new ExpressionException(position, ExpressionError.CONTEXT_ERROR);
  }

  @Override
  public <E> E accept(IExpressionContext context, IExpressionVisitor<E> visitor) {
    return visitor.apply(context, this);
  }

  @Override
  public void unparse(StringWriter writer) {
    // If identifier name contains space or is a reserved word or a function name
    if (name.indexOf(' ') >= 0 || ExpressionParser.isReservedWord(name)
        || FunctionRegistry.isFunction(name) || TypeName.of(name) != null
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
