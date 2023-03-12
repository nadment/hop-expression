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

import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaJson;
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.DataTypeName;
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
public final class Identifier implements IExpression {
  private final String name;
  private final DataTypeName type;
  private final int ordinal;

  public Identifier(final String name, final DataTypeName type, int ordinal) {
    this.name = Objects.requireNonNull(name, "name is null");
    this.type = Objects.requireNonNull(type, "data type is null");
    this.ordinal = ordinal;
  }

  public Identifier(final String name) {
    this(name, DataTypeName.UNKNOWN, -1);
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
  public DataTypeName getType() {
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
  public Object getValue(final IExpressionContext context) throws ExpressionException {

    IRowMeta rowMeta = context.getRowMeta();

    Object[] row = context.getRow();   
    if ( row==null ) {
      throw new ExpressionException(ExpressionError.CONTEXT_ERROR);
    }
    
    try {
      IValueMeta valueMeta = rowMeta.getValueMeta(ordinal);

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
          return LocalDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
        case IValueMeta.TYPE_STRING:
          return rowMeta.getString(row, ordinal);
        case IValueMeta.TYPE_INTEGER:
          return rowMeta.getInteger(row, ordinal);
        case IValueMeta.TYPE_NUMBER:
          return rowMeta.getNumber(row, ordinal);
        case IValueMeta.TYPE_BIGNUMBER:
          return rowMeta.getBigNumber(row, ordinal);
        case ValueMetaJson.TYPE_JSON:
          return valueMeta.getNativeDataType(row[ordinal]);
        case IValueMeta.TYPE_BINARY:
          return rowMeta.getBinary(row, ordinal);
        default:
          throw new ExpressionException(ExpressionError.UNSUPPORTED_VALUEMETA, name,
              valueMeta.getTypeDesc());
      }
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.UNRESOLVED_IDENTIFIER, name);
    }
  }

  @Override
  public <T> T getValue(IExpressionContext context, Class<T> clazz) throws ExpressionException {

    IRowMeta rowMeta = context.getRowMeta();
    IValueMeta valueMeta = rowMeta.getValueMeta(ordinal);

    if (valueMeta == null) {
      throw new ExpressionException(ExpressionError.UNRESOLVED_IDENTIFIER, name);
    }

    Object[] row = context.getRow();
    if ( row==null ) {
      throw new ExpressionException(ExpressionError.CONTEXT_ERROR);
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
//          if (clazz == Long.class) {
//            return clazz.cast(value ? 1L : 0L);
//          }
//          if (clazz == Double.class) {
//            return clazz.cast(value ? 1D : 0D);
//          }
//          if (clazz == BigDecimal.class) {
//            return clazz.cast(value ? BigDecimal.ONE : BigDecimal.ZERO);
//          }
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
            return clazz.cast(Converter.parseBoolean(value));
          }
          if (clazz == Long.class) {
            return clazz.cast(Converter.parseInteger(value));
          }
          if (clazz == Double.class) {
            return clazz.cast(Converter.parseNumber(value));
          }
          if (clazz == BigDecimal.class) {
            return clazz.cast(Converter.parseBigNumber(value));
          }
          if (clazz == byte[].class) {
            return clazz.cast(value.getBytes(StandardCharsets.UTF_8));
          }
          if (clazz == JsonNode.class) {
            return clazz.cast(Converter.parseJson(value));
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
          if (clazz == Double.class) {
            return clazz.cast(Double.valueOf(value));
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
          if (clazz == Double.class) {
            return clazz.cast(value);
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
          if (clazz == Double.class) {
            return clazz.cast(value.doubleValue());
          }
          if (clazz == Boolean.class) {
            return clazz.cast(value.unscaledValue()!=BigInteger.ZERO);
          }
          if (clazz == String.class) {
            return clazz.cast(NumberFormat.of("TM").format(value));
          }
          break;          
        }

        case ValueMetaJson.TYPE_JSON: {
          Object value = context.getRow()[ordinal];
          if (clazz.isInstance(value)) {
            return clazz.cast(value);
          }
          if (clazz == String.class) {
            return clazz.cast(Converter.toString((JsonNode) value));
          }
          break;
        }

        case IValueMeta.TYPE_BINARY: {
          byte[] value = rowMeta.getBinary(row, ordinal);
          if (clazz == byte[].class) {
            return clazz.cast(value);
          }
          if (clazz == String.class) {
            return clazz.cast(new String(value, StandardCharsets.UTF_8));
          }
          break;
        }

        default:
          throw new ExpressionException(ExpressionError.UNSUPPORTED_VALUEMETA, name,
              valueMeta.getTypeDesc());
      }
    } catch (ClassCastException ce) {
      throw new ExpressionException(ExpressionError.CONVERSION_ERROR, valueMeta, name, clazz);
    } catch (Exception e) {
      // Ignore
    }

    throw new ExpressionException(ExpressionError.UNRESOLVED_IDENTIFIER, name);
  }

  @Override
  public <E> E accept(IExpressionContext context, IExpressionVisitor<E> visitor) {
    return visitor.apply(context, this);
  }

  @Override
  public void unparse(StringWriter writer) {
    // If identifier name contains space or is a reserved word or a function name
    if (name.indexOf(' ') >= 0 || ExpressionBuilder.isReservedWord(name)
        || FunctionRegistry.isFunction(name) || DataTypeName.exist(name) || TimeUnit.exist(name)) {
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

    return name.equals(((Identifier) o).name);
  }

  @Override
  public String toString() {
    return this.name;
  }
}
