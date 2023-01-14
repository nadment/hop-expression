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
import org.apache.hop.expression.type.DataTypeName;
import org.apache.hop.expression.util.TimeUnit;
import java.io.StringWriter;
import java.time.ZoneId;
import java.util.Date;
import java.util.Objects;

/**
 * Expression representing a named column in an input row.
 */
public final class Identifier implements IExpression {
  private final String name;
  private final DataTypeName type;
  private final int index;

  public Identifier(final String name, final DataTypeName type, int index) {
    this.name = Objects.requireNonNull(name, "name is null");
    this.type = Objects.requireNonNull(type, "data type is null");
    this.index = index;
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
    return index;
  }

  @Override
  public Object getValue(final IExpressionContext context) throws ExpressionException {

    IRowMeta rowMeta = context.getRowMeta();

    Object[] row = context.getRow();
    try {
      IValueMeta valueMeta = rowMeta.getValueMeta(index);

      switch (valueMeta.getType()) {
        case IValueMeta.TYPE_BOOLEAN:
          return rowMeta.getBoolean(row, index);
        case IValueMeta.TYPE_DATE:
        case IValueMeta.TYPE_TIMESTAMP:
          // No getTimestamp from RowMeta ???
          Date date = rowMeta.getDate(row, index);
          if (date == null)
            return null;
          return date.toInstant().atZone(ZoneId.systemDefault());
        case IValueMeta.TYPE_STRING:
          return rowMeta.getString(row, index);
        case IValueMeta.TYPE_INTEGER:
          return rowMeta.getInteger(row, index);
        case IValueMeta.TYPE_NUMBER:
          return rowMeta.getNumber(row, index);
        case IValueMeta.TYPE_BIGNUMBER:
          return rowMeta.getBigNumber(row, index);
        case ValueMetaJson.TYPE_JSON:
          return context.getRow()[index];
        case IValueMeta.TYPE_BINARY:
          return rowMeta.getBinary(row, index);
        default:
          throw new ExpressionException(ExpressionError.UNSUPPORTED_VALUEMETA, name,
              valueMeta.getTypeDesc());
      }
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.UNRESOLVED_IDENTIFIER, name);
    }
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
    return Objects.hash(name, type, index);
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
