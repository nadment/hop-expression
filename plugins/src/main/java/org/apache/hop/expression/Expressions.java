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

import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaBigNumber;
import org.apache.hop.core.row.value.ValueMetaBinary;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaJson;
import org.apache.hop.core.row.value.ValueMetaNone;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.expression.type.BinaryDataType;
import org.apache.hop.expression.type.BooleanDataType;
import org.apache.hop.expression.type.DataName;
import org.apache.hop.expression.type.DataType;
import org.apache.hop.expression.type.DateDataType;
import org.apache.hop.expression.type.IntegerDataType;
import org.apache.hop.expression.type.JsonDataType;
import org.apache.hop.expression.type.NumberDataType;
import org.apache.hop.expression.type.StringDataType;
import org.apache.hop.expression.type.UnknownDataType;
import java.text.ParseException;

public class Expressions {
  
  /* package */ static IExpression compile(IExpressionContext context, IExpression expression)
      throws ExpressionException {
    IExpression original;

      do {
        original = expression;
        expression = expression.compile(context);
      } while (!expression.equals(original));

      return expression;
  }

  public static IExpression build(IExpressionContext context, final String source)
      throws ExpressionException {
    ExpressionParser parser = new ExpressionParser(source);
    try {
      IExpression expression = parser.parse();

      if (expression == null)
        return expression;

      expression.validate(context);
      return compile(context, expression);
    } catch (ParseException e) {
      throw createException(source, e.getErrorOffset(), e);
    } catch (IllegalArgumentException e) {
      throw createException(source, parser.getPosition(), e);
    }
  }
  
  protected static ExpressionException createException(String source, int offset, Exception e) {
    int line = 1;
    int column = 1;
    for (int index = 0; index < offset; index++) {
      char c = source.charAt(index);
      if (c == '\n' || c == '\r') {
        line++;
        column = 1;
      } else
        column++;
    }
    return new ExpressionException(ExpressionError.SYNTAX_ERROR, offset + 1, line, column,
        e.getMessage());
  }

  private Expressions() {
    // Utility class
  }

  
  public static DataType createDataType(IValueMeta valueMeta) {
    switch (valueMeta.getType()) {
      case IValueMeta.TYPE_BOOLEAN:
        return BooleanDataType.BOOLEAN;
      case IValueMeta.TYPE_DATE:
      case IValueMeta.TYPE_TIMESTAMP:
        return DateDataType.DATE;
      case IValueMeta.TYPE_STRING:
        return new StringDataType(valueMeta.getLength());
      case IValueMeta.TYPE_INTEGER:
        return IntegerDataType.INTEGER;
      case IValueMeta.TYPE_NUMBER:
      case IValueMeta.TYPE_BIGNUMBER:        
        return new NumberDataType(valueMeta.getLength(), valueMeta.getPrecision());     
      case ValueMetaJson.TYPE_JSON:
        return JsonDataType.JSON;
      case IValueMeta.TYPE_BINARY:
        return new BinaryDataType(valueMeta.getLength());
      default:
        return UnknownDataType.UNKNOWN;
    }
  }

  public static IValueMeta createValueMeta(final String name, final DataName type) {

    if (name == null) {
      throw new IllegalArgumentException("Name must not be null");
    }
    if (type == null) {
      throw new IllegalArgumentException("DataName must not be null");
    }

    switch (type) {
      case BOOLEAN:
        return new ValueMetaBoolean(name);
      case INTEGER: // Max 2.147.483.647
        return new ValueMetaInteger(name, 9, 0);
      case NUMBER:
        return new ValueMetaBigNumber(name, -1, -1);
      case STRING:
        return new ValueMetaString(name, -1, -1);
      case DATE:
        return new ValueMetaDate(name, -1, -1);
      case BINARY:
        return new ValueMetaBinary(name, -1, -1);
      case JSON:
        return new ValueMetaJson(name);
      case UNKNOWN:
      default:
        return new ValueMetaNone(name);
    }
  }

  public static IValueMeta createValueMeta(final String name, final DataType type) {
    if (name == null) {
      throw new IllegalArgumentException("Name must not be null");
    }
    if (type == null) {
      throw new IllegalArgumentException("DataType must not be null");
    }
    switch (type.getName()) {
      case BOOLEAN:
        return new ValueMetaBoolean(name);
      case INTEGER:
        return new ValueMetaInteger(name);
      case NUMBER:
        return new ValueMetaBigNumber(name, type.getPrecision(), type.getScale());
      case STRING:
        return new ValueMetaString(name, type.getPrecision(), type.getScale());
      case DATE:
        return new ValueMetaDate(name, -1, -1);
      case JSON:
        return new ValueMetaJson(name);
      case UNKNOWN:
      default:
        return new ValueMetaNone(name);
    }
  }
}

