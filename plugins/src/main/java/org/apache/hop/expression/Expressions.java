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
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeId;

public class Expressions {

  private Expressions() {
    // Utility class
  }
  
  /* package */ static IExpression compile(final IExpressionContext context, IExpression expression)
      throws ExpressionException {
    IExpression original;

    do {
      original = expression;
      expression = expression.compile(context);
    } while (!expression.equals(original));

    return expression;
  }

  public static IExpression build(final IExpressionContext context, String source)
      throws ExpressionException {
    
    ExpressionParser parser = new ExpressionParser(context.resolve(source));

    try {
      IExpression expression = parser.parse();
      expression.validate(context);
      expression = compile(context, expression);
      
      // Unknown are not expected here
      if (expression.getType().is(TypeId.UNKNOWN)) {
        throw new ExpressionException(0, ExpressionError.SYNTAX_ERROR_NEAR_KEYWORD, source);
      }
      
      return expression;
    } catch (ExpressionException e) {
      throw e;
    } catch (IllegalArgumentException e) {
      throw new ExpressionException(parser.getPosition(), ExpressionError.SYNTAX_ERROR, e.getMessage());
    }
  }

  public static IValueMeta createValueMeta(final String name, final TypeId type) {

    if (name == null) {
      throw new IllegalArgumentException("Name must not be null");
    }
    if (type == null) {
      throw new IllegalArgumentException("TypeName must not be null");
    }

    switch (type) {
      case BOOLEAN:
        return new ValueMetaBoolean(name);
      case INTEGER:
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

  public static IValueMeta createValueMeta(final String name, final Type type) {
    if (name == null) {
      throw new IllegalArgumentException("Name must not be null");
    }
    if (type == null) {
      throw new IllegalArgumentException("Type must not be null");
    }
    switch (type.getId()) {
      case BOOLEAN:
        return new ValueMetaBoolean(name);
      case INTEGER:
        return new ValueMetaInteger(name);
      case NUMBER:
        return new ValueMetaBigNumber(name, type.getPrecision(), type.getScale());
      case STRING:
        return new ValueMetaString(name, type.getPrecision(), type.getScale());
      case DATE:
        return new ValueMetaDate(name);
      case JSON:
        return new ValueMetaJson(name);
      case UNKNOWN:
      default:
        return new ValueMetaNone(name);
    }
  }
}

