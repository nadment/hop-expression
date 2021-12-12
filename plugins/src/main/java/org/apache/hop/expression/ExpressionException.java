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
package org.apache.hop.expression;

import org.apache.hop.i18n.BaseMessages;

public class ExpressionException extends RuntimeException {
 
  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  private static final long serialVersionUID = 8634955627375465878L;

  public static ExpressionException create(String message, Object... values) {  
    return new ExpressionException(BaseMessages.getString(PKG, message, values));
  }  
  
  public static ExpressionException createUnexpectedDataType(String name, DataType type) {
    return create("Expression.UnexpectedDataType", name, type);
  }  
  
  public static final ExpressionException createArgumentOutOfRange(Object arg) {
    return create("Expression.ArgumentOutOfRange", arg);
  }
  
  public static ExpressionException createUnexpectedDatePart(String name, DatePart part) {
    return create("Expression.UnexpectedDatePart", name, part);
  }
  
  public static final ExpressionException createDivisionByZero() {
    return create("Expression.DivisionByZero");
  }
  
  public static final ExpressionException createOverflow(String message) {
    return create("Expression.Overflow", message);
  }

  public static final ExpressionException createUnsupportedConversion(Object value, DataType type) {
    return create("Expression.UnsupportedConversion", value,
        DataType.fromJava(value), type);
  }

  public static ExpressionException createFormatPattern(String s, int i) {
    return create("Bad format {0} at position {1}", s, i);
  }

  public static ExpressionException createRegexpPattern(String s) {
    return create("Bad regexp {0}", s);
  }
  
  /**
   * Construct a new expression exception.
   * 
   * @param message a descriptive message
   */
  public ExpressionException(String message) {
    super(message);
  }

  /**
   * Construct a new expression exception.
   * 
   * @param message a descriptive message
   * @param cause the underlying cause of this exception
   */
  public ExpressionException(String message, Throwable exception) {
    super(message, exception);
  }


}
