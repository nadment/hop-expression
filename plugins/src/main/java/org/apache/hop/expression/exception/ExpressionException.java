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
package org.apache.hop.expression.exception;

import org.apache.hop.expression.ErrorCode;

public class ExpressionException extends RuntimeException {

  private static final long serialVersionUID = 1L;
  
  /**
   * The zero-based character offset into the string being parsed at which
   * the error was found during parsing.
   */
  private final int position;
  
  /**
   * Create a new expression exception.
   * 
   * @param error a error message
   */
  public ExpressionException(String error) {
    super(error);
    this.position = 0;
  }

  /**
   * Construct a new expression exception.
   * 
   * @param error a error message
   */
  public ExpressionException(int position, ErrorCode error) {
    super(error.message());
    this.position = position;
  }
  
  /**
   * Create a new expression exception.
   * 
   * @param error a error message
   */
  public ExpressionException(ErrorCode error) {
    this(0, error);
  }

  public ExpressionException(ErrorCode error, Object... values) {
    this(0, error, values);
  }
  
  public ExpressionException(int position, ErrorCode error, Object... values) {
    super(error.message(values));
    this.position = position;
  }
  
  /**
   * Returns the position where the error was found.
   *
   * @return the position where the error was found
   */
  public int getPosition() {
      return position;
  }
}
