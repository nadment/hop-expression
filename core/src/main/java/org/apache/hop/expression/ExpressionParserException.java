/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.expression;

import org.apache.hop.i18n.BaseMessages;

public class ExpressionParserException extends ExpressionException {

  private static final Class<?> PKG = IExpression.class; // for i18n purposes

  /** */
  private static final long serialVersionUID = 8634955627375465878L;

  private final String source;
  private final int position;

  public ExpressionParserException(String message, String source, int position) {
    super(message);
    this.source = source;
    this.position = position;
  }

  public String getSource() {
    return source;
  }

  /**
   * Returns the position where the error was found.
   *
   * @return the position where the error was found
   */
  public int getPosition() {
    return position;
  }

  public String toString() {

    String message = this.getMessage();
    message = (message != null) ? ": " + message : "";

    return BaseMessages.getString(PKG, "ExpressionParser.SyntaxError", position, message);
  }
}
