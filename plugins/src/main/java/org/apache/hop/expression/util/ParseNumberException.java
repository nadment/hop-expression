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
package org.apache.hop.expression.util;

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;

public class ParseNumberException extends ExpressionException {

  private static final long serialVersionUID = 8634955627375465878L;

  /**
   * Construct a new expression exception.
   *
   * @param error a error message
   */
  public ParseNumberException(ErrorCode error, Object... values) {
    super(error.message(values));
  }
}
