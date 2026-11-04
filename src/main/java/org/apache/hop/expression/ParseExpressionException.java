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

import java.io.Serial;
import lombok.Getter;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

@Getter
@NullMarked
public class ParseExpressionException extends ExpressionException {
  @Serial private static final long serialVersionUID = 1L;

  /**
   * The zero-based character offset into the string being parsed at which the error was found
   * during parsing.
   */
  private final int position;

  /**
   * Construct a new expression exception.
   *
   * @param error an error message
   */
  public ParseExpressionException(int position, ErrorCode error, @Nullable Object... values) {
    // Keep the stack trace, a parse error is raised once, not for each row
    super(true, error, values);

    this.position = position;
  }
}
