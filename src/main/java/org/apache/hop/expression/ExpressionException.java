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

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serial;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Exception raised when an expression cannot be parsed, compiled or evaluated.
 *
 * <p>An exception may be raised for each row with invalid data, and caught without being reported
 * by functions like {@code TRY_CAST}, {@code TRY_TO_DATE} or {@code IS_DATE}, which only check the
 * {@link ErrorCode}. The creation of the exception is therefore kept cheap:
 *
 * <ul>
 *   <li>The stack trace is not captured: the error is identified by its {@link ErrorCode} and its
 *       message, not by the Java call stack, and capturing the deep stack of the recursive
 *       evaluation would be a significant cost. A {@link ParseExpressionException} keeps its stack
 *       trace, as it is raised once when parsing, not for each row.
 *   <li>The localized message is built on first read by {@link #getMessage()}, not by the
 *       constructor: it requires a lookup in the resource bundle and the formatting of the values,
 *       which are useless when the exception is caught without being reported.
 * </ul>
 *
 * <p>The values of the message must be immutable, like a string, a number or a date, because the
 * message may be built later with their state at that time.
 */
@NullMarked
public class ExpressionException extends RuntimeException {
  @Serial private static final long serialVersionUID = 1L;

  private final ErrorCode error;

  /**
   * The values of the message, released once the message is built. Not serialized because a value
   * may not be serializable, the message is built before serialization.
   */
  private transient @Nullable Object @Nullable [] values;

  /** The localized message, built on first read. */
  private @Nullable String message;

  /**
   * Create a new expression exception.
   *
   * @param error an error message
   * @param values the immutable values that replace the placeholders of the message
   */
  public ExpressionException(ErrorCode error, @Nullable Object... values) {
    this(false, error, values);
  }

  /**
   * Create a new expression exception, with or without the stack trace.
   *
   * <p>For an exception raised once, like a parse error, the stack trace can be captured without
   * cost on the evaluation.
   *
   * @param writableStackTrace whether the stack trace is captured
   * @param error an error message
   * @param values the immutable values that replace the placeholders of the message
   */
  protected ExpressionException(
      boolean writableStackTrace, ErrorCode error, @Nullable Object... values) {
    // No message yet, suppression enabled
    super(null, null, true, writableStackTrace);
    this.error = error;
    this.values = values;
  }

  public ErrorCode getErrorCode() {
    return error;
  }

  /**
   * Returns the localized message, built on first read.
   *
   * <p>Not synchronized: concurrent first reads may build the message twice, which is harmless as
   * the result is always the same immutable string.
   *
   * @return the localized message
   */
  @Override
  public String getMessage() {
    String result = message;
    if (result == null) {
      result = error.message(values);
      message = result;
      values = null;
    }
    return result;
  }

  /** Builds the message before serialization, because the values are not serialized. */
  @Serial
  private void writeObject(ObjectOutputStream out) throws IOException {
    getMessage();
    out.defaultWriteObject();
  }
}
