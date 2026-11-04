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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import org.junit.jupiter.api.Test;

class ExpressionExceptionTest {

  @Test
  void stackTraceNotCaptured() {
    ExpressionException exception = new ExpressionException(ErrorCode.DIVISION_BY_ZERO);
    assertEquals(ErrorCode.DIVISION_BY_ZERO, exception.getErrorCode());
    assertEquals(0, exception.getStackTrace().length);
  }

  @Test
  void stackTraceCapturedForParseError() {
    ParseExpressionException exception =
        new ParseExpressionException(3, ErrorCode.SYNTAX_ERROR, "x");
    assertEquals(ErrorCode.SYNTAX_ERROR, exception.getErrorCode());
    assertTrue(exception.getStackTrace().length > 0);
    // The message is also built on read
    assertEquals(ErrorCode.SYNTAX_ERROR.message("x"), exception.getMessage());
  }

  @Test
  void messageBuiltOnRead() {
    ExpressionException exception = new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 2, -1);
    String expected = ErrorCode.ARGUMENT_OUT_OF_RANGE.message(2, -1);
    assertEquals(expected, exception.getMessage());
    // The same message on next reads, once the values are released
    assertEquals(expected, exception.getMessage());
    assertEquals(expected, exception.getLocalizedMessage());
    assertEquals(ExpressionException.class.getName() + ": " + expected, exception.toString());
  }

  @Test
  void serialization() throws Exception {
    ExpressionException exception = new ExpressionException(ErrorCode.ARGUMENT_OUT_OF_RANGE, 2, -1);

    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    try (ObjectOutputStream out = new ObjectOutputStream(bytes)) {
      out.writeObject(exception);
    }
    ExpressionException result;
    try (ObjectInputStream in =
        new ObjectInputStream(new ByteArrayInputStream(bytes.toByteArray()))) {
      result = (ExpressionException) in.readObject();
    }

    // The message is built before serialization, the values are not serialized
    assertEquals(ErrorCode.ARGUMENT_OUT_OF_RANGE, result.getErrorCode());
    assertEquals(ErrorCode.ARGUMENT_OUT_OF_RANGE.message(2, -1), result.getMessage());
  }
}
