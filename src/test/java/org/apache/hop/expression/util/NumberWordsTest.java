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

package org.apache.hop.expression.util;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class NumberWordsTest {

  @Test
  void convertCardinal() {
    assertEquals("zero", NumberWords.convertCardinal(0));
    assertEquals("forty-two", NumberWords.convertCardinal(42));
    assertEquals("three hundred", NumberWords.convertCardinal(300));
    assertEquals("one million", NumberWords.convertCardinal(1000000));
    assertEquals(
        "one million two hundred thirty-four thousand five hundred sixty-seven",
        NumberWords.convertCardinal(1234567));
  }

  @Test
  void convertYear() {
    assertEquals("nineteen hundred", NumberWords.convertYear(1900));
    assertEquals("twenty twenty-four", NumberWords.convertYear(2024));
    assertEquals("twelve thousand", NumberWords.convertYear(12000));
  }
}
