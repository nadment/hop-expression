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

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import org.junit.jupiter.api.Test;

class StringConversionTest {

  @Test
  void convertDate() {
    assertEquals(
        "2024-08-12",
        StringConversion.convert(ZonedDateTime.of(2024, 8, 12, 0, 0, 0, 0, ZoneOffset.UTC)));
    assertEquals(
        "2024-08-12 10:30:45",
        StringConversion.convert(ZonedDateTime.of(2024, 8, 12, 10, 30, 45, 0, ZoneOffset.UTC)));
    assertEquals(
        "2024-08-12 10:30:45.123000000",
        StringConversion.convert(
            ZonedDateTime.of(2024, 8, 12, 10, 30, 45, 123000000, ZoneOffset.UTC)));

    // Midnight with only a fraction of a second
    assertEquals(
        "2024-08-12 00:00:00.123456789",
        StringConversion.convert(
            ZonedDateTime.of(2024, 8, 12, 0, 0, 0, 123456789, ZoneOffset.UTC)));
  }
}
