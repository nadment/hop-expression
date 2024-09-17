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
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

public class TimeUnitTest extends ExpressionTest {

  @Test
  void of() throws Exception {
    assertEquals(TimeUnit.MILLENNIUM, TimeUnit.of("MiLLENNIUM"));
    assertEquals(TimeUnit.MILLENNIUM, TimeUnit.of("MiLLENNIUMs"));
    assertEquals(TimeUnit.CENTURY, TimeUnit.of("CeNTuRy"));
    assertEquals(TimeUnit.DECADE, TimeUnit.of("DeCaDE"));
    assertEquals(TimeUnit.DECADE, TimeUnit.of("DECADES"));
    assertEquals(TimeUnit.YEAR, TimeUnit.of("year"));
    assertEquals(TimeUnit.YEAR, TimeUnit.of("yearS"));
    assertEquals(TimeUnit.QUARTER, TimeUnit.of("quarter"));
    assertEquals(TimeUnit.QUARTER, TimeUnit.of("quarters"));
    assertEquals(TimeUnit.MONTH, TimeUnit.of("Month"));
    assertEquals(TimeUnit.MONTH, TimeUnit.of("MonthS"));
    assertEquals(TimeUnit.DAY, TimeUnit.of("day"));
    assertEquals(TimeUnit.DAY, TimeUnit.of("dayS"));
    assertEquals(TimeUnit.DAY, TimeUnit.of("dayofmonth"));
    assertEquals(TimeUnit.DAYOFWEEK, TimeUnit.of("DayOfWeek"));
    assertEquals(TimeUnit.HOUR, TimeUnit.of("HOUR"));
    assertEquals(TimeUnit.HOUR, TimeUnit.of("hours"));
    assertEquals(TimeUnit.MINUTE, TimeUnit.of("minute"));
    assertEquals(TimeUnit.MINUTE, TimeUnit.of("minuteS"));
    assertEquals(TimeUnit.SECOND, TimeUnit.of("SECOND"));
    assertEquals(TimeUnit.SECOND, TimeUnit.of("secondS"));
    assertEquals(TimeUnit.MICROSECOND, TimeUnit.of("microsecond"));
    assertEquals(TimeUnit.MICROSECOND, TimeUnit.of("microsecondS"));
    assertEquals(TimeUnit.MILLISECOND, TimeUnit.of("millisecond"));
    assertEquals(TimeUnit.MILLISECOND, TimeUnit.of("millisecondS"));
    assertEquals(TimeUnit.NANOSECOND, TimeUnit.of("nanosecond"));
    assertEquals(TimeUnit.NANOSECOND, TimeUnit.of("nanosecondS"));
    assertEquals(TimeUnit.ISOWEEK, TimeUnit.of("IsoWeek"));
    assertEquals(TimeUnit.ISOWEEK, TimeUnit.of("IsoWeekOFYEAR"));
    assertEquals(TimeUnit.ISODAYOFWEEK, TimeUnit.of("IsoDayOfWeek"));
    assertEquals(TimeUnit.ISOYEAR, TimeUnit.of("IsoYEAR"));

    assertNotEquals(TimeUnit.HOUR, TimeUnit.MINUTE);
    assertNotEquals(TimeUnit.of("HOUR"), null);
    assertNull(TimeUnit.of("NOP"));
  }
}
