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

package org.apache.hop.core.expression;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.hop.expression.TimeUnit;
import org.junit.Test;

public class TimeUnitTest extends ExpressionTest {
     
  @Test
  public void of() throws Exception {
    assertEquals(TimeUnit.MILLENNIUM, TimeUnit.of("MiLLENNIUM"));
    assertEquals(TimeUnit.CENTURY, TimeUnit.of("CeNTuRy"));
    assertEquals(TimeUnit.DECADE, TimeUnit.of("DeCaDE"));
    assertEquals(TimeUnit.YEAR, TimeUnit.of("year"));
    assertEquals(TimeUnit.QUARTER, TimeUnit.of("quarter"));
    assertEquals(TimeUnit.MONTH, TimeUnit.of("Month"));
    assertEquals(TimeUnit.DAY, TimeUnit.of("day"));
    assertEquals(TimeUnit.DAY, TimeUnit.of("dayofmonth"));
    assertEquals(TimeUnit.DAYOFWEEK, TimeUnit.of("DayOfWeek"));  
    assertEquals(TimeUnit.HOUR, TimeUnit.of("HOUR"));
    assertEquals(TimeUnit.ISOWEEK, TimeUnit.of("IsoWeek"));
    assertEquals(TimeUnit.ISOWEEK, TimeUnit.of("IsoWeekOFYEAR"));    
    assertEquals(TimeUnit.ISODAYOFWEEK, TimeUnit.of("IsoDayOfWeek"));
    assertEquals(TimeUnit.ISOYEAR, TimeUnit.of("IsoYEAR"));
    assertNotEquals(TimeUnit.HOUR, TimeUnit.MINUTE);
    assertNotEquals(TimeUnit.of("HOUR"), null);
    assertThrows(IllegalArgumentException.class, () -> TimeUnit.of("NOP"));
  }
  
  @Test
  public void exist() throws Exception {
    assertTrue(TimeUnit.exist("MONTH"));
    assertTrue(TimeUnit.exist("DAYOFYEAR"));
  } 
}
