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
import org.apache.hop.expression.DatePart;
import org.junit.Test;

public class DatePartTest extends BaseExpressionTest {
     
  @Test
  public void of() throws Exception {
    assertEquals(DatePart.MILLENNIUM, DatePart.of("MILLENNIUM"));
    assertEquals(DatePart.CENTURY, DatePart.of("CENTURY"));
    assertEquals(DatePart.DECADE, DatePart.of("DECADE"));
    assertEquals(DatePart.YEAR, DatePart.of("year"));
    assertEquals(DatePart.YEAR, DatePart.of("Y"));
    assertEquals(DatePart.QUARTER, DatePart.of("quarter"));
    assertEquals(DatePart.QUARTER, DatePart.of("q"));
    assertEquals(DatePart.MONTH, DatePart.of("Month"));
    assertEquals(DatePart.MONTH, DatePart.of("MM"));
    assertEquals(DatePart.DAY, DatePart.of("d"));
    assertEquals(DatePart.DAY, DatePart.of("dd"));
    assertEquals(DatePart.DAY, DatePart.of("dayofmonth"));
    assertEquals(DatePart.DAYOFWEEK, DatePart.of("DOW"));  
    assertEquals(DatePart.HOUR, DatePart.of("HOUR"));
    assertEquals(DatePart.HOUR, DatePart.of("HH"));
    assertNotEquals(DatePart.HOUR, DatePart.MINUTE);
    assertNotEquals(DatePart.of("HOUR"), null);
    assertThrows(IllegalArgumentException.class, () -> DatePart.of("NOP"));
  }
  
  @Test
  public void exist() throws Exception {
    assertTrue(DatePart.exist("MONTH"));
    assertTrue(DatePart.exist("MM"));
  } 
}