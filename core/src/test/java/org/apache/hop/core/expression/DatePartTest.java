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
import org.apache.hop.expression.DatePart;
import org.junit.Test;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public class DatePartTest {
  @Test
  public void get() throws Exception {
    LocalDateTime local = LocalDateTime.of(2020, 8, 15, 23, 45, 59);
    ZonedDateTime datetime = ZonedDateTime.of(local, ZoneId.of("UTC"));

    assertEquals(2020, DatePart.YEAR.get(datetime));
    assertEquals(3, DatePart.QUARTER.get(datetime));
    assertEquals(8, DatePart.MONTH.get(datetime));
    assertEquals(33, DatePart.WEEKOFYEAR.get(datetime));
    assertEquals(15, DatePart.DAY.get(datetime));
    assertEquals(228, DatePart.DAYOFYEAR.get(datetime));
    assertEquals(23, DatePart.HOUR.get(datetime));
    assertEquals(45, DatePart.MINUTE.get(datetime));
    assertEquals(59, DatePart.SECOND.get(datetime));
    assertEquals(0, DatePart.MILLISECOND.get(datetime));
    assertEquals(0, DatePart.MICROSECOND.get(datetime));
    assertEquals(0, DatePart.NANOSECOND.get(datetime));
    assertEquals(3, DatePart.MILLENNIUM.get(datetime));
    assertEquals(21, DatePart.CENTURY.get(datetime));
    assertEquals(202, DatePart.DECADE.get(datetime));
    assertEquals(1597535159, DatePart.EPOCH.get(datetime));

    // TODO: More tests
    assertEquals(7, DatePart.DAYOFWEEK.get(datetime));
    assertEquals(6, DatePart.DAYOFWEEKISO.get(datetime));
    assertEquals(2020, DatePart.YEAROFWEEK.get(datetime));
    assertEquals(2020, DatePart.YEAROFWEEKISO.get(datetime));

  }
}

