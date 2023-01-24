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

import static java.time.temporal.ChronoField.DAY_OF_MONTH;
import static java.time.temporal.ChronoField.MONTH_OF_YEAR;
import java.time.Month;
import java.time.temporal.IsoFields;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalAdjuster;

public class FirstDayOfQuarter implements TemporalAdjuster {

  @Override
  public Temporal adjustInto(Temporal temporal) {

    int quarter = temporal.get(IsoFields.QUARTER_OF_YEAR);
    switch (quarter) {
      case 1:
        temporal = temporal.with(MONTH_OF_YEAR, Month.JANUARY.getValue());
        break;
      case 2:
        temporal = temporal.with(MONTH_OF_YEAR, Month.APRIL.getValue());
        break;
      case 3:
        temporal = temporal.with(MONTH_OF_YEAR, Month.JULY.getValue());
        break;
      case 4:
        temporal = temporal.with(MONTH_OF_YEAR, Month.OCTOBER.getValue());
        break;
    }

    return temporal.with(DAY_OF_MONTH, 1);
  }
}
