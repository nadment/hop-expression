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

import org.apache.hop.expression.util.Pair;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Enumeration of time units ranges.
 */
public enum TimeUnitRange {
  YEAR(TimeUnit.YEAR, null),
  YEAR_TO_MONTH(TimeUnit.YEAR, TimeUnit.MONTH),
  MONTH(TimeUnit.MONTH, null),
  DAY(TimeUnit.DAY, null),
  DAY_TO_HOUR(TimeUnit.DAY, TimeUnit.HOUR),
  DAY_TO_MINUTE(TimeUnit.DAY, TimeUnit.MINUTE),
  DAY_TO_SECOND(TimeUnit.DAY, TimeUnit.SECOND),
  HOUR(TimeUnit.HOUR, null),
  HOUR_TO_MINUTE(TimeUnit.HOUR, TimeUnit.MINUTE),
  HOUR_TO_SECOND(TimeUnit.HOUR, TimeUnit.SECOND),
  MINUTE(TimeUnit.MINUTE, null),
  MINUTE_TO_SECOND(TimeUnit.MINUTE, TimeUnit.SECOND),
  SECOND(TimeUnit.SECOND, null);
  
  public final TimeUnit startUnit;
  public final TimeUnit endUnit;
  
  private static final Map<Pair<TimeUnit, TimeUnit>, TimeUnitRange> MAP = createMap();
  
  /**
   * Creates a TimeUnitRange.
   *
   * @param startUnit Start time unit
   * @param endUnit   End time unit
   */
  private TimeUnitRange(TimeUnit startUnit, TimeUnit endUnit) {
    assert startUnit != null;
    this.startUnit = startUnit;
    this.endUnit = endUnit;
  }

  /**
   * Returns a {@link TimeUnitRange} with a given name (ignore case).
   * 
   * @param name The name of the time unit
   * @return Time unit range, or null if not valid
   */
  public static TimeUnitRange of(final String name) {
     if ( name==null ) 
       return null;
     
     return TimeUnitRange.valueOf(name.toUpperCase());
  }
  
  /**
   * Returns a {@link TimeUnitRange} with a given start and end unit.
   *
   * @param startUnit Start unit
   * @param endUnit   End unit
   * @return Time unit range, or null if not valid
   */
  public static TimeUnitRange of(TimeUnit startUnit, TimeUnit endUnit) {
    return MAP.get(new Pair<>(startUnit, endUnit));
  }
  
  private static Map<Pair<TimeUnit, TimeUnit>, TimeUnitRange> createMap() {
    Map<Pair<TimeUnit, TimeUnit>, TimeUnitRange> map = new HashMap<>();
    for (TimeUnitRange value : values()) {
      map.put(new Pair<>(value.startUnit, value.endUnit), value);
    }
    return Collections.unmodifiableMap(map);
  }
  
}
