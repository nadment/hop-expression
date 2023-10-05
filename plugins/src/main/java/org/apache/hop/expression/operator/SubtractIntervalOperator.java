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
package org.apache.hop.expression.operator;

import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Interval;
import java.time.ZonedDateTime;

/**
 * Subtracts a specified interval to a date or timestamp
 */
public class SubtractIntervalOperator extends SubtractOperator {
  public static final SubtractIntervalOperator INSTANCE = new SubtractIntervalOperator();
  
  public SubtractIntervalOperator() {
    super();
  }

  @Override
  public Object eval(final IExpression[] operands) {
    ZonedDateTime datetime = operands[0].getValue(ZonedDateTime.class);
    if (datetime == null)
      return null;

    Interval interval = operands[1].getValue(Interval.class);
    if (interval == null)
      return null;

    return interval.subtractFrom(datetime);
  }
}