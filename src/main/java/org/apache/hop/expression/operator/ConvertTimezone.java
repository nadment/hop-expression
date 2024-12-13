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

import java.time.ZoneId;
import java.time.ZonedDateTime;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.DateTimeFormat;

/** Converts a timestamp to another time zone. */
@FunctionPlugin
public class ConvertTimezone extends Function {

  public ConvertTimezone() {
    super(
        "CONVERT_TIMEZONE",
        ReturnTypes.DATE_NULLABLE,
        OperandTypes.STRING_STRING_DATE.or(OperandTypes.STRING_DATE),
        OperatorCategory.DATE,
        "/docs/convert_timezone.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    if (operands.length == 3) {
      ZoneId zoneSource = DateTimeFormat.toZoneId(operands[0].getValue(String.class));
      ZoneId zoneTarget = DateTimeFormat.toZoneId(operands[1].getValue(String.class));
      ZonedDateTime value = operands[2].getValue(ZonedDateTime.class);

      if (value == null) return null;

      return value.withZoneSameLocal(zoneSource).withZoneSameInstant(zoneTarget);
    } else {
      ZoneId zoneTarget = DateTimeFormat.toZoneId(operands[0].getValue(String.class));
      ZonedDateTime value = operands[1].getValue(ZonedDateTime.class);

      if (value == null) return null;
      return value.withZoneSameInstant(zoneTarget);
    }
  }
}
