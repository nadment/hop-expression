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

import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.time.ZoneId;
import java.time.ZonedDateTime;

/**
 * Converts a timestamp to another time zone.
 */
//TODO: @FunctionPlugin
public class ConvertTimezone extends Function {

  public ConvertTimezone() {
    super("CONVERT_TIMEZONE", true, ReturnTypes.DATE, OperandTypes.STRING_DATE, OperatorCategory.DATE,
        "/docs/convert_timezone.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    ZoneId zone = toZoneId(operands[0].getValue(context, String.class));
    
    ZonedDateTime value = operands[1].getValue(context, ZonedDateTime.class);
    if (value == null)
      return null;
    
    return value.withZoneSameInstant(zone);
  }

  protected ZoneId toZoneId(final String zone) throws Exception {
    try {
      return ZoneId.of(zone);
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.UNKNOWN_TIMEZONE, zone);
    }
  }
}
