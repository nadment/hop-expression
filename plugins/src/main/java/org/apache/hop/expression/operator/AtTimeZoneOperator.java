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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;
import java.time.ZoneId;

/**
 * Converts a value of one timezone into another timezone.
 * <p>
 * <code>value AT TIME ZONE zone.<code>
 */
public class AtTimeZoneOperator extends Operator {

  public AtTimeZoneOperator() {
    super("TIMEZONE", "AT TIME ZONE", 10, true, true, ReturnTypes.DATE, OperandTypes.DATE_STRING,
        "i18n::Operator.Category.Conversion", "/docs/attimezone.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    Object value = operands[0].getValue(context);
    if (value == null)
      return null;

    ZoneId zone = toZoneId(Coerce.toString(operands[1].getValue(context)));
    return Coerce.toDateTime(value).withZoneSameInstant(zone);
  }

  protected ZoneId toZoneId(String zone) throws Exception {
    try {
      return ZoneId.of(zone);
    } catch (Exception e) {
      throw new ExpressionException(ExpressionError.UNKNOWN_TIMEZONE, zone);
    }
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append(" AT TIME ZONE ");
    operands[1].unparse(writer);
  }
}
