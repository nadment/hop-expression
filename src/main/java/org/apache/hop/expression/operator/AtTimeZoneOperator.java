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

import java.io.StringWriter;
import java.time.ZonedDateTime;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.DateTimeFormat;

/**
 * Converts a value of one time zone into another time zone.
 * <p>
 * <code>value AT TIME ZONE zone.<code>
 */
public class AtTimeZoneOperator extends Operator {

  public static final AtTimeZoneOperator INSTANCE = new AtTimeZoneOperator();

  public AtTimeZoneOperator() {
    super(
        "TIMEZONE",
        "AT TIME ZONE",
        50,
        Associativity.LEFT,
        ReturnTypes.DATE_NULLABLE,
        OperandTypes.DATE_STRING,
        OperatorCategory.CONVERSION,
        "/docs/attimezone.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    ZonedDateTime value = operands[0].getValue(ZonedDateTime.class);
    if (value == null) return null;

    String zone = operands[1].getValue(String.class);
    return value.withZoneSameLocal(DateTimeFormat.toZoneId(zone));
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer, getLeftPrec(), getRightPrec());
    writer.append(" AT TIME ZONE ");
    operands[1].unparse(writer, getLeftPrec(), getRightPrec());
  }
}
