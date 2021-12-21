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

import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.ScalarFunction;
import java.io.StringWriter;
import java.time.ZoneId;

/**
 * Converts a value of one timezone into another timezone.
 * <p>
 * <code>value AT TIME ZONE zone.<code>
 */
public class Timezone extends Operator {

  public Timezone() {
    super("TO_TIMEZONE", "AT TIME ZONE", 10, true, true, "i18n::Operator.Category.Conversion", "/docs/timezone.html");
  }

  //@ScalarFunction(id = "TIMEZONE", minArgs = 2, maxArgs = 2, category = "i18n::Operator.Category.Date", documentationUrl="/docs/timezone.html")
  public Object eval(final IExpressionContext context, IExpression[] operands)
      throws ExpressionException {
    Object value = operands[0].eval(context);
    if (value == null)
      return null;

    String zone = DataType.toString(operands[1].eval(context));
    ZoneId zoneId = ZoneId.of(zone);    
    return  DataType.toDate(value).withZoneSameInstant(zoneId);
  }

  @Override
  public void write(StringWriter writer, IExpression[] operands) {
    operands[0].write(writer);
    writer.append(" AT TIME ZONE ");
    operands[1].write(writer);
  }
}
