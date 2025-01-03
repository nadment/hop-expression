/*
 *
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
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Interval;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.TimeUnit;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.Types;

/**
 * Extracts the specified time unit from a date, timestamp or interval.
 *
 * <p>Time unit: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
 */
@FunctionPlugin(names = "DATE_PART")
public class ExtractFunction extends Function {

  public ExtractFunction() {
    super(
        "EXTRACT",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.TIMEUNIT_DATE.or(OperandTypes.TIMEUNIT_INTERVAL),
        OperatorCategory.DATE,
        "/docs/extract.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    Type type = call.getOperand(1).getType();
    if (Types.isInterval(type)) {
      return new Call(IntervalExtractFunction.INSTANCE, call.getOperands());
    }
    return new Call(DateExtractFunction.INSTANCE, call.getOperands());
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("EXTRACT(");
    operands[0].unparse(writer, 0, 0);
    writer.append(" FROM ");
    operands[1].unparse(writer, 0, 0);
    writer.append(')');
  }

  /** Extracts the specified date or time part from a date, time, or timestamp. */
  private static final class DateExtractFunction extends ExtractFunction {
    public static final DateExtractFunction INSTANCE = new DateExtractFunction();

    @Override
    public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
      // Replace EXTRACT from date with the corresponding function YEAR, DAY, HOUR... only if
      // without time zone
      TimeUnit unit = call.getOperand(0).getValue(TimeUnit.class);
      Function function = FunctionRegistry.getFunction(unit.name());
      if (function != null && !(function instanceof UserDefinedFunction)) {
        return new Call(function, call.getOperand(1));
      }

      return call;
    }

    @Override
    public Object eval(final IExpression[] operands) {

      TimeUnit unit = operands[0].getValue(TimeUnit.class);

      ZonedDateTime datetime = operands[1].getValue(ZonedDateTime.class);
      if (datetime == null) return null;

      return unit.extract(datetime);
    }
  }

  /** Extracts the specified time unit from a interval. */
  private static final class IntervalExtractFunction extends ExtractFunction {
    public static final IntervalExtractFunction INSTANCE = new IntervalExtractFunction();

    @Override
    public Object eval(final IExpression[] operands) {

      TimeUnit unit = operands[0].getValue(TimeUnit.class);

      Interval interval = operands[1].getValue(Interval.class);
      if (interval == null) return null;

      return unit.extract(interval);
    }
  }
}
