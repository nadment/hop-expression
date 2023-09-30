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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;
import java.io.StringWriter;

/**
 * Extracts the specified time unit from a date, timestamp or interval.
 * 
 * Time unit: DECADE | YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND...
 */
@FunctionPlugin(names = "DATE_PART")
public class ExtractFunction extends Function {

  public ExtractFunction() {
    super("EXTRACT", ReturnTypes.INTEGER, OperandTypes.TIMEUNIT_TEMPORAL.or(OperandTypes.TIMEUNIT_INTERVAL), Category.DATE,
        "/docs/extract.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    Type type = call.getOperand(1).getType();
    if (type.is(TypeName.INTERVAL)) {
      return new Call(ExtractIntervalFunction.INSTANCE, call.getOperands());
    }    
    return new Call(ExtractDateFunction.INSTANCE, call.getOperands());
  }

  protected static int millennium(int year) {
    return year > 0 ? (year + 999) / 1000 : year / 1000;
  }

  protected static int century(int year) {
    return year > 0 ? (year + 99) / 100 : year / 100;
  }

  protected static int decade(int year) {
    return year >= 0 ? year / 10 : (year - 9) / 10;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("EXTRACT(");
    operands[0].unparse(writer);
    writer.append(" FROM ");
    operands[1].unparse(writer);
    writer.append(')');
  }
}
