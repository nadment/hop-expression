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

import org.apache.hop.expression.Call;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import java.io.StringWriter;

/**
 * Converts a value of one data type into another data type <code>::</code>
 * 
 * @see CastFunction
 */
public class CastOperator extends Operator {

  public CastOperator() {
    super("CAST", "::", 40, true, ReturnTypes.CAST_OPERATOR, OperandTypes.CAST_OPERATOR,
        OperatorCategory.CONVERSION, "/docs/cast.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {
    return new Call(FunctionRegistry.getFunction("CAST"), call.getOperands());
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object value = operands[0].getValue();
    Type type = operands[1].getValue(Type.class);

    return type.cast(value, null);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    operands[0].unparse(writer);
    writer.append("::");
    writer.append(operands[1].toString());
  }
}
