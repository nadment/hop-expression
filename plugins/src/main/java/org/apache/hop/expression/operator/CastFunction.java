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
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.DataTypeName;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.StringWriter;

/**
 * Converts a value of one data type into another data type <code>::</code> or <code>
 * CAST(value AS type [FORMAT format])</code>.
 */
@FunctionPlugin
public class CastFunction extends Function {

  // Function
  public CastFunction() {
    super("CAST", true, ReturnTypes.CAST, OperandTypes.ANY_DATATYPE_OPTIONAL_TEXT,
        OperatorCategory.CONVERSION, "/docs/cast.html");
  }

  // Operator
  public CastFunction(String name) {
    super("CAST", name, 40, true, true, ReturnTypes.CAST, OperandTypes.ANY_DATATYPE_OPTIONAL_TEXT,
        OperatorCategory.CONVERSION, "/docs/cast.html");
  }

  @Override
  public Object eval(final IExpressionContext context, IExpression[] operands) throws Exception {
    Object value = operands[0].getValue(context);
    if (value == null)
      return null;

    Object v1 = operands[1].getValue(context);

    if (!(v1 instanceof DataTypeName)) {
      throw new ExpressionException(ExpressionError.INVALID_DATATYPE, v1);
    }

    DataTypeName type = (DataTypeName) v1;
    String format = null;
    if (operands.length == 3) {
      format = Coerce.toString(operands[2].getValue(context));
    }

    return Converter.to(value, type, format);
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append("CAST(");
    operands[0].unparse(writer);
    writer.append(" AS ");
    writer.append(operands[1].toString());
    if (operands.length == 3) {
      writer.append(" FORMAT ");
      operands[2].unparse(writer);
    }
    writer.append(')');
  }
}
