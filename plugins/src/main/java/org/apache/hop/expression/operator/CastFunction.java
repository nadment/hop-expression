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
 * Converts a value of one data type into another data type
 * <code>CAST(value AS type [FORMAT format])</code>.
 * 
 * @see CastOperator
 * @see TryCastFunction
 */
@FunctionPlugin
public class CastFunction extends Function {

  public CastFunction() {
    this("CAST");
  }

  public CastFunction(String id) {
    super(id, ReturnTypes.CAST_OPERATOR, OperandTypes.CAST_OPERATOR, Category.CONVERSION,
        "/docs/cast.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object value = operands[0].getValue();
    if (value == null)
      return null;

    Type type = operands[1].getValue(Type.class);

    String format = null;
    if (operands.length == 3) {
      format = operands[2].getValue(String.class);
    }

    return type.cast(value, format);
  }

  @Override
  public IExpression compile(final IExpressionContext context, Call call)
      throws ExpressionException {

    // Remove lossless cast
    Type source = call.getOperand(0).getType();
    Type target = call.getOperand(1).getValue(Type.class);

    // If same type name
    TypeName name = target.getName();
    if (source.is(name)) {
      if (source.getPrecision() == target.getPrecision()
          && source.getScale() == target.getScale()) {
        return call.getOperand(0);
      }

      // // If source precision and scale are specified and less or equal than target
      // if ( name.allowsScale() && source.getPrecision()!=Type.PRECISION_NOT_SPECIFIED &&
      // source.getPrecision()<=target.getPrecision() && source.getScale()!=Type.SCALE_NOT_SPECIFIED
      // && source.getScale()<=target.getScale() ) {
      // return call.getOperand(0);
      // }
      // // If source precision is specified and less or equal than target
      // else if ( name.allowsPrecNoScale() && source.getPrecision()!=Type.PRECISION_NOT_SPECIFIED
      // && source.getPrecision()<=target.getPrecision()) {
      // return call.getOperand(0);
      // }
      // else if ( name.allowsNoPrecNoScale() ) {
      // return call.getOperand(0);
      // }
    }

    return call;
  }

  @Override
  public void unparse(StringWriter writer, IExpression[] operands) {
    writer.append(this.getName());
    writer.append('(');
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
