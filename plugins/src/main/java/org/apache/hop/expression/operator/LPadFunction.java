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

/**
 * The function left-pads a string or binary with another string or binary, to a certain length.
 *
 * @see {@link RPadFunction}
 */
@FunctionPlugin
public class LPadFunction extends Function {

  /** The maximum size to which the padding can expand. */
  protected static final int PAD_LIMIT = 8192;

  public LPadFunction() {
    super("LPAD", ReturnTypes.ARG0,
        OperandTypes.STRING_NUMERIC_OPTIONAL_STRING.or(OperandTypes.BINARY_NUMERIC_BINARY),
        Category.STRING, "/docs/lpad.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();
    if (type.is(TypeName.BINARY)) {
      return new Call(LPadBinaryFunction.INSTANCE, call.getOperands());
    }

    return new Call(LPadStringFunction.INSTANCE, call.getOperands());
  }
}
