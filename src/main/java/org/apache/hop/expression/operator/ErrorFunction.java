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

import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/** This function throws the given error message and abort execution of the pipeline or workflow. */
@FunctionPlugin
public class ErrorFunction extends Function {

  public ErrorFunction() {
    super(
        "ERROR",
        ReturnTypes.ANY,
        OperandTypes.STRING,
        OperatorCategory.SPECIAL,
        "/docs/error.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String message = operands[0].getValue(String.class);
    throw new ExpressionException(ErrorCode.MESSAGE_ERROR, message);
  }
}
