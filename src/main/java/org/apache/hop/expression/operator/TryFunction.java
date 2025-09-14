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

import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * The function ensures that errors during expression evaluation result in NULL instead of causing
 * an error.
 */
@FunctionPlugin
public class TryFunction extends Function {

  public static final TryFunction INSTANCE = new TryFunction();

  public TryFunction() {
    super("TRY", ReturnTypes.ARG0, OperandTypes.ANY, OperatorCategory.SPECIAL, "/docs/try.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    try {
      return operands[0].getValue();
    } catch (ExpressionException exception) {
      return switch (exception.getErrorCode()) {
        case ARITHMETIC_OVERFLOW,
                ARGUMENT_OUT_OF_RANGE,
                DIVISION_BY_ZERO,
                CONVERSION_ERROR,
                CONVERSION_OVERFLOW,
                UNPARSABLE_NUMBER_WITH_FORMAT,
                UNPARSABLE_DATE_WITH_FORMAT,
                UNPARSABLE_BINARY_WITH_FORMAT ->
            null;
        default -> throw exception;
      };
    }
  }
}
