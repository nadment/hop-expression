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
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.Literal;
import org.apache.hop.expression.exception.ExpressionException;
import org.apache.hop.expression.type.NumberType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.Type;
import org.apache.hop.expression.type.TypeName;

/**
 * Converts a numeric or date expression to a string value.
 */
@FunctionPlugin
public class ToCharFunction extends Function {

  public ToCharFunction() {
    super(
        "TO_CHAR", ReturnTypes.STRING, OperandTypes.NUMERIC_OPTIONAL_TEXT
            .or(OperandTypes.TEMPORAL_OPTIONAL_TEXT).or(OperandTypes.BINARY_OPTIONAL_TEXT),
        Category.CONVERSION, "/docs/to_char.html");
  }

  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();

    String pattern = null;
    if (call.getOperandCount() > 1) {
      pattern = call.getOperand(1).getValue(String.class);
    }

    if (type.is(TypeName.STRING) && call.getOperandCount() == 1) {
      return call.getOperand(0);
    }

    if (type.is(TypeName.DATE)) {
      if (pattern == null) {
        pattern = context.getVariable(ExpressionContext.EXPRESSION_DATE_FORMAT);
      }

      return new Call(ToCharDateFunction.INSTANCE, call.getOperand(0), Literal.of(pattern));
    }

    if (type.isSameFamily(NumberType.NUMBER)) {
      if (pattern == null) {
        pattern = "TM";
      }

      return new Call(ToCharNumberFunction.INSTANCE, call.getOperand(0), Literal.of(pattern));
    }

    if (type.is(TypeName.BINARY)) {
      if (pattern == null) {
        pattern = "HEX";
      }

      // Normalize pattern
      pattern = pattern.toUpperCase();
      if (pattern.equals("UTF-8")) {
        pattern = "UTF8";
      }

      if (!(pattern.equals("HEX") || pattern.equals("BASE64") || pattern.equals("UTF8"))) {
        throw new ExpressionException(ExpressionError.INVALID_BINARY_FORMAT, pattern);
      }

      return new Call(ToCharBinaryFunction.INSTANCE, call.getOperand(0), Literal.of(pattern));
    }

    return call;
  }
}
