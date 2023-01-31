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

import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Converter;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;

/**
 * Converts a numeric or date expression to a string value.
 */
@FunctionPlugin
public class ToCharFunction extends Function {

  public ToCharFunction() {
    super(
        "TO_CHAR", true, ReturnTypes.STRING, OperandTypes.NUMERIC_OPTIONAL_TEXT
            .or(OperandTypes.DATE_OPTIONAL_TEXT).or(OperandTypes.BINARY_OPTIONAL_TEXT),
        OperatorCategory.CONVERSION, "/docs/to_char.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object value = operands[0].getValue(context);
    if (value == null) {
      return null;
    }

    String pattern = null;
    if (operands.length > 1) {
      pattern = operands[1].getValue(context, String.class);
    }

    if (value instanceof Number) {
      if (pattern == null) {
        pattern = "TM";
      }

      return NumberFormat.of(pattern).format(Converter.coerceToBigNumber(value));
    }

    if (value instanceof ZonedDateTime) {
      if (pattern == null) {
        pattern = context.getVariable(ExpressionContext.EXPRESSION_DATE_FORMAT);
      }
      return DateTimeFormat.of(pattern).format(Converter.coerceToDateTime(value));
    }
    

      if (pattern == null) {
        pattern = "HEX";
      } else
        pattern = pattern.toUpperCase();

      byte[] bytes = Converter.coerceToBinary(value);

      if (pattern.equals("HEX")) {
        return Converter.toStringHex(bytes);
      }
      if (pattern.equals("BASE64")) {
        return Converter.toStringBase64(bytes);
      }
      if (pattern.equals("UTF-8") || pattern.equals("UTF8")) {
        return new String(bytes, StandardCharsets.UTF_8);
      }

      throw new ExpressionException(ExpressionError.INVALID_BINARY_FORMAT, pattern);
  }
}
