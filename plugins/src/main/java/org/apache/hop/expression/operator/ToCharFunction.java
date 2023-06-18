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

import org.apache.commons.codec.binary.Hex;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.BinaryDataType;
import org.apache.hop.expression.type.DateDataType;
import org.apache.hop.expression.type.NumberDataType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.DateTimeFormat;
import org.apache.hop.expression.util.NumberFormat;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import java.util.Base64;

/**
 * Converts a numeric or date expression to a string value.
 */
@FunctionPlugin
public class ToCharFunction extends Function {

  public ToCharFunction() {
    super(
        "TO_CHAR", ReturnTypes.STRING, OperandTypes.NUMERIC_OPTIONAL_TEXT
            .or(OperandTypes.DATE_OPTIONAL_TEXT).or(OperandTypes.BINARY_OPTIONAL_TEXT),
        OperatorCategory.CONVERSION, "/docs/to_char.html");
  }

  @Override
  public Object eval(IExpression[] operands)
      throws Exception {
    Object value = operands[0].getValue();
    if (value == null) {
      return null;
    }

    String pattern = null;
    if (operands.length > 1) {
      pattern = operands[1].getValue(String.class);
    }

    if (value instanceof Number) {
      if (pattern == null) {
        pattern = "TM";
      }

      return NumberFormat.of(pattern).format(NumberDataType.coerce(value));
    }

    if (value instanceof ZonedDateTime) {
      if (pattern == null) {
        // TODO: pattern = context.getVariable(ExpressionContext.EXPRESSION_DATE_FORMAT); 
      }
      return DateTimeFormat.of(pattern).format(DateDataType.coerce(value));
    }

    if (pattern == null) {
      pattern = "HEX";
    } else
      pattern = pattern.toUpperCase();

    byte[] bytes = BinaryDataType.coerce(value);

    if (pattern.equals("HEX")) {
      return Hex.encodeHexString(bytes);
    }
    if (pattern.equals("BASE64")) {
      return Base64.getEncoder().encodeToString(bytes);
    }
    if (pattern.equals("UTF-8") || pattern.equals("UTF8")) {
      return new String(bytes, StandardCharsets.UTF_8);
    }

    throw new ExpressionException(ExpressionError.INVALID_BINARY_FORMAT, pattern);
  }
}
