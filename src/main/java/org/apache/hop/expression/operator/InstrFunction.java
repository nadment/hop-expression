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
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Returns the position in the string that is the first character of a specified occurrence of the
 * substring.
 */
@FunctionPlugin
public class InstrFunction extends Function {

  public InstrFunction() {
    super(
        "INSTR",
        ReturnTypes.INTEGER_NULLABLE,
        OperandTypes.STRING_STRING
            .or(OperandTypes.STRING_STRING_INTEGER)
            .or(OperandTypes.STRING_STRING_INTEGER_INTEGER),
        OperatorCategory.STRING,
        "/docs/instr.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String str = operands[0].getValue(String.class);
    if (str == null) {
      return null;
    }
    String substr = operands[1].getValue(String.class);
    if (substr == null) {
      return null;
    }

    int start = 0;
    int occurrence = 1;
    int result = 0;

    // If 3 operands, indicate the position to start
    if (operands.length >= 3) {
      start = operands[2].getValue(Long.class).intValue();

      if (start == 0) {
        throw new IllegalArgumentException(ErrorCode.ARGUMENT_OUT_OF_RANGE.message(3, start));
      }

      start -= 1;

      // The occurrence to find, must be positive
      if (operands.length == 4) {
        occurrence = operands[3].getValue(Long.class).intValue();
        if (occurrence < 1) {
          throw new IllegalArgumentException(ErrorCode.ARGUMENT_OUT_OF_RANGE.message(occurrence));
        }
      }
    }

    if (start >= 0) {
      while ((result = str.indexOf(substr, start)) > 0) {
        if (--occurrence <= 0) break;
        start = result + substr.length();
      }
    } else if (start < 0) {
      start = str.length() + start;
      while ((result = str.lastIndexOf(substr, start)) > 0) {
        if (--occurrence <= 0) break;
        start = result - substr.length();
      }
    }

    return Long.valueOf(result + 1L);
  }
}
