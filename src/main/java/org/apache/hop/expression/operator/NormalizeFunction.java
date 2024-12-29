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

import java.text.Normalizer;
import java.text.Normalizer.Form;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * The function returns a string as a normalized string.
 *
 * <p>Form must be one of NFC, NFD, NFKC, NFKD, which are the four Unicode normalization methods
 * (NFC is default).
 */
@FunctionPlugin
public class NormalizeFunction extends Function {

  public NormalizeFunction() {
    super(
        "NORMALIZE",
        ReturnTypes.STRING_NULLABLE,
        OperandTypes.STRING.or(OperandTypes.STRING_STRING),
        OperatorCategory.STRING,
        "/docs/normalize.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null) return null;

    Form form = Form.NFC;

    if (operands.length == 2) {
      String name = operands[1].getValue(String.class);
      try {
        form = Form.valueOf(name);
      } catch (Exception e) {
        throw new ExpressionException(ErrorCode.INVALID_ARGUMENT, name);
      }
    }

    return Normalizer.normalize(value, form);
  }
}
