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

import org.apache.hop.expression.Category;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * The function return the Unicode code point for the first Unicode character in a string. If the
 * string is empty, a value of 0 is returned.
 *
 * @see {@link ChrFunction}, {@link AsciiFunction},
 */
@FunctionPlugin
public class UnicodeFunction extends Function {

  public UnicodeFunction() {
    super("UNICODE", ReturnTypes.INTEGER, OperandTypes.STRING, Category.STRING,
        "/docs/unicode.html");
  }

  @Override
    public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null)
      return null;

    int codePoint = 0;
    if (value.length() > 0) {
      codePoint = value.codePointAt(0);
    }
    return Long.valueOf(codePoint);
  }
}
