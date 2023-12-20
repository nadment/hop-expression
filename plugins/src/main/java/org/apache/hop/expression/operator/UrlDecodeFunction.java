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

import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;

/**
 * The function decode the URL to a string.
 *
 * @see {@link UrlEncodeFunction}
 */
@FunctionPlugin
public class UrlDecodeFunction extends Function {

  public UrlDecodeFunction() {
    super("URL_DECODE", ReturnTypes.STRING_NULLABLE, OperandTypes.STRING, OperatorCategory.STRING,
        "/docs/url_decode.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String value = operands[0].getValue(String.class);
    if (value == null)
      return null;

    return URLDecoder.decode(value, StandardCharsets.UTF_8);
  }
}
