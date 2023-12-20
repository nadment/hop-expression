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

import org.apache.commons.codec.language.Soundex;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * 
 */
@FunctionPlugin
public class DifferenceFunction extends Function {
  private static final Soundex SOUNDEX = new Soundex();
  private static final int SOUNDEX_LENGTH = 4;

  public DifferenceFunction() {
    super("DIFFERENCE", ReturnTypes.INTEGER_NULLABLE, OperandTypes.STRING_STRING, OperatorCategory.STRING,
        "/docs/difference.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    String v0 = operands[0].getValue(String.class);
    if (v0 == null)
      return null;
    String v1 = operands[1].getValue(String.class);
    if (v1 == null)
      return null;

    return Long.valueOf(difference(v0, v1));
  }

  public static int difference(String s0, String s1) {
    String result0 = SOUNDEX.soundex(s0);
    String result1 = SOUNDEX.soundex(s1);
    for (int i = 0; i < SOUNDEX_LENGTH; i++) {
      if (result0.charAt(i) != result1.charAt(i)) {
        return i;
      }
    }
    return SOUNDEX_LENGTH;
  }
}
