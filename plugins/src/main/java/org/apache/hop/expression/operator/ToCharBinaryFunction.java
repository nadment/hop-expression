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
import org.apache.hop.expression.IExpression;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

/**
 * Converts a binary expression to a string value.
 */
public class ToCharBinaryFunction extends ToCharFunction {

  public ToCharBinaryFunction() {
    super();
  }
  
  @Override
  public Object eval(IExpression[] operands)
      throws Exception {
    byte[] bytes = operands[0].getValue(byte[].class);
    if (bytes == null) {
      return null;
    }

    String pattern = operands[1].getValue(String.class);
    if (pattern.equals("BASE64")) {
      return Base64.getEncoder().encodeToString(bytes);
    }
    if (pattern.equals("UTF8")) {
      return new String(bytes, StandardCharsets.UTF_8);
    }
    
    return Hex.encodeHexString(bytes); 
  }
}
