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

import org.apache.commons.codec.DecoderException;
import org.apache.hop.expression.FunctionPlugin;

/**
 * Converts the string expression to a binary value.
 */
@FunctionPlugin
public class TryToBinaryFunction extends ToBinaryFunction {

  public TryToBinaryFunction() {
    super("TRY_TO_BINARY");
  }

  @Override
  protected byte[] formatHex(String value) throws DecoderException {
    try {
      return super.formatHex(value);
    } catch (Exception e) {
      return null;
    }
  }

  @Override
  protected byte[] formatBase64(String value) {
    try {
      return super.formatBase64(value);
    } catch (Exception e) {
      return null;
    }
  }

  @Override
  protected byte[] formatUtf8(String value) {
    try {
      return super.formatUtf8(value);
    } catch (Exception e) {
      return null;
    }
  }
}
