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

package org.apache.hop.expression.util;

import java.net.InetAddress;
import org.apache.hop.core.util.Utils;
import org.apache.hop.expression.ConversionException;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.type.TypeName;

public final class InetConversion extends Conversion<InetAddress> {

  private InetConversion() {
    // Utility class
  }

  @Override
  public Class<InetAddress> getConvertedType() {
    return InetAddress.class;
  }

  @Override
  public TypeName getTypeName() {
    return TypeName.INET;
  }

  /**
   * Convert String value to Inet.
   *
   * @param str the string to convert
   * @return InetAddress
   */
  public static InetAddress convert(final String str) throws ConversionException {
    if (str == null || Utils.isEmpty(str)) return null;

    try {
      return InetAddress.getByName(str);
    } catch (Exception e) {
      throw new ConversionException(ErrorCode.CONVERSION_ERROR_TO_INET, TypeName.STRING, str);
    }
  }
}
