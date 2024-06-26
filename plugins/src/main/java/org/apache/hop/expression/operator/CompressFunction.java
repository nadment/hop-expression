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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.GZIPOutputStream;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Compress the data using the GZIP algorithm.
 *
 * <p>TODO: Compress the data using the specified algorithm. If no algorithm is supplied, GZIP is
 * used.
 *
 * <p>The function returns a byte array of type.
 */
@FunctionPlugin
public class CompressFunction extends Function {

  public CompressFunction() {
    super(
        "COMPRESS",
        ReturnTypes.BINARY_NULLABLE,
        OperandTypes.BINARY,
        OperatorCategory.STRING,
        "/docs/compress.html");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    byte[] bytes = operands[0].getValue(byte[].class);
    if (bytes == null) return null;

    try {
      ByteArrayOutputStream output = new ByteArrayOutputStream(bytes.length + 200);
      GZIPOutputStream compression = new GZIPOutputStream(output);
      compression.write(bytes);
      compression.flush();
      compression.close();

      return output.toByteArray();
    } catch (IOException e) {
      throw new ExpressionException(ErrorCode.COMPRESSION_ERROR);
    }
  }
}
