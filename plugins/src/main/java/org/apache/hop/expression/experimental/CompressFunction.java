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
package org.apache.hop.expression.experimental;

import org.apache.hop.core.compress.CompressionOutputStream;
import org.apache.hop.core.compress.CompressionPluginType;
import org.apache.hop.core.compress.ICompressionProvider;
import org.apache.hop.core.plugins.IPlugin;
import org.apache.hop.core.plugins.PluginRegistry;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.util.Coerse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * Compress the data using the specified algorithm. If no algorithm is supplied, GZIP is used.
 * 
 * The function returns a byte array of type.
 */
//@FunctionPlugin
public class CompressFunction extends Function {

  public CompressFunction() {
    super("COMPRESS", true, ReturnTypes.BINARY, OperandTypes.BINARY,
        "i18n::Operator.Category.String", "/docs/compress.html");
  }
  
  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;

    String algorithm = "SNAPPY";
    ICompressionProvider provider = getCompressionProvider(algorithm);
    try {
      byte[] bytes = Coerse.toBinary(v0);
      ByteArrayOutputStream output = new ByteArrayOutputStream(bytes.length + 200);
      CompressionOutputStream compression = provider.createOutputStream(output);
      compression.write(bytes);
      compression.flush();
      return output.toByteArray();
    } catch (IOException e) {
      //throw new ExpressionException("Compress {0} error {1}", algorithm, e.getMessage());
      throw new ExpressionException(ExpressionError.OPERATOR_ERROR, algorithm, e.getMessage());
    }
  }
  
  // TODO: Use a cache
  private static ICompressionProvider getCompressionProvider(String id) throws ExpressionException {
    PluginRegistry registry = PluginRegistry.getInstance();
    for (IPlugin plugin : registry.getPlugins(CompressionPluginType.class)) {
      if (id.equalsIgnoreCase(plugin.getIds()[0])) {
        try {
          return registry.loadClass(plugin, ICompressionProvider.class);
        } catch (Exception e) {
          throw new ExpressionException(ExpressionError.ILLEGAL_ARGUMENT, id);
        }
      }
    }
    return null;
  }
}
