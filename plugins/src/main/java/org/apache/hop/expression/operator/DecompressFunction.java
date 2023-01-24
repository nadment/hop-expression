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
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.Coerce;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.zip.GZIPInputStream;

/**
 * Decompress an input value, using the GZIP algorithm.
 * The function returns a byte array of type.
 */
@FunctionPlugin
public class DecompressFunction extends Function {

  public DecompressFunction() {
    super("DECOMPRESS", true, ReturnTypes.BINARY, OperandTypes.BINARY, OperatorCategory.STRING,
        "/docs/decompress.html");
  }

  @Override
  public Object eval(final IExpressionContext context, final IExpression[] operands)
      throws Exception {
    Object v0 = operands[0].getValue(context);
    if (v0 == null)
      return null;

    String algorithm = "SNAPPY";
    byte[] bytes = Coerce.toBinary(v0);


    ByteArrayOutputStream output = new ByteArrayOutputStream(bytes.length + 100);
    // CompressionInputStream cis = provider.createInputStream(new ByteArrayInputStream(bytes));
    GZIPInputStream cis = new GZIPInputStream(new ByteArrayInputStream(bytes));

    final byte[] buffer = new byte[8024];
    int n = 0;
    while ((n = cis.read(buffer)) != -1) {
      output.write(buffer, 0, n);
    }
    return output.toByteArray();
  }

  private static ICompressionProvider getCompressionProviderByName(String id)
      throws ExpressionException {
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
