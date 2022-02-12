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
import org.apache.hop.core.compress.CompressionInputStream;
import org.apache.hop.core.compress.CompressionOutputStream;
import org.apache.hop.core.compress.CompressionPluginType;
import org.apache.hop.core.compress.ICompressionProvider;
import org.apache.hop.core.plugins.IPlugin;
import org.apache.hop.core.plugins.PluginRegistry;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.Error;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.ScalarFunction;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class ExperimentalFunction {
 
  private ExperimentalFunction() {}

  /**
   * Compresses white space.
   * White space is defined as any sequence of blanks, null characters, newlines (line feeds),
   * carriage returns, horizontal tabs and form feeds (vertical tabs). Trims white space from the
   * beginning and end of the string, and replaces all other white space with single blanks.
   * This function is useful for comparisons. The value for c1 must be a string of variablelength
   * character string data type (not fixed-length character data type). The result is the same
   * length as the argument.
   */
  @ScalarFunction(id = "SQUEEZE", category = "i18n::Operator.Category.String", minArgs = 1, maxArgs = 1)  
  public static Object squeeze(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    String str = DataType.toString(v0);
    char[] a = str.toCharArray();
    int n = 1;
    for (int i = 1; i < a.length; i++) { 
        a[n] = a[i];
        if (!Character.isSpaceChar(a[n])) n++;
        else {
          a[n] = ' ';
          if (a[n-1] != ' ') n++;        
        }
    }
    
    return new String(a, 0, n);    
  }
  
  // TODO: Use a cache
  private static ICompressionProvider getCompressionProvider(String id)
      throws ExpressionException {
    PluginRegistry registry = PluginRegistry.getInstance();
    for (IPlugin plugin : registry.getPlugins(CompressionPluginType.class)) {
      if (id.equalsIgnoreCase(plugin.getIds()[0])) {
        try {
          return registry.loadClass(plugin, ICompressionProvider.class);
        } catch (Exception e) {
          throw new ExpressionException(Error.ILLEGAL_ARGUMENT, id);
        }
      }
    }
    return null;
  }

/**
 * Compress the data using the specified algorithm. If no algorithm is supplied, GZIP is used.
 * 
 * The function returns a byte array of type.
 * 
 * @param context
 * @param operands
 * @return
 * @throws ExpressionException
 */
  //@ScalarFunction(id = "COMPRESS", category = "i18n::Operator.Category.String", minArgs = 1, maxArgs = 1)
//  public static Object compress(final IExpressionContext context, final IExpression[] operands)
//      throws ExpressionException {
//    Object v0 = operands[0].eval(context);
//    if (v0 == null)
//      return null;
//
//    String algorithm = "SNAPPY";
//    ICompressionProvider provider = getCompressionProvider(algorithm);
//    try {
//      byte[] bytes = DataType.toBinary(v0);   
//      ByteArrayOutputStream output = new ByteArrayOutputStream(bytes.length+200);
//      CompressionOutputStream  compression = provider.createOutputStream(output);
//      compression.write(bytes);
//      compression.flush();      
//      return output.toByteArray();
//    } catch (IOException e) {
//      throw new ExpressionException("Compress {0} error {1}", algorithm, e.getMessage());
//    }
//  }
//

  /** 
   * Decompress an input value, using the GZIP algorithm. 
   * DECOMPRESS will return a byte array 
   * @param context
   * @param operands
   * @return
   * @throws ExpressionException
   */
 // @ScalarFunction(id = "DECOMPRESS", category = "i18n::Operator.Category.String", minArgs = 1, maxArgs = 1)
//  public static Object decompress(final IExpressionContext context, final IExpression[] operands)
//      throws ExpressionException {
//    Object v0 = operands[0].eval(context);
//    if (v0 == null)
//      return null;
//    
//    String algorithm = "SNAPPY";
//    ICompressionProvider provider = getCompressionProvider(algorithm);
//    
//    try {      
//      byte[] bytes = DataType.toBinary(v0);
//      ByteArrayOutputStream output = new ByteArrayOutputStream(bytes.length+100);
//      CompressionInputStream decompression = provider.createInputStream(new ByteArrayInputStream(bytes));      
//      final byte[] buffer = new byte[8024];
//      int n = 0;
//      while (-1 != (n = decompression.read(buffer))) {
//          output.write(buffer, 0, n);
//      }      
//      return output.toByteArray();
//    } catch (IOException e) {
//      throw new ExpressionException("Decompress {0} error {1}", algorithm, e.getMessage());
//    }
//  }


  /** Week from the beginning of the month (0-5) */
//  @ScalarFunction(id = "WEEKOFMONTH", category = "i18n::Operator.Category.Date")
//  public static Object weekOfMonth(final IExpressionContext context, final IExpression[] operands)
//      throws ExpressionException {
//    Object value = operands[0].eval(context);
//    if (value == null)
//      return null;
//
//    ZonedDateTime datetime = DataType.toDate(value);
//    return Long.valueOf(datetime.get(ChronoField.ALIGNED_WEEK_OF_MONTH));
//  }
}
