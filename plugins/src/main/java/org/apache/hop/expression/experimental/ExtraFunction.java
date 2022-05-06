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

import org.apache.hop.core.compress.CompressionPluginType;
import org.apache.hop.core.compress.ICompressionProvider;
import org.apache.hop.core.plugins.IPlugin;
import org.apache.hop.core.plugins.PluginRegistry;
import org.apache.hop.expression.ExpressionError;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.ScalarFunction;
import org.apache.hop.expression.util.Coerse;
import java.text.Normalizer;
import java.util.regex.Pattern;

public class ExtraFunction {

  private ExtraFunction() {}

  private static final Pattern DIACRITICS =
      Pattern.compile("[\\p{InCombiningDiacriticalMarks}\\p{IsLm}\\p{IsSk}]+");

  /**
   * The function removes accents (diacritic marks) from a given string.
   * Note that ligatures will be left as is.
   */
  @ScalarFunction(id = "UNACCENT", category = "i18n::Operator.Category.String", minArgs = 1,
      maxArgs = 1, documentationUrl = "/docs/unaccent.html")
  public static Object unaccent(final IExpressionContext context, final IExpression[] operands)
      throws ExpressionException {
    Object v0 = operands[0].eval(context);
    if (v0 == null)
      return null;

    String str = Coerse.toString(v0);

    final StringBuilder decomposed =
        new StringBuilder(Normalizer.normalize(str, Normalizer.Form.NFD));

    for (int i = 0; i < decomposed.length(); i++) {
      if (decomposed.charAt(i) == '\u0141') {
        decomposed.deleteCharAt(i);
        decomposed.insert(i, 'L');
      } else if (decomposed.charAt(i) == '\u0142') {
        decomposed.deleteCharAt(i);
        decomposed.insert(i, 'l');
      }
    }

    // Note that this doesn't correctly remove ligatures...
    return DIACRITICS.matcher(decomposed).replaceAll("");
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
  // @ScalarFunction(id = "COMPRESS", category = "i18n::Operator.Category.String", minArgs = 1,
  // maxArgs = 1)
  // public static Object compress(final IExpressionContext context, final IExpression[] operands)
  // throws ExpressionException {
  // Object v0 = operands[0].eval(context);
  // if (v0 == null)
  // return null;
  //
  // String algorithm = "SNAPPY";
  // ICompressionProvider provider = getCompressionProvider(algorithm);
  // try {
  // byte[] bytes = DataType.toBinary(v0);
  // ByteArrayOutputStream output = new ByteArrayOutputStream(bytes.length+200);
  // CompressionOutputStream compression = provider.createOutputStream(output);
  // compression.write(bytes);
  // compression.flush();
  // return output.toByteArray();
  // } catch (IOException e) {
  // throw new ExpressionException("Compress {0} error {1}", algorithm, e.getMessage());
  // }
  // }
  //

  /**
   * Decompress an input value, using the GZIP algorithm.
   * DECOMPRESS will return a byte array
   * 
   * @param context
   * @param operands
   * @return
   * @throws ExpressionException
   */
  // @ScalarFunction(id = "DECOMPRESS", category = "i18n::Operator.Category.String", minArgs = 1,
  // maxArgs = 1)
  // public static Object decompress(final IExpressionContext context, final IExpression[] operands)
  // throws ExpressionException {
  // Object v0 = operands[0].eval(context);
  // if (v0 == null)
  // return null;
  //
  // String algorithm = "SNAPPY";
  // ICompressionProvider provider = getCompressionProvider(algorithm);
  //
  // try {
  // byte[] bytes = DataType.toBinary(v0);
  // ByteArrayOutputStream output = new ByteArrayOutputStream(bytes.length+100);
  // CompressionInputStream decompression = provider.createInputStream(new
  // ByteArrayInputStream(bytes));
  // final byte[] buffer = new byte[8024];
  // int n = 0;
  // while (-1 != (n = decompression.read(buffer))) {
  // output.write(buffer, 0, n);
  // }
  // return output.toByteArray();
  // } catch (IOException e) {
  // throw new ExpressionException("Decompress {0} error {1}", algorithm, e.getMessage());
  // }
  // }


  /** Week from the beginning of the month (0-5) */
  // @ScalarFunction(id = "WEEKOFMONTH", category = "i18n::Operator.Category.Date")
  // public static Object weekOfMonth(final IExpressionContext context, final IExpression[]
  // operands)
  // throws ExpressionException {
  // Object value = operands[0].eval(context);
  // if (value == null)
  // return null;
  //
  // ZonedDateTime datetime = Coerse.toDate(value);
  // return Long.valueOf(datetime.get(ChronoField.ALIGNED_WEEK_OF_MONTH));
  // }
}
