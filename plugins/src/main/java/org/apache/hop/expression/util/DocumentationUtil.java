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

import org.apache.commons.io.IOUtils;
import org.apache.hop.core.logging.ILogChannel;
import org.apache.hop.core.logging.LogChannel;
import org.apache.hop.expression.IExpression;
import java.io.InputStreamReader;
import java.io.StringWriter;

public class DocumentationUtil {
  private static ILogChannel LOG = new LogChannel("EXPRESSION");
  
  private DocumentationUtil() {
    super();
  }


  public static String load(String id, String url) {
    StringWriter writer = new StringWriter();
    try (
        InputStreamReader is = new InputStreamReader(IExpression.class.getResourceAsStream(url))) {
      IOUtils.copy(is, writer);
    } catch (Exception e) {
      writer.append(e.getMessage());      
      LOG.logError("Warning missing operator documentation: " + id);
    }

    return writer.toString();
  }
  
  public static String findDescription(String doc) {
    if (doc == null) {  
      return "";
    }

    int beginIndex = doc.indexOf("id=\"preamble\"");
    beginIndex = doc.indexOf("<p>", beginIndex);

    if (beginIndex > 0) {
      int endIndex = doc.indexOf("</p>", beginIndex);

      return doc.substring(beginIndex + 3, endIndex);
    }

    return "";
  }

}