/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package org.apache.hop.expression;

import org.apache.commons.io.IOUtils;
import org.apache.hop.core.logging.LogChannel;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.util.concurrent.ConcurrentHashMap;

public class OperatorUtils {

  private OperatorUtils() {
    // Utility
  }

  private static final ConcurrentHashMap<String, String> docs = new ConcurrentHashMap<>();
  public static String getHtml(String name) {
    String doc = docs.get(name);
    if (doc != null) {
      return doc;
    }

    doc = readAsciidoc(name);
    docs.put(name, doc);

    return doc;
  }

  private static String readAsciidoc(String name) {
    String file = "/docs/" + name.toLowerCase() + ".html";

    StringWriter writer = new StringWriter();

    try (
        InputStreamReader is = new InputStreamReader(IExpression.class.getResourceAsStream(file))) {
      IOUtils.copy(is, writer);
    } catch (Exception e) {
      writer.append(e.getMessage());
      LogChannel.GENERAL.logDebug("Warning no documentation : " + name);
    }

    return writer.toString();
  }

  /* package */ static String findDescription(String name) {
    String doc = getHtml(name);

    if (doc == null)
      return "";

    int beginIndex = doc.indexOf("id=\"preamble\"");
    beginIndex = doc.indexOf("<p>", beginIndex);

    if (beginIndex > 0) {
      int endIndex = doc.indexOf("</p>", beginIndex);

      return doc.substring(beginIndex + 3, endIndex);
    }

    return "";
  }
}
