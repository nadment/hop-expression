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

package org.apache.hop.expression.util;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;
import org.apache.commons.io.IOUtils;
import org.apache.hop.core.logging.ILogChannel;
import org.apache.hop.core.logging.LogChannel;
import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/** Utility to load operator documentation and extract its description. */
@NullMarked
public final class DocumentationUtil {

  private static final ILogChannel LOG = new LogChannel("Expression");

  private DocumentationUtil() {
    // Utility class
  }

  /**
   * Loads the documentation resource for an operator.
   *
   * @param clazz the class used to resolve the documentation resource
   * @param id the unique identifier of the operator, used for error logging
   * @param documentationUrl the resource path of the documentation
   * @return the documentation content, or {@code null} if it could not be loaded
   */
  public static @Nullable String loadDocumentation(
      Class<?> clazz, String id, String documentationUrl) {
    try (StringWriter writer = new StringWriter()) {
      InputStream is = clazz.getResourceAsStream(documentationUrl);
      if (is != null) {
        InputStreamReader reader = new InputStreamReader(is);
        IOUtils.copy(reader, writer);
      }
      return writer.toString();
    } catch (Exception e) {
      LOG.logError("Missing operator documentation: {0}", id);
      return null;
    }
  }

  /**
   * Extracts the description from the preamble of an operator documentation.
   *
   * @param documentation the documentation content, may be {@code null}
   * @return the description, or an empty string if it could not be found
   */
  public static String findDocumentationDescription(@Nullable String documentation) {
    if (documentation == null) {
      return "";
    }

    int beginIndex = documentation.indexOf("id=\"preamble\"");
    beginIndex = documentation.indexOf("<p>", beginIndex);

    if (beginIndex > 0) {
      int endIndex = documentation.indexOf("</p>", beginIndex);

      return documentation.substring(beginIndex + 3, endIndex);
    }

    return "";
  }
}
