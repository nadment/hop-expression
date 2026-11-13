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

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import org.apache.commons.io.IOUtils;
import org.apache.commons.text.StringEscapeUtils;
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
    try (InputStream is = clazz.getResourceAsStream(documentationUrl)) {
      // No log here: the documentation is only generated when packaging, and an operator can be
      // created before the logging of Hop is initialized
      if (is == null) {
        return null;
      }
      return IOUtils.toString(is, StandardCharsets.UTF_8);
    } catch (IOException e) {
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
      if (endIndex > 0) {
        return documentation.substring(beginIndex + 3, endIndex);
      }
    }

    return "";
  }

  /**
   * Extracts the syntax of a function from the section "Syntax" of its documentation, like {@code
   * ABS( value )}. A documentation can give several syntaxes, for the forms or the aliases of a
   * function: the first syntax starting with the name is returned.
   *
   * @param documentation the documentation content, may be {@code null}
   * @param name the name of the function
   * @return the syntax, or an empty string if it could not be found
   */
  public static String findDocumentationSyntax(@Nullable String documentation, String name) {
    if (documentation == null) {
      return "";
    }

    int beginIndex = documentation.indexOf("id=\"_syntax\"");
    if (beginIndex < 0) {
      return "";
    }
    beginIndex = documentation.indexOf("<pre>", beginIndex);
    int endIndex = documentation.indexOf("</pre>", beginIndex);
    if (beginIndex < 0 || endIndex < 0) {
      return "";
    }

    String prefix = name + "(";
    for (String line : documentation.substring(beginIndex + 5, endIndex).split("\n")) {
      String syntax = line.strip();
      if (syntax.regionMatches(true, 0, prefix, 0, prefix.length())) {
        // The documentation is HTML, a character like '>' is escaped
        return StringEscapeUtils.unescapeHtml4(syntax);
      }
    }

    return "";
  }
}
