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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.apache.hop.expression.ExpressionExtension;
import org.apache.hop.junit.rules.RestoreHopEnvironmentExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith({RestoreHopEnvironmentExtension.class, ExpressionExtension.class})
class DocumentationUtilTest {

  @Test
  void loadDocumentation() {
    assertNull(
        DocumentationUtil.loadDocumentation(DocumentationUtilTest.class, "TEST", "/missing.html"));
  }

  @Test
  void findDocumentationDescription() {
    assertEquals("", DocumentationUtil.findDocumentationDescription(null));
    assertEquals("", DocumentationUtil.findDocumentationDescription("<html></html>"));
    assertEquals(
        "The description",
        DocumentationUtil.findDocumentationDescription(
            "<div id=\"preamble\"><p>The description</p></div>"));

    // Paragraph not closed
    assertEquals("", DocumentationUtil.findDocumentationDescription("<div id=\"preamble\"><p>The"));
  }

  @Test
  void findDocumentationSyntax() {
    assertEquals("", DocumentationUtil.findDocumentationSyntax(null, "ABS"));
    assertEquals("", DocumentationUtil.findDocumentationSyntax("<html></html>", "ABS"));

    String documentation =
        "<h2 id=\"_syntax\">Syntax</h2><div><pre>ARRAY_SORT( array [, ascending ] )\n"
            + "ARRAY_SORT( array, (p1, p2) -&gt; comparator )\n"
            + "SORT_ARRAY( array )</pre></div><pre>value::type</pre>";

    // The first syntax of the function, the name is not case-sensitive
    assertEquals(
        "ARRAY_SORT( array [, ascending ] )",
        DocumentationUtil.findDocumentationSyntax(documentation, "ARRAY_SORT"));
    assertEquals(
        "ARRAY_SORT( array [, ascending ] )",
        DocumentationUtil.findDocumentationSyntax(documentation, "array_sort"));

    // The syntax of an alias
    assertEquals(
        "SORT_ARRAY( array )",
        DocumentationUtil.findDocumentationSyntax(documentation, "SORT_ARRAY"));

    // The characters escaped in HTML are restored
    assertEquals(
        "MAP( array, (p1, p2) -> comparator )",
        DocumentationUtil.findDocumentationSyntax(
            "<h2 id=\"_syntax\"></h2><pre>MAP( array, (p1, p2) -&gt; comparator )</pre>", "MAP"));

    // No syntax for the name, or a name that is only the start of another name
    assertEquals("", DocumentationUtil.findDocumentationSyntax(documentation, "ABS"));
    assertEquals("", DocumentationUtil.findDocumentationSyntax(documentation, "ARRAY"));

    // Section without listing, or listing not closed
    assertEquals("", DocumentationUtil.findDocumentationSyntax("<h2 id=\"_syntax\"></h2>", "ABS"));
    assertEquals(
        "", DocumentationUtil.findDocumentationSyntax("<h2 id=\"_syntax\"></h2><pre>ABS(", "ABS"));
  }
}
