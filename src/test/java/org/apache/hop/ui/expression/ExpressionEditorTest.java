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
package org.apache.hop.ui.expression;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.junit.jupiter.api.Test;

class ExpressionEditorTest {

  private static String toggle(String source, int offset, int length) throws Exception {
    IDocument document = new Document(source);
    ExpressionEditor.toggleComment(document, offset, length);
    return document.get();
  }

  @Test
  void toggleCommentOfLine() throws Exception {
    // The line of the caret is commented, then uncommented
    assertEquals("// 1 + 2", toggle("1 + 2", 2, 0));
    assertEquals("1 + 2", toggle("// 1 + 2", 0, 0));
    assertEquals("1 + 2", toggle("//1 + 2", 0, 0));
    // A comment after spaces, and a comment with two dashes, are uncommented
    assertEquals("  1 + 2", toggle("  // 1 + 2", 0, 0));
    assertEquals("1 + 2", toggle("-- 1 + 2", 0, 0));
    // A blank line is commented if it is the only line
    assertEquals("// ", toggle("", 0, 0));
  }

  @Test
  void toggleCommentOfSelection() throws Exception {
    String source = "CASE\n  WHEN A THEN 1\n\nEND";
    String commented = "// CASE\n//   WHEN A THEN 1\n\n// END";
    // The blank lines are not commented
    assertEquals(commented, toggle(source, 0, source.length()));
    assertEquals(source, toggle(commented, 0, commented.length()));

    // A line not commented comments all the lines
    assertEquals("// // 1\n// 2", toggle("// 1\n2", 0, 6));

    // A selection that ends at the start of a line does not include this line
    assertEquals("// 1\n2", toggle("1\n2", 0, 2));
  }
}
