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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.swt.SWT;
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

  @Test
  void sourcePositionWithoutVariable() {
    // Without variable resolved, the position is the same
    assertEquals(new Position(2, 0), ExpressionEditor.findSourcePosition("1+x", "1+x", 2));
    // An error at the end of the expression
    assertEquals(new Position(3, 0), ExpressionEditor.findSourcePosition("1+x", "1+x", 3));
  }

  @Test
  void sourcePositionWithVariable() {
    String source = "1+${V}+2";
    String resolved = "1+abc+2";
    // Before the variable, the position is the same
    assertEquals(new Position(1, 0), ExpressionEditor.findSourcePosition(source, resolved, 1));
    // In the value of the variable, the variable is underlined
    assertEquals(new Position(2, 4), ExpressionEditor.findSourcePosition(source, resolved, 2));
    assertEquals(new Position(2, 4), ExpressionEditor.findSourcePosition(source, resolved, 4));
    // After the variable, the position is moved
    assertEquals(new Position(7, 0), ExpressionEditor.findSourcePosition(source, resolved, 6));
    assertEquals(new Position(8, 0), ExpressionEditor.findSourcePosition(source, resolved, 7));

    // Between two variables, the source is underlined from the first one to the last one
    assertEquals(new Position(0, 9), ExpressionEditor.findSourcePosition("${A}+${B}", "1+abc", 1));
  }

  @Test
  void shortcutModifier() {
    assertTrue(ExpressionEditor.isShortcutModifier(SWT.MOD1));
    assertTrue(ExpressionEditor.isShortcutModifier(SWT.MOD1 | SWT.SHIFT));
    assertFalse(ExpressionEditor.isShortcutModifier(0));
    assertFalse(ExpressionEditor.isShortcutModifier(SWT.SHIFT));
    // AltGr is Ctrl+Alt on Windows, it types a character
    assertFalse(ExpressionEditor.isShortcutModifier(SWT.CTRL | SWT.ALT));
    assertFalse(ExpressionEditor.isShortcutModifier(SWT.MOD1 | SWT.ALT));
  }

  @Test
  void treeFilter() {
    // The tree is filtered from three characters, in lower case and without the spaces around
    assertEquals("", ExpressionEditor.toTreeFilter(""));
    assertEquals("", ExpressionEditor.toTreeFilter("ab"));
    assertEquals("", ExpressionEditor.toTreeFilter("  ab  "));
    assertEquals("abc", ExpressionEditor.toTreeFilter("abc"));
    assertEquals("date", ExpressionEditor.toTreeFilter(" Date "));
  }

  @Test
  void treeMatch() {
    // Without filter, all the elements are shown
    assertTrue(ExpressionEditor.isTreeMatch("UPPER", ""));
    // The label contains the filter, ignoring case
    assertTrue(ExpressionEditor.isTreeMatch("ADD_DAYS", "day"));
    assertTrue(ExpressionEditor.isTreeMatch("NOW, CURRENT_TIMESTAMP", "now"));
    assertTrue(ExpressionEditor.isTreeMatch("Customer name", "name"));
    assertFalse(ExpressionEditor.isTreeMatch("UPPER", "low"));
  }
}
