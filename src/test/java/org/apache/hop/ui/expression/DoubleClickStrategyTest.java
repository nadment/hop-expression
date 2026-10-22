/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.ui.expression;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Proxy;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.swt.graphics.Point;
import org.junit.jupiter.api.Test;

class DoubleClickStrategyTest {

  /**
   * Double-clicks a position of a text.
   *
   * @param text the text
   * @param position the position of the double-click
   * @return the selected text
   */
  private static String doubleClick(String text, int position) {
    Document document = new Document(text);
    Point selection = new Point(position, 0);

    // A viewer limited to the document and the selection
    ITextViewer viewer =
        (ITextViewer)
            Proxy.newProxyInstance(
                ITextViewer.class.getClassLoader(),
                new Class<?>[] {ITextViewer.class},
                (proxy, method, args) ->
                    switch (method.getName()) {
                      case "getDocument" -> document;
                      case "getSelectedRange" -> new Point(selection.x, selection.y);
                      case "setSelectedRange" -> {
                        selection.x = (int) args[0];
                        selection.y = (int) args[1];
                        yield null;
                      }
                      default -> throw new UnsupportedOperationException(method.getName());
                    });

    new DoubleClickStrategy().doubleClicked(viewer);
    return text.substring(selection.x, selection.x + selection.y);
  }

  @Test
  void selectWord() {
    // The first word of the expression is selected with its first character
    assertEquals("ABS", doubleClick("ABS(-1)", 0));
    assertEquals("ABS", doubleClick("ABS(-1)", 2));
    assertEquals("FIELD", doubleClick("1 + FIELD", 6));
    assertEquals("FIELD", doubleClick("1 + FIELD", 4));
    assertEquals("", doubleClick("1 + 2", 2));
  }

  @Test
  void selectVariable() {
    assertEquals("${VAR}", doubleClick("1 + ${VAR} + 2", 7));
    assertEquals("${VAR}", doubleClick("${VAR}", 0));

    // A '$' without '{' is not a variable, the word is selected without the '}'
    assertEquals("$ab", doubleClick("$ab}", 2));

    // A variable does not continue on the next line
    assertEquals("VAR", doubleClick("${\nVAR}", 4));
  }

  @Test
  void selectIdentifier() {
    assertEquals("[FIELD 1]", doubleClick("1 + [FIELD 1]", 6));

    // An identifier does not continue on the next line
    assertEquals("FIELD", doubleClick("[\nFIELD]", 3));
  }
}
