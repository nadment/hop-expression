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
import static org.junit.jupiter.api.Assertions.assertNull;

import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;
import org.apache.hop.expression.ExpressionExtension;
import org.apache.hop.expression.Function;
import org.apache.hop.junit.rules.RestoreHopEnvironmentExtension;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith({RestoreHopEnvironmentExtension.class, ExpressionExtension.class})
class FunctionHyperlinkDetectorTest {

  private final List<Function> opened = new ArrayList<>();
  private final FunctionHyperlinkDetector detector = new FunctionHyperlinkDetector(opened::add);

  /**
   * A partitioner with a partition of another type from an offset to the end, like a string or a
   * comment of the editor. The partitioner of the editor needs the Eclipse platform.
   */
  private static IDocumentPartitioner partitioner(int otherFrom, String otherType) {
    return (IDocumentPartitioner)
        Proxy.newProxyInstance(
            IDocumentPartitioner.class.getClassLoader(),
            new Class<?>[] {IDocumentPartitioner.class},
            (proxy, method, args) ->
                switch (method.getName()) {
                  case "getContentType" ->
                      ((int) args[0] >= otherFrom) ? otherType : IDocument.DEFAULT_CONTENT_TYPE;
                  case "getLegalContentTypes" ->
                      new String[] {IDocument.DEFAULT_CONTENT_TYPE, otherType};
                  case "documentChanged" -> false;
                  default -> null;
                });
  }

  /** A viewer of the source, the detector only uses getDocument. */
  private static ITextViewer viewer(IDocument document) {
    return (ITextViewer)
        Proxy.newProxyInstance(
            ITextViewer.class.getClassLoader(),
            new Class<?>[] {ITextViewer.class},
            (proxy, method, args) -> "getDocument".equals(method.getName()) ? document : null);
  }

  private IHyperlink[] detect(String source, int offset) {
    return detector.detectHyperlinks(viewer(new Document(source)), new Region(offset, 0), false);
  }

  /** Detects in a source with a string or a comment from an offset to the end. */
  private IHyperlink[] detect(String source, int offset, int otherFrom, String otherType) {
    IDocument document = new Document(source);
    document.setDocumentPartitioner(partitioner(otherFrom, otherType));
    return detector.detectHyperlinks(viewer(document), new Region(offset, 0), false);
  }

  @Test
  void function() {
    IHyperlink[] links = detect("1 + UPPER(NAME)", 6);
    assertEquals(1, links.length);
    assertEquals(4, links[0].getHyperlinkRegion().getOffset());
    assertEquals(5, links[0].getHyperlinkRegion().getLength());
    assertEquals("UPPER", links[0].getHyperlinkText());

    // The link opens the documentation of the function
    links[0].open();
    assertEquals("UPPER", opened.get(0).getName());

    // The name of a function is not case-sensitive
    assertEquals(1, detect("upper(NAME)", 0).length);
  }

  @Test
  void notFunction() {
    // A word not followed by a parenthesis is a field, like YEAR
    assertNull(detect("UPPER || NAME", 1));
    assertNull(detect("UPPER (NAME)", 1));
    // Not a function, or not a word
    assertNull(detect("NOPE(NAME)", 1));
    assertNull(detect("1 + UPPER(NAME)", 2));
    // A word in a string or a comment
    assertNull(detect("1 + 'UPPER(NAME)'", 6, 4, ExpressionPartitionScanner.STRING));
    assertNull(detect("1 // UPPER(NAME)", 6, 2, ExpressionPartitionScanner.COMMENT));
    // The same function before the string is detected
    assertEquals(1, detect("UPPER('A') || 'B'", 1, 14, ExpressionPartitionScanner.STRING).length);
  }
}
