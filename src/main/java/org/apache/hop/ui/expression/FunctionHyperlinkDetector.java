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
package org.apache.hop.ui.expression;

import java.util.function.Consumer;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionRegistry;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.TextUtilities;
import org.eclipse.jface.text.hyperlink.AbstractHyperlinkDetector;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.jspecify.annotations.Nullable;

/**
 * Detects the name of a function, Ctrl+click opens its documentation.
 *
 * <p>As in the lexer, a word is a function only if it is immediately followed by a left
 * parenthesis, otherwise it is a field like "YEAR". A word in a string or a comment is not a
 * function.
 */
public class FunctionHyperlinkDetector extends AbstractHyperlinkDetector {

  private final Consumer<Function> opener;

  /**
   * Creates the detector.
   *
   * @param opener opens the documentation of a function
   */
  public FunctionHyperlinkDetector(Consumer<Function> opener) {
    this.opener = opener;
  }

  @Override
  public IHyperlink @Nullable [] detectHyperlinks(
      ITextViewer textViewer, IRegion region, boolean canShowMultipleHyperlinks) {
    IDocument document = textViewer.getDocument();
    if (document == null) {
      return null;
    }
    try {
      int offset = region.getOffset();
      String contentType =
          TextUtilities.getContentType(
              document, IDocumentExtension3.DEFAULT_PARTITIONING, offset, false);
      if (!IDocument.DEFAULT_CONTENT_TYPE.equals(contentType)) {
        return null;
      }

      IRegion word = ExpressionTextHover.findWordRegion(document, offset);
      int end = word.getOffset() + word.getLength();
      if (word.getLength() == 0 || end >= document.getLength() || document.getChar(end) != '(') {
        return null;
      }

      Function function =
          FunctionRegistry.getFunction(document.get(word.getOffset(), word.getLength()));
      if (function == null) {
        return null;
      }
      return new IHyperlink[] {new FunctionHyperlink(word, function, opener)};
    } catch (BadLocationException e) {
      // The document changed, no link
      return null;
    }
  }

  /** The link of a function name, it opens the documentation of the function. */
  private record FunctionHyperlink(IRegion region, Function function, Consumer<Function> opener)
      implements IHyperlink {

    @Override
    public IRegion getHyperlinkRegion() {
      return region;
    }

    @Override
    public @Nullable String getTypeLabel() {
      return null;
    }

    @Override
    public String getHyperlinkText() {
      return function.getName();
    }

    @Override
    public void open() {
      opener.accept(function);
    }
  }
}
