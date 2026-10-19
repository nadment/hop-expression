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

import java.util.Iterator;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionRegistry;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextHoverExtension;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;

public class ExpressionTextHover implements ITextHover, ITextHoverExtension {

  @Override
  public String getHoverInfo(ITextViewer textViewer, IRegion region) {
    // If hover an error, display its message
    String error = findErrorMessage(textViewer, region);
    if (error != null) {
      return error;
    }

    IDocument document = textViewer.getDocument();
    try {
      // If hover identifier, display

      // If hover function, display description. As in the lexer, a word is a function only if it
      // is immediately followed by a left parenthesis, otherwise it is a field like "YEAR".
      int end = region.getOffset() + region.getLength();
      if (region.getLength() == 0 || end >= document.getLength() || document.getChar(end) != '(') {
        return null;
      }

      String text = document.get(region.getOffset(), region.getLength());
      Function function = FunctionRegistry.getFunction(text);
      if (function != null) return function.getDescription();
    } catch (BadLocationException e) {
      // The document changed, no hover
    }

    return null;
  }

  /**
   * Finds the message of an error annotation that overlaps the hovered region.
   *
   * @param textViewer the viewer
   * @param region the hovered region
   * @return the message of the error, or null if there is no error
   */
  private static String findErrorMessage(ITextViewer textViewer, IRegion region) {
    if (!(textViewer instanceof ISourceViewer sourceViewer)) return null;
    IAnnotationModel model = sourceViewer.getAnnotationModel();
    if (model == null) return null;

    // An empty region is over a character, like an operator
    int length = Math.max(region.getLength(), 1);
    Iterator<Annotation> iterator = model.getAnnotationIterator();
    while (iterator.hasNext()) {
      Annotation annotation = iterator.next();
      Position position = model.getPosition(annotation);
      if (ExpressionAnnotationAccess.isError(annotation)
          && position != null
          && position.overlapsWith(region.getOffset(), length)) {
        return annotation.getText();
      }
    }
    return null;
  }

  @Override
  public IRegion getHoverRegion(ITextViewer textViewer, int offset) {
    try {
      return findWordRegion(textViewer.getDocument(), offset);
    } catch (BadLocationException e) {
      // The document changed, no hover
      return null;
    }
  }

  /**
   * Finds the word at an offset of a document.
   *
   * @param document the document
   * @param offset the offset
   * @return the region of the word, or an empty region at the offset if it is not over a word, for
   *     example a space or an operator
   * @throws BadLocationException if the offset is out of the document
   */
  static IRegion findWordRegion(IDocument document, int offset) throws BadLocationException {
    // Not over a word, for example a space or an operator
    if (offset >= document.getLength()
        || !Character.isUnicodeIdentifierPart(document.getChar(offset))) {
      return new Region(offset, 0);
    }

    // Expand to the start and the end of the word
    int start = offset;
    while (start > 0 && Character.isUnicodeIdentifierPart(document.getChar(start - 1))) {
      start--;
    }
    int end = offset + 1;
    while (end < document.getLength() && Character.isUnicodeIdentifierPart(document.getChar(end))) {
      end++;
    }
    return new Region(start, end - start);
  }

  @Override
  public IInformationControlCreator getHoverControlCreator() {
    return DefaultInformationControl::new;
  }
}
