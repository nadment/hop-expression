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
import org.eclipse.jface.text.Region;

public class ExpressionTextHover implements ITextHover, ITextHoverExtension {

  @Override
  public String getHoverInfo(ITextViewer textViewer, IRegion region) {
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

  @Override
  public IRegion getHoverRegion(ITextViewer textViewer, int offset) {
    IDocument document = textViewer.getDocument();
    try {
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
      while (end < document.getLength()
          && Character.isUnicodeIdentifierPart(document.getChar(end))) {
        end++;
      }
      return new Region(start, end - start);
    } catch (BadLocationException e) {
      // The document changed, no hover
      return null;
    }
  }

  @Override
  public IInformationControlCreator getHoverControlCreator() {
    return DefaultInformationControl::new;
  }
}
