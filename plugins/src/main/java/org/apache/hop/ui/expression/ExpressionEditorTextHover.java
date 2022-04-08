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
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextHoverExtension;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.widgets.Shell;

/**
 * Displays HTML information in a {@link org.eclipse.swt.browser.Browser} widget.
 */
public class ExpressionEditorTextHover implements ITextHover, ITextHoverExtension {

  public ExpressionEditorTextHover() {
    super();
  }

  /**
   * Minimal size constraints.
   */
  private static final int MIN_WIDTH = 80;

  private static final int MIN_HEIGHT = 50;

  /** The control's browser widget */
  private Browser fBrowser;


  @Override
  public String getHoverInfo(ITextViewer textViewer, IRegion region) {

    try {
      String text = textViewer.getDocument().get(region.getOffset(), region.getLength());

      // If hover identifier, display

      // If hover function, display description
      Function function = FunctionRegistry.getFunction(text);
      if (function != null)
        return function.getDescription();

      return null;
    } catch (BadLocationException e) {
    }

    return null;
  }
  
  @Override
  public IRegion getHoverRegion(ITextViewer textViewer, int offset) {

    int start = -2;
    int end = -1;
    IDocument document = textViewer.getDocument();
    try {
      int pos = offset;
      char c;
      while (pos >= 0) {
        c = document.getChar(pos);
        if (!Character.isUnicodeIdentifierPart(c))
          break;
        --pos;
      }
      start = pos;

      pos = offset;
      int length = document.getLength();

      while (pos < length) {
        c = document.getChar(pos);
        if (!Character.isUnicodeIdentifierPart(c))
          break;
        ++pos;
      }
      end = pos;
    } catch (BadLocationException x) {
    }

    // if a mouse is hovered over an invalid word
    if (start >= -1 && end > -1) {
      // if hovered over an one-letter identifier
      if (start == offset && end == offset)
        return new Region(offset, 0);
      else if (start == offset)
        return new Region(start, end - start);
      else
        return new Region(start + 1, end - start - 1);
    }

    return null;
  }

  @Override
  public IInformationControlCreator getHoverControlCreator() {
    return new IInformationControlCreator() {
      public IInformationControl createInformationControl(Shell parent) {
        return new DefaultInformationControl(parent);

      }
    };
  }
}
