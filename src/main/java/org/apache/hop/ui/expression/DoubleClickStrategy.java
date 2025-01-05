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

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextViewer;

public class DoubleClickStrategy implements ITextDoubleClickStrategy {

  public void doubleClicked(ITextViewer viewer) {
    int position = viewer.getSelectedRange().x;

    if (position < 0) return;

    if (!selectVariable(viewer, position) && !selectIdentifier(viewer, position)) {
      selectWord(viewer, position);
    }
  }

  protected boolean selectVariable(ITextViewer viewer, int position) {
    IDocument document = viewer.getDocument();

    try {
      int start = position;
      while (start >= 0) {
        char c = document.getChar(start);
        if (c == Character.LINE_SEPARATOR || c == '}') return false;
        if (c == '$') break;
        start--;
      }

      if (document.getChar(start) != '$' && document.getChar(start + 1) != '{') return false;

      int end = position;
      int length = document.getLength();
      while (end < length) {
        char c = document.getChar(end);
        if (c == Character.LINE_SEPARATOR || c == '$') return false;
        if (c == '}') break;
        end++;
      }
      if (document.getChar(end) != '}') return false;

      viewer.setSelectedRange(start, end - start + 1);
    } catch (BadLocationException x) {
      return false;
    }
    return true;
  }

  protected boolean selectIdentifier(ITextViewer viewer, int position) {
    IDocument document = viewer.getDocument();

    try {
      int start = position;
      while (start >= 0) {
        char c = document.getChar(start);
        if (c == Character.LINE_SEPARATOR || c == ']') return false;
        if (c == '[') break;
        start--;
      }

      if (document.getChar(start) != '[') return false;

      int end = position;
      int length = document.getLength();
      while (end < length) {
        char c = document.getChar(end);
        if (c == Character.LINE_SEPARATOR || c == '[') return false;
        if (c == ']') break;
        end++;
      }
      if (document.getChar(end) != ']') return false;

      viewer.setSelectedRange(start, end - start + 1);
    } catch (BadLocationException x) {
      return false;
    }
    return true;
  }

  protected boolean selectWord(ITextViewer viewer, int position) {
    IDocument document = viewer.getDocument();
    try {
      int start = position;
      while (start > 0) {
        char c = document.getChar(start);
        if (!Character.isJavaIdentifierPart(c)) break;
        start--;
      }

      int end = position;
      int length = document.getLength();
      while (end < length) {
        char c = document.getChar(end);
        if (!Character.isJavaIdentifierPart(c)) break;
        end++;
      }

      viewer.setSelectedRange(start + 1, end - start - 1);

    } catch (BadLocationException x) {
      return false;
    }

    return true;
  }
}
