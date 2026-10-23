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
import java.util.function.Supplier;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.DescribedVariable;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.VariableRegistry;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.type.Types;
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
import org.jspecify.annotations.Nullable;

/**
 * Shows the message of an error, the description of a function, the value of a variable or the type
 * of a field under the mouse.
 */
public class ExpressionTextHover implements ITextHover, ITextHoverExtension {

  private final @Nullable IVariables variables;
  private final Supplier<@Nullable IRowMeta> rowMeta;
  private final boolean variablesOnly;

  /**
   * Creates the hover of a partition of the expression.
   *
   * @param variables the variables, or null
   * @param rowMeta the supplier of the fields, which returns null while they are unknown
   * @param variablesOnly true for a string, where only the variables are shown
   */
  public ExpressionTextHover(
      @Nullable IVariables variables, Supplier<@Nullable IRowMeta> rowMeta, boolean variablesOnly) {
    this.variables = variables;
    this.rowMeta = rowMeta;
    this.variablesOnly = variablesOnly;
  }

  @Override
  public String getHoverInfo(ITextViewer textViewer, IRegion region) {
    // If hover an error, display its message
    String error = findErrorMessage(textViewer, region);
    if (error != null) {
      return error;
    }

    IDocument document = textViewer.getDocument();
    try {
      String text = document.get(region.getOffset(), region.getLength());

      // If hover a variable, display its value
      if (text.startsWith("${") && text.endsWith("}")) {
        return getVariableInfo(text.substring(2, text.length() - 1));
      }
      if (variablesOnly) {
        return null;
      }

      // If hover a quoted identifier, display the field
      if (text.length() > 1 && text.startsWith("\"") && text.endsWith("\"")) {
        return getFieldInfo(text.substring(1, text.length() - 1));
      }

      // If hover function, display description. As in the lexer, a word is a function only if it
      // is immediately followed by a left parenthesis, otherwise it is a field like "YEAR".
      if (text.isEmpty()) {
        return null;
      }
      int end = region.getOffset() + region.getLength();
      if (end < document.getLength() && document.getChar(end) == '(') {
        Function function = FunctionRegistry.getFunction(text);
        return (function == null) ? null : function.getDescription();
      }

      // If hover identifier, display the field
      return getFieldInfo(text);
    } catch (BadLocationException e) {
      // The document changed, no hover
    }

    return null;
  }

  /**
   * Gets the value and the description of a variable.
   *
   * @param name the name of the variable
   * @return the information, or null if the variables are unknown
   */
  @Nullable String getVariableInfo(String name) {
    if (variables == null) {
      return null;
    }
    String value = variables.getVariable(name);
    StringBuilder info = new StringBuilder("${").append(name).append('}');
    info.append((value == null) ? " is not defined" : " = " + value);
    // The registry of the described variables is created with the environment of Hop
    VariableRegistry registry = VariableRegistry.getInstance();
    DescribedVariable variable = (registry == null) ? null : registry.findDescribedVariable(name);
    if (variable != null && !Utils.isEmpty(variable.getDescription())) {
      info.append('\n').append(variable.getDescription());
    }
    return info.toString();
  }

  /**
   * Gets the type and the origin of a field, the type is the data type of the field in an
   * expression, like STRING(50).
   *
   * @param name the name of the field
   * @return the information, or null if the fields are unknown or the name is not a field
   */
  @Nullable String getFieldInfo(String name) {
    IRowMeta fields = rowMeta.get();
    if (fields == null) {
      return null;
    }
    IValueMeta valueMeta = fields.searchValueMeta(name);
    if (valueMeta == null) {
      return null;
    }
    StringBuilder info = new StringBuilder(valueMeta.getName()).append(": ");
    try {
      info.append(Types.createType(valueMeta));
    } catch (ExpressionException e) {
      // A type of Hop that is not supported by the expressions
      info.append(valueMeta.getTypeDesc());
    }
    if (!Utils.isEmpty(valueMeta.getOrigin())) {
      info.append("\nOrigin: ").append(valueMeta.getOrigin());
    }
    return info.toString();
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
      IDocument document = textViewer.getDocument();
      IRegion variable = findEnclosedRegion(document, offset, "${", '}');
      if (variable != null || variablesOnly) {
        return variable;
      }
      IRegion quoted = findEnclosedRegion(document, offset, "\"", '"');
      if (quoted != null) {
        return quoted;
      }
      return findWordRegion(document, offset);
    } catch (BadLocationException e) {
      // The document changed, no hover
      return null;
    }
  }

  /**
   * Finds the region of a text enclosed by a start and an end on the line of an offset, like a
   * variable <code>${NAME}</code> or a quoted identifier <code>"NAME"</code>.
   *
   * @param document the document
   * @param offset the offset, inside the region or on its start or end
   * @param start the start of the region
   * @param end the end of the region
   * @return the region with its start and its end, or null if the offset is not enclosed
   * @throws BadLocationException if the offset is out of the document
   */
  static @Nullable IRegion findEnclosedRegion(
      IDocument document, int offset, String start, char end) throws BadLocationException {
    if (offset >= document.getLength()) {
      return null;
    }
    IRegion line = document.getLineInformationOfOffset(offset);
    String text = document.get(line.getOffset(), line.getLength());
    int index = offset - line.getOffset();

    // The last start before the offset, the offset can be on the first character of the start
    int from = text.lastIndexOf(start, index);
    if (from < 0) {
      return null;
    }
    // With the same start and end, like a quote, a quote after an odd number of quotes is a
    // closing quote: the region starts at the previous quote if the offset is on the closing quote
    if (start.length() == 1 && start.charAt(0) == end && countBefore(text, end, from) % 2 != 0) {
      if (from != index) {
        return null;
      }
      int open = text.lastIndexOf(end, from - 1);
      return new Region(line.getOffset() + open, from + 1 - open);
    }
    int to = text.indexOf(end, from + start.length());
    if (to < 0 || to < index) {
      return null;
    }
    return new Region(line.getOffset() + from, to + 1 - from);
  }

  /** Counts the occurrences of a character before an index. */
  private static int countBefore(String text, char c, int index) {
    int count = 0;
    for (int i = 0; i < index; i++) {
      if (text.charAt(i) == c) count++;
    }
    return count;
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
