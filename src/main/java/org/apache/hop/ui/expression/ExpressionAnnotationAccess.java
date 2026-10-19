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

import org.apache.hop.ui.core.gui.GuiResource;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationAccess;
import org.eclipse.jface.text.source.IAnnotationAccessExtension;
import org.eclipse.jface.text.source.ImageUtilities;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;

/** Gives access to the annotations of the expression editor, the errors of the expression. */
public class ExpressionAnnotationAccess implements IAnnotationAccess, IAnnotationAccessExtension {

  /** The type of the annotation of an error in the expression. */
  public static final String ERROR_TYPE = "org.hop.expression.error";

  /**
   * Checks if an annotation is an error in the expression.
   *
   * @param annotation the annotation
   * @return true if the annotation is an error
   */
  public static boolean isError(Annotation annotation) {
    return ERROR_TYPE.equals(annotation.getType());
  }

  @Override
  public String getTypeLabel(Annotation annotation) {
    // The label of the type, the message is the text of the annotation
    return isError(annotation) ? "Error" : null;
  }

  @Override
  public int getLayer(Annotation annotation) {
    return DEFAULT_LAYER;
  }

  @Override
  public void paint(Annotation annotation, GC gc, Canvas canvas, Rectangle bounds) {
    if (isError(annotation)) {
      // Get the image when painting, the images of GuiResource can be created again (zoom, theme)
      Image image = GuiResource.getInstance().getImageError();
      ImageUtilities.drawImage(image, gc, canvas, bounds, SWT.CENTER);
    }
  }

  @Override
  public boolean isPaintable(Annotation annotation) {
    return isError(annotation);
  }

  @Override
  public boolean isSubtype(Object annotationType, Object potentialSupertype) {
    // A type has no super type, it is only a subtype of itself
    return annotationType != null && annotationType.equals(potentialSupertype);
  }

  @Override
  public Object[] getSupertypes(Object annotationType) {
    return new Object[0];
  }

  @Override
  public Object getType(Annotation annotation) {
    return annotation.getType();
  }

  @Override
  public boolean isMultiLine(Annotation annotation) {
    // An error is shown on the line where it starts
    return false;
  }

  @Override
  public boolean isTemporary(Annotation annotation) {
    return !annotation.isPersistent();
  }
}
