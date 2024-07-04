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
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;

public class ExpressionAnnotationAccess implements IAnnotationAccess, IAnnotationAccessExtension {

  private static Image image = GuiResource.getInstance().getImageError();

  @Override
  public String getTypeLabel(Annotation annotation) {
    return annotation.getText();
  }

  @Override
  public int getLayer(Annotation annotation) {
    return DEFAULT_LAYER;
  }

  @Override
  public void paint(Annotation annotation, GC gc, Canvas canvas, Rectangle bounds) {
    gc.drawImage(image, bounds.x, bounds.y);
  }

  @Override
  public boolean isPaintable(Annotation annotation) {
    return true;
  }

  @Override
  public boolean isSubtype(Object annotationType, Object potentialSupertype) {
    return true;
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
    return true;
  }

  @Override
  public boolean isTemporary(Annotation annotation) {
    return !annotation.isPersistent();
  }
}
