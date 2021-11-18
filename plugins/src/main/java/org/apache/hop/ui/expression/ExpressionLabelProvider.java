/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.ui.expression;

import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.operator.Function;
import org.apache.hop.ui.core.ConstUi;
import org.apache.hop.ui.core.gui.GuiResource;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IToolTipProvider;
import org.eclipse.swt.graphics.Image;

public class ExpressionLabelProvider implements ILabelProvider, IToolTipProvider {

  private Image imageVariable;

  public ExpressionLabelProvider() {
    super();
    imageVariable = GuiResource.getInstance().getImage("ui/images/variable.svg", ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE);
  }

  @Override
  public void addListener(ILabelProviderListener var1) {}

  @Override
  public void dispose() {
    if (imageVariable != null) imageVariable.dispose();
  }

  @Override
  public boolean isLabelProperty(Object var1, String var2) {
    return false;
  }

  @Override
  public void removeListener(ILabelProviderListener var1) {}

  @Override
  public Image getImage(Object element) {

    if (element instanceof String) {
      return this.imageVariable;
    }

    if (element instanceof Function) {
      return GuiResource.getInstance().getImageFunction();
    }

    if (element instanceof IValueMeta) {
      IValueMeta valueMeta = (IValueMeta) element;
      return GuiResource.getInstance().getImage(valueMeta);
    }

    return null;
  }
    
  @Override
  public String getText(Object element) {
    if (element instanceof Operator) {
      return ((Operator) element).getName();
    }

    if (element instanceof IValueMeta) {
      return ((IValueMeta) element).getName();
    }
    
    return String.valueOf(element);
  }

  @Override
  public String getToolTipText(Object element) {
    if (element instanceof Operator) {
      Operator operator = (Operator) element;
      return Operator.getHtml(operator.getName());
    }

    return null;
  }
}
