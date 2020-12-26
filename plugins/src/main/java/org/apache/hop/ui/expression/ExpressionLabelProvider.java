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

import java.util.Arrays;
import java.util.List;

import org.apache.hop.core.exception.HopPluginException;
import org.apache.hop.core.plugins.IPlugin;
import org.apache.hop.core.plugins.PluginRegistry;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaPluginType;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.Operator;
import org.apache.hop.ui.core.ConstUi;
import org.apache.hop.ui.util.SwtSvgImageUtil;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IToolTipProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

public class ExpressionLabelProvider implements ILabelProvider, IToolTipProvider {

  private Image imageFunction;
  private Image imageVariable;
  private Image[] imageValueMeta;

  public ExpressionLabelProvider() {
    super();

    imageFunction =
        SwtSvgImageUtil.getImage(
            Display.getCurrent(), getClass().getClassLoader(), "function.svg", ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE);
    imageVariable =
        SwtSvgImageUtil.getImage(
            Display.getCurrent(), getClass().getClassLoader(), "variable.svg", ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE);

    // Load image from plugin ValueMeta
    imageValueMeta = new Image[0];
    PluginRegistry registry = PluginRegistry.getInstance();
    List<IPlugin> plugins = registry.getPlugins(ValueMetaPluginType.class);
    for (IPlugin plugin : plugins) {
      try {
        IValueMeta meta = (IValueMeta) registry.loadClass(plugin);

        String file = plugin.getImageFile();
        Image image =
            SwtSvgImageUtil.getImage(
                Display.getCurrent(), getClass().getClassLoader(), file, ConstUi.SMALL_ICON_SIZE, ConstUi.SMALL_ICON_SIZE);

        if (imageValueMeta.length < meta.getType()) {
          imageValueMeta = Arrays.copyOf(imageValueMeta, meta.getType() + 1);
        }

        imageValueMeta[meta.getType()] = image;
      } catch (HopPluginException e) {
        // Ignore
      }
    }
  }

  @Override
  public void addListener(ILabelProviderListener var1) {}

  @Override
  public void dispose() {
    if (imageFunction != null) imageFunction.dispose();
    if (imageVariable != null) imageVariable.dispose();
    for (Image image : imageValueMeta) {
      if (image != null) image.dispose();
    }
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

    if (element instanceof ExpressionProposal) {
      ExpressionProposal proposal = (ExpressionProposal) element;

      switch (proposal.getType()) {
        case Function:
        case Field:
          element = proposal.getData();
          break;
        case Variable:
          return imageVariable;
        default:
          break;
      }
    }

    if (element instanceof Function) {
      return this.imageFunction;
    }

    if (element instanceof IValueMeta) {
      IValueMeta valueMeta = (IValueMeta) element;
      return imageValueMeta[valueMeta.getType()];
    }

    return null;
  }

  @Override
  public String getText(Object element) {
    if (element instanceof Operator) {
      return ((Operator) element).getName();
    }

    if (element instanceof ExpressionProposal) {
      ExpressionProposal proposal = (ExpressionProposal) element;
      return proposal.getLabel();
    }
    return String.valueOf(element);
  }

  @Override
  public String getToolTipText(Object element) {
    if (element instanceof Operator) {
      Operator operator = (Operator) element;

      return Operator.getHtmlDocumentation(operator.getKind());
    }

    return null;
  }
}
