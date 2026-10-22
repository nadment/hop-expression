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

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.apache.hop.core.Const;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.variables.DescribedVariable;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.Operator;
import org.apache.hop.ui.core.gui.GuiResource;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IToolTipProvider;
import org.eclipse.swt.graphics.Image;

public class ExpressionLabelProvider implements ILabelProvider, IToolTipProvider {

  private String css;

  private String getCss() {
    if (css == null) {
      try (InputStream is = this.getClass().getResourceAsStream("/docs/expression.css")) {
        css = (is == null) ? "" : new String(is.readAllBytes(), StandardCharsets.UTF_8);
      } catch (IOException e) {
        css = "";
      }
    }
    return css;
  }

  /**
   * Starts the help page of an element, with its title.
   *
   * @param title the title, escaped
   * @return the builder of the page
   */
  private StringBuilder beginHtml(String title) {
    StringBuilder builder = new StringBuilder();
    builder.append("<html><style>");
    builder.append(getCss());
    builder.append("</style><body class=\"article\">");
    builder.append("<div id=\"header\"><h1>");
    builder.append(StringEscapeUtils.escapeHtml4(title));
    builder.append("</h1></div><div id=\"content\">");
    return builder;
  }

  /**
   * Appends a section to the help page.
   *
   * @param builder the builder of the page
   * @param heading the heading of the section
   * @param text the text of the section, escaped because it can contain characters like '&lt;'
   */
  private static void appendSection(StringBuilder builder, String heading, String text) {
    builder.append("<h2>").append(heading).append("</h2>");
    builder.append(StringEscapeUtils.escapeHtml4(text));
  }

  /**
   * Ends the help page.
   *
   * @param builder the builder of the page
   * @return the page
   */
  private static String endHtml(StringBuilder builder) {
    // Close the content
    builder.append("</div></body></html>");
    return builder.toString();
  }

  @Override
  public void addListener(ILabelProviderListener listener) {
    // The labels never change, no listener to notify
  }

  @Override
  public boolean isLabelProperty(Object element, String property) {
    return false;
  }

  @Override
  public void removeListener(ILabelProviderListener listener) {
    // The labels never change, no listener to notify
  }

  @Override
  public Image getImage(Object element) {

    if (element instanceof DescribedVariable) {
      return GuiResource.getInstance().getImageVariable();
    }

    if (element instanceof Function) {
      return GuiResource.getInstance().getImageFunction();
    }

    if (element instanceof IValueMeta meta) {
      return GuiResource.getInstance().getImage(meta);
    }

    return null;
  }

  @Override
  public String getText(Object element) {
    if (element instanceof Operator operator) {
      return operator.getName();
    }

    if (element instanceof DescribedVariable variable) {
      return variable.getName();
    }

    if (element instanceof IValueMeta valueMeta) {
      return valueMeta.getName();
    }

    return String.valueOf(element);
  }

  @Override
  public String getToolTipText(Object element) {
    if (element instanceof Operator operator) {
      return operator.getDocumentation();
    }

    if (element instanceof DescribedVariable variable) {
      // Header
      StringBuilder builder = beginHtml(variable.getName());

      // Content
      appendSection(builder, "Description", Const.NVL(variable.getDescription(), "-"));

      // Footer
      return endHtml(builder);
    }

    if (element instanceof IValueMeta meta) {
      // Header
      StringBuilder builder = beginHtml(meta.getName());

      // Content
      appendSection(builder, "Type", meta.getTypeDesc());
      appendSection(builder, "Transform origin", Const.NVL(meta.getOrigin(), "-"));
      appendSection(builder, "Comment", StringUtils.defaultString(meta.getComments()));

      // Footer
      return endHtml(builder);
    }

    return null;
  }

  @Override
  public void dispose() {
    // No resource to dispose
  }
}
