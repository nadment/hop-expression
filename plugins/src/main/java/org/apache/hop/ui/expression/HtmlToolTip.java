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

import org.eclipse.jface.viewers.IToolTipProvider;
import org.eclipse.jface.window.ToolTip;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

public class HtmlToolTip extends ToolTip {

  private IToolTipProvider tooltipProvider;

  public HtmlToolTip(Tree control, IToolTipProvider provider) {
    super(control, ToolTip.NO_RECREATE, false);

    this.tooltipProvider = provider;

    this.setShift(new Point(-3, -3));
    this.setRespectMonitorBounds(true);
    this.setRespectDisplayBounds(true);
    this.setHideOnMouseDown(false);
        
    control.addListener(SWT.MouseHover,  e -> this.hide());
  }

  @Override
  protected Composite createToolTipContentArea(Event event, Composite parent) {

    Browser browser = new Browser(parent, SWT.NONE);
    browser.setSize(450, 400);

    String doc = this.getText(event);
    if (doc!=null) browser.setText(doc);

    return browser;
  }

  @Override
  protected boolean shouldCreateToolTip(Event event) {    
    return this.getText(event)!=null;
  }

  protected final String getText(Event event) {
    Tree tree = (Tree) event.widget;
    TreeItem item = tree.getItem(new Point(event.x, event.y));
    if (item != null) {
      return this.tooltipProvider.getToolTipText(item.getData());
    }

    return null;
  }
}
