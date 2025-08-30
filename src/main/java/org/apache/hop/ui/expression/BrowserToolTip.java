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

import lombok.Getter;
import lombok.Setter;
import org.apache.hop.core.util.Utils;
import org.eclipse.jface.window.ToolTip;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;

/** Displays HTML information in a {@link Browser} widget. */
public class BrowserToolTip extends ToolTip {
  /** Minimal size constraints. */
  private static final int MIN_WIDTH = 800;

  private static final int MIN_HEIGHT = 600;

  private static final int DEFAULT_SHIFT_X = -3;
  private static final int DEFAULT_SHIFT_Y = -3;

  /** HTML text to display. */
  @Getter @Setter private String text;

  public BrowserToolTip(Control control) {
    super(control, ToolTip.RECREATE, true);
    this.setShift(new Point(DEFAULT_SHIFT_X, DEFAULT_SHIFT_Y));
    this.setRespectMonitorBounds(true);
    this.setRespectDisplayBounds(true);
    this.setHideOnMouseDown(true);
  }

  @Override
  protected Composite createToolTipContentArea(Event event, Composite parent) {

    if (parent instanceof Shell shell) {
      shell.setMinimumSize(MIN_WIDTH, MIN_HEIGHT);
    }

    // Create the widget browser
    Browser browser = new Browser(parent, SWT.NONE);

    // Cancel opening of new windows
    browser.addOpenWindowListener(e -> e.required = true);

    // Replace the browser's built-in context menu with none
    browser.setMenu(new Menu(parent.getShell(), SWT.NONE));

    // Set HTML content
    browser.setText(text);

    return browser;
  }

  @Override
  protected boolean shouldCreateToolTip(Event event) {
    return !Utils.isEmpty(text);
  }
}
