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

import org.eclipse.jface.text.AbstractInformationControl;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;

/**
 * Displays HTML information in a {@link org.eclipse.swt.browser.Browser} widget.
 */
public class BrowserInformationControl extends AbstractInformationControl  {


  /**
   * Minimal size constraints.
  */
  private static final int MIN_WIDTH= 80;

  private static final int MIN_HEIGHT= 50;
  
  /** The control's browser widget */
  private Browser fBrowser;


  public BrowserInformationControl(Shell parentShell, boolean isResizable) {
    super(parentShell, isResizable);
  }


  @Override
  public boolean hasContents() {
    return false;
  }



  @Override
  protected void createContent(Composite parent) {
      fBrowser= new Browser(parent, SWT.NONE);
      fBrowser.setJavascriptEnabled(false);

      Display display= getShell().getDisplay();
      fBrowser.setForeground(display.getSystemColor(SWT.COLOR_INFO_FOREGROUND));
      fBrowser.setBackground(display.getSystemColor(SWT.COLOR_INFO_BACKGROUND));

      // Cancel opening of new windows
      fBrowser.addOpenWindowListener(event -> event.required=true);

      // Replace browser's built-in context menu with none
      fBrowser.setMenu(new Menu(getShell(), SWT.NONE));
  }

}
