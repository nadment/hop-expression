/*******************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 ******************************************************************************/

package org.apache.hop.ui.trans.steps.where;

import org.apache.hop.core.annotations.PluginDialog;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.row.RowMetaInterface;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.trans.TransMeta;
import org.apache.hop.trans.step.StepMeta;
import org.apache.hop.trans.steps.where.WhereMeta;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.expression.ExpressionEditor;
import org.apache.hop.ui.trans.steps.expression.AbstractStepDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

@PluginDialog(id = "ExpressionFilter", image = "ExpressionFilter.svg", pluginType = PluginDialog.PluginType.STEP)
public class WhereDialog extends AbstractStepDialog<WhereMeta> {

	private static Class<?> PKG = WhereMeta.class; // for i18n purposes

	private ExpressionEditor wEditor;

	/**
	 * Constructor that saves incoming meta object to a local variable, so it can
	 * conveniently read and write settings from/to it.
	 *
	 * @param parent    the SWT shell to open the dialog in
	 * @param in        the meta object holding the step's settings
	 * @param transMeta transformation description
	 * @param stepName  the step name
	 */
	public WhereDialog(Shell parent, Object in, TransMeta transMeta, String stepName) {
		super(parent, in, transMeta, stepName);

		this.setText(BaseMessages.getString(PKG, "WhereDialog.Shell.Title"));
	}

	@Override
	protected void loadMeta(final WhereMeta meta) {

		if (meta.getExpression() != null) {
			this.wEditor.setText(meta.getExpression());
		}
	}

	@Override
	public Point getMinimumSize() {
		return new Point(600, 400);
	}

	@Override
	protected void saveMeta(final WhereMeta meta) {

		// save step name
		stepname = wStepname.getText();

		meta.setExpression(this.wEditor.getText());
	}

	@Override
	protected Control createDialogArea(final Composite parent) {

		wEditor = new ExpressionEditor(parent, SWT.NONE);
		wEditor.setVariables(this.transMeta);
		wEditor.setLayoutData(new FormDataBuilder().top().fullWidth().bottom().result());

		// Search the fields in the background
		new Thread(() -> {
			try {
				StepMeta stepMeta = transMeta.findStep(stepname);
				if (stepMeta != null) {
					RowMetaInterface rowMeta = transMeta.getPrevStepFields(stepMeta);
					wEditor.setRowMeta(rowMeta);
				}
			} catch (HopException e) {
				logError(BaseMessages.getString(PKG, "System.Dialog.GetFieldsFailed.Message"), e);
			}
		}).start();

		return parent;
	}
}