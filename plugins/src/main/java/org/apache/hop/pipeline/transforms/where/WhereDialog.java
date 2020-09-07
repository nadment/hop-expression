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

package org.apache.hop.pipeline.transforms.where;

import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.ITransformDialog;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.ui.core.FormDataBuilder;
import org.apache.hop.ui.expression.AbstractTransformDialog;
import org.apache.hop.ui.expression.ExpressionEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

public class WhereDialog extends AbstractTransformDialog<WhereMeta> implements ITransformDialog {

	private static final Class<?> PKG = WhereMeta.class; // for i18n purposes

	private ExpressionEditor wEditor;

	public WhereDialog(Shell parent, Object transformMeta, PipelineMeta pipelineMeta, String name) {
		super(parent, transformMeta, pipelineMeta, name);

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
		this.transformName = this.wTransformName.getText();

		meta.setExpression(this.wEditor.getText());
	}

	@Override
	protected Control createDialogArea(final Composite parent) {

		wEditor = new ExpressionEditor(parent, SWT.NONE, true);
		wEditor.setVariables(this.pipelineMeta);
		wEditor.setLayoutData(new FormDataBuilder().top().fullWidth().bottom().result());

		// Search the fields in the background
		new Thread(() -> {
			try {
				TransformMeta transformMeta = pipelineMeta.findTransform(transformName);
				if (transformMeta != null) {
					IRowMeta rowMeta = pipelineMeta.getPrevTransformFields(transformMeta);
					wEditor.setRowMeta(rowMeta);
				}
			} catch (HopException e) {
				logError(BaseMessages.getString(PKG, "System.Dialog.GetFieldsFailed.Message"), e);
			}
		}).start();

		return parent;
	}
}