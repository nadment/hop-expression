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

package org.apache.hop.trans.steps.where;

import java.util.List;

import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.util.Utils;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.RowExpressionEvaluator;
import org.apache.hop.expression.Value;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.trans.Trans;
import org.apache.hop.trans.TransMeta;
import org.apache.hop.trans.step.BaseStep;
import org.apache.hop.trans.step.StepDataInterface;
import org.apache.hop.trans.step.StepInterface;
import org.apache.hop.trans.step.StepMeta;
import org.apache.hop.trans.step.StepMetaInterface;
import org.apache.hop.trans.step.errorhandling.StreamInterface;

/**
 * @author Nicolas ADMENT 
 *
 */

public class WhereStep extends BaseStep implements StepInterface {

	private static Class<?> PKG = WhereMeta.class;

	public WhereStep(StepMeta stepMeta, StepDataInterface stepDataInterface, int copyNr, TransMeta transMeta,
			Trans trans) {
		super(stepMeta, stepDataInterface, copyNr, transMeta, trans);
	}

	@Override
	public boolean init(StepMetaInterface smi, StepDataInterface sdi) {
		// Casting to step-specific implementation classes is safe
		WhereMeta meta = (WhereMeta) smi;
		WhereData data = (WhereData) sdi;

		first = true;

		return super.init(meta, data);
	}

	@Override
	public boolean processRow(StepMetaInterface smi, StepDataInterface sdi) throws HopException {

		// safely cast the step settings (meta) and runtime info (data) to
		// specific implementations
		WhereMeta meta = (WhereMeta) smi;
		WhereData data = (WhereData) sdi;

		// get incoming row, getRow() potentially blocks waiting for more rows,
		// returns null if no more rows expected
		Object[] row = getRow();

		// if no more rows are expected, indicate step is finished and
		// processRow() should not be called again
		if (row == null) {
			setOutputDone();
			return false;
		}

		// the "first" flag is inherited from the base step implementation
		// it is used to guard some processing tasks, like figuring out field
		// indexes
		// in the row structure that only need to be done once
		if (first) {
			if (log.isDebug()) {
				logDebug(BaseMessages.getString(PKG, "ExpressionFilter.Log.StartedProcessing"));
			}

			first = false;

			data.condition = Expression.parse(meta.getExpression());

			// clone the input row structure and place it in our data object
			data.outputRowMeta = getInputRowMeta().clone();
			// use meta.getFields() to change it, so it reflects the output row
			// structure
			meta.getFields(data.outputRowMeta, getStepname(), null, null, this, null);

			// Cache the position of the RowSet for the output.
			List<StreamInterface> streams = meta.getStepIOMeta().getTargetStreams();

			if (!Utils.isEmpty(streams.get(0).getStepname())) {
				data.trueRowSet = findOutputRowSet(getStepname(), getCopy(), streams.get(0).getStepname(), 0);
				if (data.trueRowSet == null) {
					throw new HopException(BaseMessages.getString(PKG, "ExpressionFilter.Log.TargetStepInvalid",
							streams.get(0).getStepname()));
				}
			} else {
				data.trueRowSet = null;
			}

			if (!Utils.isEmpty(streams.get(1).getStepname())) {
				data.falseRowSet = findOutputRowSet(getStepname(), getCopy(), streams.get(1).getStepname(), 0);
				if (data.falseRowSet == null) {
					throw new HopException(BaseMessages.getString(PKG, "ExpressionFilter.Log.TargetStepInvalid",
							streams.get(1).getStepname()));
				}
			} else {
				data.falseRowSet = null;
			}

		}

		// TODO: create one context per thread 
		RowExpressionEvaluator context = new RowExpressionEvaluator(getInputRowMeta());
		context.setRow(row);
		Value keep = data.condition.eval(context);

		if (keep.toBoolean()) {
			// put the row to the TRUE output row stream
			if (data.trueRowSet != null) {
				if (log.isRowLevel()) {
					logRowlevel(BaseMessages.getString(PKG, "ExpressionFilter.Log.KeepRow",
							data.trueRowSet.getDestinationStepName(), getInputRowMeta().getString(row)));
				}
				putRowTo(data.outputRowMeta, row, data.trueRowSet);
			}
		} else {
			// put the row to the FALSE output row stream
			if (data.falseRowSet != null) {
				if (log.isRowLevel()) {
					logRowlevel(BaseMessages.getString(PKG, "ExpressionFilter.Log.FilterRow",
							data.falseRowSet.getDestinationStepName(), getInputRowMeta().getString(row)));
				}
				putRowTo(data.outputRowMeta, row, data.falseRowSet);
			}
		}

		// log progress if it is time to to so
		if (checkFeedback(getLinesRead())) {
			if (log.isBasic()) {
				logBasic(BaseMessages.getString(PKG, "ExpressionFilter.Log.LineNumber") + getLinesRead());
			}
		}

		// indicate that processRow() should be called again
		return true;
	}

	/**
	 * This method is called by PDI once the step is done processing.
	 *
	 * The dispose() method is the counterpart to init() and should release any
	 * resources acquired for step execution like file handles or database
	 * connections.
	 */
	@Override
	public void dispose(StepMetaInterface smi, StepDataInterface sdi) {

		// Casting to step-specific implementation classes is safe
		WhereMeta meta = (WhereMeta) smi;
		WhereData data = (WhereData) sdi;

		super.dispose(meta, data);
	}
}