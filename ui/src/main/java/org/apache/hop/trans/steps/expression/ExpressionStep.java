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

package org.apache.hop.trans.steps.expression;

import java.util.Arrays;
import java.util.HashMap;

import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.row.ValueMetaInterface;
import org.apache.hop.core.row.value.ValueMetaFactory;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.ExpressionException;
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

/**
 * @author Nicolas ADMENT
 */

public class ExpressionStep extends BaseStep implements StepInterface {

	private static Class<?> PKG = ExpressionMeta.class;

	private HashMap<DataType, ValueMetaInterface> valueMetaByType = new HashMap<>(8);

	public ExpressionStep(StepMeta stepMeta, StepDataInterface stepDataInterface, int copyNr, TransMeta transMeta,
			Trans trans) {
		super(stepMeta, stepDataInterface, copyNr, transMeta, trans);
	}

	@Override
	public boolean init(StepMetaInterface smi, StepDataInterface sdi) {
		// Casting to step-specific implementation classes is safe
		ExpressionMeta meta = (ExpressionMeta) smi;
		ExpressionData data = (ExpressionData) sdi;

		return super.init(meta, data);
	}

	@Override
	public boolean processRow(StepMetaInterface smi, StepDataInterface sdi) throws HopException {

		// safely cast the step settings (meta) and runtime info (data) to
		// specific implementations
		ExpressionMeta meta = (ExpressionMeta) smi;
		ExpressionData data = (ExpressionData) sdi;

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
				logDebug(BaseMessages.getString(PKG, "Filter.Log.StartedProcessing"));
			}

			first = false;

			// Value meta conversion
			valueMetaByType.put(DataType.BOOLEAN, ValueMetaFactory.createValueMeta(ValueMetaInterface.TYPE_BOOLEAN));
			valueMetaByType.put(DataType.INTEGER, ValueMetaFactory.createValueMeta(ValueMetaInterface.TYPE_INTEGER));
			valueMetaByType.put(DataType.STRING, ValueMetaFactory.createValueMeta(ValueMetaInterface.TYPE_STRING));
			valueMetaByType.put(DataType.DATE, ValueMetaFactory.createValueMeta(ValueMetaInterface.TYPE_DATE));
			valueMetaByType.put(DataType.BINARY, ValueMetaFactory.createValueMeta(ValueMetaInterface.TYPE_BINARY));
			valueMetaByType.put(DataType.NUMBER, ValueMetaFactory.createValueMeta(ValueMetaInterface.TYPE_NUMBER));
			valueMetaByType.put(DataType.BIGNUMBER,	ValueMetaFactory.createValueMeta(ValueMetaInterface.TYPE_BIGNUMBER));

			// clone the input row structure and place it in our data object
			data.outputRowMeta = getInputRowMeta().clone();

			
			data.expressionContext = new RowExpressionEvaluator(getInputRowMeta());
			
			// use meta.getFields() to change it, so it reflects the output row structure
			meta.getFields(data.outputRowMeta, getStepname(), null, null, this, null);
			data.expressions = new Expression[data.outputRowMeta.size()];

			// Parse field expression
			for (ExpressionField field : meta.getExpressionValues()) {
				int index = data.outputRowMeta.indexOfValue(field.getName());
				data.expressions[index] = Expression.parse(field.getExpression());
			}
		}

		// Copies row into outputRowValues and pads extra null-default slots for the
		// output values
		Object[] outputRowValues = Arrays.copyOf(row, data.outputRowMeta.size());

		for (ExpressionField field : meta.getExpressionValues()) {

			int index = data.outputRowMeta.indexOfValue(field.getName());

			ValueMetaInterface valueMeta = null;
			Value value = null;
			try {
				valueMeta = data.outputRowMeta.getValueMeta(index);

				Expression expression = data.expressions[index];

				// Evaluate expression
				RowExpressionEvaluator context = data.expressionContext; 
				context.setRow(row);
				value = expression.eval(context);

				ValueMetaInterface vm = valueMetaByType.get(value.getType());

				outputRowValues[index] = valueMeta.convertData(vm, value.getObject());
			} catch (ExpressionException e) {
				logError(BaseMessages.getString(PKG, "ExpressionValue.Log.ExpressionError"));
				throw e;
			} catch (HopValueException e) {
				logError(BaseMessages.getString(PKG, "ExpressionValue.Log.DataIncompatibleError", String.valueOf(value),
						valueMeta));
				throw e;
			}
		}

		// put the row to the output row stream
		putRow(data.outputRowMeta, outputRowValues);

		// log progress if it is time to to so
		if (checkFeedback(getLinesRead())) {
			if (log.isBasic()) {
				logBasic(BaseMessages.getString(PKG, "ExpressionValue.Log.LineNumber") + getLinesRead());
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
		ExpressionMeta meta = (ExpressionMeta) smi;
		ExpressionData data = (ExpressionData) sdi;

		super.dispose(meta, data);
	}
}