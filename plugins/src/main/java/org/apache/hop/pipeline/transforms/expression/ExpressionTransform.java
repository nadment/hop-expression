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

package org.apache.hop.pipeline.transforms.expression;

import java.util.Arrays;

import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.expression.Expression;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.Value;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransform;
import org.apache.hop.pipeline.transform.TransformMeta;

/**
 * @author Nicolas ADMENT
 */

public class ExpressionTransform extends BaseTransform<ExpressionMeta, ExpressionData> {

	private static final Class<?> PKG = ExpressionMeta.class;

	public ExpressionTransform(TransformMeta transformMeta, ExpressionMeta meta, ExpressionData data, int copyNr,
			PipelineMeta pipelineMeta, Pipeline pipeline) {
		super(transformMeta, meta, data, copyNr, pipelineMeta, pipeline);
	}

	@Override
	public boolean processRow() throws HopException {

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
				logDebug(BaseMessages.getString(PKG, "ExpressionTransform.Log.StartedProcessing"));
			}

			first = false;

			// Clone the input row structure and place it in our data object
			data.outputRowMeta = getInputRowMeta().clone();

			// Use meta.getFields() to change it, so it reflects the output row structure
			meta.getFields(data.outputRowMeta, this.getTransformName(), null, null, this, null);
			data.expressions = new Expression[data.outputRowMeta.size()];

			RowExpressionContext context = new RowExpressionContext(getInputRowMeta());
			data.expressionContext = context;

			// For all fields expression
			for (ExpressionField field : meta.getExpressionValues()) {
				int index = data.outputRowMeta.indexOfValue(field.getName());

				// Substitute variable
				String e = field.getExpression();
				String source = environmentSubstitute(e);

				if (log.isDetailed()) {
					logDetailed("field [" + field.getName() + "] has expression [" + source + "]");
				}

				// TODO: DEBUG ignore expression if type NONE
				IValueMeta valueMeta = data.outputRowMeta.getValueMeta(index);
				if (valueMeta.getType() == IValueMeta.TYPE_NONE) {
					source = "NULL";
				}

				// Parse and optimize expression
				Expression expression = ExpressionParser.parse(source);
				data.expressions[index] = expression.optimize(context);
			}
		}

		// Copies row into outputRowValues and pads extra null-default slots for the
		// output values
		Object[] outputRowValues = Arrays.copyOf(row, data.outputRowMeta.size());

		for (ExpressionField field : meta.getExpressionValues()) {

			int index = data.outputRowMeta.indexOfValue(field.getName());

			Value value = null;
			try {

				// Evaluate expression
				RowExpressionContext context = data.expressionContext;
				context.setRow(row);

				Expression expression = data.expressions[index];
				value = expression.eval(context);

				if (log.isDetailed()) {
					logDetailed("field [" + field.getName() + "] has expression [" + value + "]");
				}

				IValueMeta valueMeta = data.outputRowMeta.getValueMeta(index);
				outputRowValues[index] = convertValue(valueMeta, value);
			} catch (ExpressionException e) {
				logError(BaseMessages.getString(PKG, "ExpressionTransform.Exception.ExpressionError"));
				throw e;
			}
		}

		// Put the row to the output row stream
		putRow(data.outputRowMeta, outputRowValues);

		// Log progress if it is time to to so
		if (checkFeedback(getLinesRead())) {
			if (log.isBasic()) {
				logBasic(BaseMessages.getString(PKG, "ExpressionTransform.Log.LineNumber") + getLinesRead());
			}
		}

		// indicate that processRow() should be called again
		return true;
	}

	public Object convertValue(IValueMeta meta, Value value) throws HopValueException {
		switch (meta.getType()) {
		case IValueMeta.TYPE_NONE:
		case IValueMeta.TYPE_STRING:
			return value.toString();
		case IValueMeta.TYPE_NUMBER:
			return value.toNumber();
		case IValueMeta.TYPE_INTEGER:
			return value.toInteger();
		case IValueMeta.TYPE_DATE:
			return java.util.Date.from(value.toDate());
		case IValueMeta.TYPE_TIMESTAMP:
			return java.sql.Timestamp.from(value.toDate());
		case IValueMeta.TYPE_BIGNUMBER:
			return value.toBigNumber();
		case IValueMeta.TYPE_BOOLEAN:
			return value.toBoolean();
		case IValueMeta.TYPE_BINARY:
			return value.toBinary();
		default:
			throw new HopValueException(
					value + " : I can't convert the specified value to data type : " + meta.getType());
		}
	}
}