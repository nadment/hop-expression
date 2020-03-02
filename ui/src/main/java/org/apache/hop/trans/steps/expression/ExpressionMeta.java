/******************************************************************************
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

import java.util.ArrayList;
import java.util.List;

import org.apache.hop.core.CheckResult;
import org.apache.hop.core.CheckResultInterface;
import org.apache.hop.core.Const;
import org.apache.hop.core.annotations.Step;
import org.apache.hop.core.exception.HopStepException;
import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.exception.HopXMLException;
import org.apache.hop.core.injection.InjectionDeep;
import org.apache.hop.core.injection.InjectionSupported;
import org.apache.hop.core.row.RowMetaInterface;
import org.apache.hop.core.row.ValueMetaInterface;
import org.apache.hop.core.row.value.ValueMetaFactory;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.VariableSpace;
import org.apache.hop.core.xml.XMLHandler;
import org.apache.hop.expression.Expression;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metastore.api.IMetaStore;
import org.apache.hop.trans.Trans;
import org.apache.hop.trans.TransMeta;
import org.apache.hop.trans.step.BaseStepMeta;
import org.apache.hop.trans.step.StepDataInterface;
import org.apache.hop.trans.step.StepInterface;
import org.apache.hop.trans.step.StepMeta;
import org.apache.hop.trans.step.StepMetaInterface;
import org.apache.hop.ui.trans.steps.expression.ExpressionDialog;
import org.w3c.dom.Node;

/**
 * This step filter rows with expression and keeps only rows where this
 * expression is true.
 * 
 * @author Nicolas ADMENT
 *
 */
@Step(id = "Expression", name = "Expression.Name", image = "Expression.svg", description = "Expression.Description", i18nPackageName = "org.kettle.trans.steps.expression", categoryDescription = "i18n:org.apache.hop.trans.step:BaseStep.Category.Experimental", documentationUrl = "https://help.pentaho.com")
@InjectionSupported(localizationPrefix = "ExpressionMeta.Injection.")
public class ExpressionMeta extends BaseStepMeta implements StepMetaInterface {

	private static Class<?> PKG = ExpressionMeta.class; // for i18n purposes

	/**
	 * Constants:
	 */
	private static final String TAG_FIELD_EXPRESSION = "expression"; //$NON-NLS-1$
	private static final String TAG_FIELD_NAME = "field"; //$NON-NLS-1$
	private static final String TAG_FIELD_TYPE = "type"; //$NON-NLS-1$

	@InjectionDeep
	private List<ExpressionField> fields;

	public ExpressionMeta() {
		super();
	}

	/**
	 * Called by PDI to get a new instance of the step implementation. A standard
	 * implementation passing the arguments to the constructor of the step class is
	 * recommended.
	 *
	 * @param stepMeta          description of the step
	 * @param stepDataInterface instance of a step data class
	 * @param cnr               copy number
	 * @param transMeta         description of the transformation
	 * @param disp              runtime implementation of the transformation
	 * @return the new instance of a step implementation
	 */
	@Override
	public StepInterface getStep(StepMeta stepMeta, StepDataInterface stepDataInterface, int cnr, TransMeta transMeta,
			Trans disp) {
		return new ExpressionStep(stepMeta, stepDataInterface, cnr, transMeta, disp);
	}

	/**
	 * Called by PDI to get a new instance of the step data class.
	 */
	@Override
	public StepDataInterface getStepData() {
		return new ExpressionData();
	}

	/**
	 * This method is called every time a new step is created and should
	 * allocate/set the step configuration to sensible defaults. The values set here
	 * will be used by Spoon when a new step is created.
	 */
	@Override
	public void setDefault() {
		this.fields = new ArrayList<>();
	}

	@Override
	public Object clone() {
		ExpressionMeta clone = (ExpressionMeta) super.clone();

		return clone;
	}

	@Override
	public String getXML() throws HopValueException {

		StringBuilder xml = new StringBuilder(500);

		xml.append("<fields>");
		for (ExpressionField value : this.getExpressionValues()) {
			xml.append("<field>");
			xml.append(XMLHandler.addTagValue(TAG_FIELD_NAME, value.getName()));
			xml.append(XMLHandler.addTagValue(TAG_FIELD_EXPRESSION, value.getExpression()));
			xml.append(XMLHandler.addTagValue(TAG_FIELD_TYPE, ValueMetaFactory.getValueMetaName(value.getType())));
			xml.append("</field>");
		}
		xml.append("</fields>");

		return xml.toString();
	}

	@Override
	public void loadXML(Node stepNode, IMetaStore metaStore) throws HopXMLException {

		try {
			Node nodes = XMLHandler.getSubNode(stepNode, "fields");
			int count = XMLHandler.countNodes(nodes, "field");

			fields = new ArrayList<>(count);
			for (int i = 0; i < count; i++) {
				Node line = XMLHandler.getSubNodeByNr(nodes, "field", i);

				ExpressionField value = new ExpressionField();
				value.setName(Const.NVL(XMLHandler.getTagValue(line, TAG_FIELD_NAME), ""));
				value.setExpression(Const.NVL(XMLHandler.getTagValue(line, TAG_FIELD_EXPRESSION), ""));
				value.setType(XMLHandler.getTagValue(line, TAG_FIELD_TYPE));

				fields.add(value);
			}

		} catch (Exception e) {
			throw new HopXMLException(
					BaseMessages.getString(PKG, "ExpressionMeta.Exception.UnableToReadStepInfoFromXML"), e);
		}

	}

	// For compatibility with 7.x
	@Override
	public String getDialogClassName() {
		return ExpressionDialog.class.getName();
	}

	/**
	 * This method is called to determine the changes the step is making to the
	 * row-stream.
	 *
	 * @param inputRowMeta the row structure coming in to the step
	 * @param stepName     the name of the step making the changes
	 * @param info         row structures of any info steps coming in
	 * @param nextStep     the description of a step this step is passing rows to
	 * @param space        the variable space for resolving variables
	 * @param repository   the repository instance optionally read from
	 * @param metaStore    the metaStore to optionally read from
	 */
	@Override
	public void getFields(RowMetaInterface inputRowMeta, String stepName, RowMetaInterface[] info, StepMeta nextStep,
			VariableSpace space, IMetaStore metaStore) throws HopStepException {
		try {
			// store the input stream meta
			RowMetaInterface unalteredInputRowMeta = inputRowMeta.clone();

			// add the output fields if specified
			for (ExpressionField field : this.getExpressionValues()) {
				if (!Utils.isEmpty(field.getName())) {

					// create ValueMeta
					ValueMetaInterface vm = ValueMetaFactory.createValueMeta(field.getName(), field.getType());
					vm.setOrigin(stepName);
					vm.setLength(field.getLength(), field.getPrecision());

					// field already exist
					int index = unalteredInputRowMeta.indexOfValue(field.getName());
					if (index > 0) {
						inputRowMeta.removeValueMeta(index);
						inputRowMeta.addValueMeta(index, vm);
					} else {
						inputRowMeta.addValueMeta(vm);
					}
				}
			}
		} catch (Exception e) {
			throw new HopStepException(e);
		}
	}

	/**
	 * This method is called when the user selects the "Verify Transformation"
	 * option in Spoon.
	 *
	 * @param remarks   the list of remarks to append to
	 * @param transMeta the description of the transformation
	 * @param stepMeta  the description of the step
	 * @param prev      the structure of the incoming row-stream
	 * @param input     names of steps sending input to the step
	 * @param output    names of steps this step is sending output to
	 * @param info      fields coming in from info steps
	 * @param metaStore metaStore to optionally read from
	 */
	@Override
	public void check(List<CheckResultInterface> remarks, TransMeta transMeta, StepMeta stepMeta, RowMetaInterface prev,
			String input[], String output[], RowMetaInterface info, VariableSpace space, IMetaStore metaStore) {

		// Look up fields in the input stream <prev>
		if (prev != null && prev.size() > 0) {
			remarks.add(new CheckResult(
					CheckResultInterface.TYPE_RESULT_OK, BaseMessages.getString(PKG,
							"ExpressionMeta.CheckResult.ReceivingFieldsFromPreviousSteps", prev.size() + ""),
					stepMeta));
		} else {
			remarks.add(new CheckResult(CheckResultInterface.TYPE_RESULT_ERROR,
					BaseMessages.getString(PKG, "ExpressionMeta.CheckResult.NotReceivingFieldsFromPreviousSteps"),
					stepMeta));
		}

		// See if we have input streams leading to this step!
		if (input.length > 0) {
			remarks.add(new CheckResult(CheckResultInterface.TYPE_RESULT_OK,
					BaseMessages.getString(PKG, "ExpressionMeta.CheckResult.ReceivingInfoFromOtherSteps"), stepMeta));

		} else {
			remarks.add(new CheckResult(CheckResultInterface.TYPE_RESULT_ERROR,
					BaseMessages.getString(PKG, "ExpressionMeta.CheckResult.NotReceivingInfoFromOtherSteps"),
					stepMeta));

		}

		// Check expression
		for (ExpressionField field : this.fields) {
			try {
				Expression.parse(field.getExpression());
			} catch (Exception e) {
				remarks.add(new CheckResult(
						CheckResultInterface.TYPE_RESULT_ERROR, BaseMessages.getString(PKG,
								"ExpressionMeta.CheckResult.InvalidExpression", field.getName(), e.getMessage()),
						stepMeta));
			}

		}
	}

	public List<ExpressionField> getExpressionValues() {
		return this.fields;
	}

	public void setExpressionValues(final List<ExpressionField> values) {
		this.fields = values;
	}
}