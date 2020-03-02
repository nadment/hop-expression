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

package org.apache.hop.trans.steps.where;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.apache.hop.core.CheckResult;
import org.apache.hop.core.CheckResultInterface;
import org.apache.hop.core.Const;
import org.apache.hop.core.annotations.Step;
import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.exception.HopXMLException;
import org.apache.hop.core.injection.Injection;
import org.apache.hop.core.injection.InjectionSupported;
import org.apache.hop.core.row.RowMetaInterface;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.VariableSpace;
import org.apache.hop.core.xml.XMLHandler;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metastore.api.IMetaStore;
import org.apache.hop.trans.Trans;
import org.apache.hop.trans.TransMeta;
import org.apache.hop.trans.step.BaseStepMeta;
import org.apache.hop.trans.step.StepDataInterface;
import org.apache.hop.trans.step.StepIOMeta;
import org.apache.hop.trans.step.StepIOMetaInterface;
import org.apache.hop.trans.step.StepInterface;
import org.apache.hop.trans.step.StepMeta;
import org.apache.hop.trans.step.StepMetaInterface;
import org.apache.hop.trans.step.errorhandling.Stream;
import org.apache.hop.trans.step.errorhandling.StreamIcon;
import org.apache.hop.trans.step.errorhandling.StreamInterface;
import org.apache.hop.trans.step.errorhandling.StreamInterface.StreamType;
import org.apache.hop.ui.trans.steps.where.WhereDialog;
import org.w3c.dom.Node;

/**
 * This step filter rows with expression and keeps only rows where this
 * expression is true.
 * 
 * @author Nicolas ADMENT
 *
 */
@Step(id = "Where", image = "Where.svg", i18nPackageName = "org.kettle.trans.steps.where", name = "Where.Name", description = "Where.Description", categoryDescription = "i18n:org.apache.hop.trans.step:BaseStep.Category.Experimental", documentationUrl = "https://help.pentaho.com")
@InjectionSupported(localizationPrefix = "Where.Injection.")
public class WhereMeta extends BaseStepMeta implements StepMetaInterface {

	private static Class<?> PKG = WhereMeta.class; // for i18n purposes

	/**
	 * Constants:
	 */
	private static final String TAG_EXPRESSION = "expression";
	private static final String TAG_SEND_TRUE_TO = "send_true_to";
	private static final String TAG_SEND_FALSE_TO = "send_false_to";

	@Injection(name = "EXPRESSION")
	private String expression;

	/** The true target step name */
	// @Injection(name = "TRUE_TARGET_STEP_NAME")
	// private String trueTargetStepname;
	// @Injection(name = "FALSE_TARGET_STEP_NAME")
	// private String falseTargetStepname;

	public WhereMeta() {
		super();
	}

	@Override
	public StepInterface getStep(StepMeta stepMeta, StepDataInterface stepDataInterface, int cnr, TransMeta transMeta,
			Trans disp) {
		return new WhereStep(stepMeta, stepDataInterface, cnr, transMeta, disp);
	}

	@Override
	public StepDataInterface getStepData() {
		return new WhereData();
	}

	@Override
	public void setDefault() {
		this.expression = "";
	}

	@Override
	public Object clone() {
		WhereMeta clone = (WhereMeta) super.clone();

		clone.setTrueStepName(getTrueStepName());
		clone.setFalseStepName(getFalseStepName());
		clone.setExpression(expression);

		return clone;
	}

	@Override
	public int hashCode() {
		return Objects.hash(getStepIOMeta().getTargetStreams(), expression);
	}

	// For compatibility with 7.x
	@Override
	public String getDialogClassName() {
		return WhereDialog.class.getName();
	}

	@Override
	public String getXML() throws HopValueException {

		StringBuilder xml = new StringBuilder(500);

		xml.append(XMLHandler.addTagValue(TAG_EXPRESSION, expression));
		// if (getTrueStepName() != null) {
		xml.append(XMLHandler.addTagValue(TAG_SEND_TRUE_TO, getTrueStepName()));
		// }
		// if (getFalseStepName() != null) {
		xml.append(XMLHandler.addTagValue(TAG_SEND_FALSE_TO, getFalseStepName()));
		// }
		return xml.toString();
	}

	@Override
	public void loadXML(Node stepNode, IMetaStore metaStore) throws HopXMLException {

		try {
			this.expression = XMLHandler.getTagValue(stepNode, TAG_EXPRESSION);
			this.setTrueStepName(XMLHandler.getTagValue(stepNode, TAG_SEND_TRUE_TO));
			this.setFalseStepName(XMLHandler.getTagValue(stepNode, TAG_SEND_FALSE_TO));
		} catch (Exception e) {
			throw new HopXMLException(BaseMessages.getString(PKG, "WhereMeta.Exception.UnableToReadStepInfoFromXML"),
					e);
		}
	}


	@Override
	public void check(List<CheckResultInterface> remarks, TransMeta transMeta, StepMeta stepMeta, RowMetaInterface prev,
			String input[], String output[], RowMetaInterface info, VariableSpace space, IMetaStore metaStore) {

		// See if there filter expression
		if (Utils.isEmpty(this.getExpression())) {
			remarks.add(new CheckResult(CheckResultInterface.TYPE_RESULT_WARNING,
					BaseMessages.getString(PKG, "WhereMeta.CheckResult.EmptyFilterExpression"), stepMeta));
		}

		checkTarget(stepMeta, true, getTrueStepName(), output).ifPresent(remarks::add);
		checkTarget(stepMeta, false, getFalseStepName(), output).ifPresent(remarks::add);

		// Look up fields in the input stream <prev>
		if (prev != null && prev.size() > 0) {
			remarks.add(new CheckResult(CheckResultInterface.TYPE_RESULT_OK,
					BaseMessages.getString(PKG, "WhereMeta.CheckResult.StepReceivingFields", prev.size() + ""),
					stepMeta));

			// List<String> orphanFields = getOrphanFields( condition, prev );
			//
			// if ( orphanFields.size() > 0 ) {
			// error_message = BaseMessages.getString( PKG,
			// "FilterRowsMeta.CheckResult.FieldsNotFoundFromPreviousStep" )
			// + Const.CR;
			// for ( String field : orphanFields ) {
			// error_message += "\t\t" + field + Const.CR;
			// }
			// cr = new CheckResult( CheckResultInterface.TYPE_RESULT_ERROR,
			// error_message, stepMeta );
			// } else {
			// cr =
			// new CheckResult( CheckResultInterface.TYPE_RESULT_OK,
			// BaseMessages.getString( PKG,
			// "FilterRowsMeta.CheckResult.AllFieldsFoundInInputStream" ),
			// stepMeta );
			// }
			// remarks.add( cr );
		} else {
			remarks.add(new CheckResult(CheckResultInterface.TYPE_RESULT_ERROR,
					BaseMessages.getString(PKG, "WhereMeta.CheckResult.CouldNotReadFieldsFromPreviousStep"), stepMeta));
		}

		// See if we have input streams leading to this step!
		if (input.length > 0) {
			remarks.add(new CheckResult(CheckResultInterface.TYPE_RESULT_OK,
					BaseMessages.getString(PKG, "WhereMeta.CheckResult.StepReceivingInfoFromOtherSteps"), stepMeta));

		} else {
			remarks.add(new CheckResult(CheckResultInterface.TYPE_RESULT_ERROR,
					BaseMessages.getString(PKG, "WhereMeta.CheckResult.NoInputReceivedFromOtherSteps"), stepMeta));

		}
	}

	private Optional<CheckResult> checkTarget(StepMeta stepMeta, boolean target, String targetStepName,
			String[] output) {
		if (targetStepName != null) {
			int trueTargetIdx = Const.indexOfString(targetStepName, output);
			if (trueTargetIdx < 0) {
				return Optional.of(new CheckResult(CheckResultInterface.TYPE_RESULT_ERROR,
						BaseMessages.getString(PKG, "WhereMeta.CheckResult.TargetStepInvalid", target, targetStepName),
						stepMeta));
			}
		}
		return Optional.empty();
	}

	public String getExpression() {
		return expression;
	}

	public void setExpression(String expression) {
		this.expression = expression;
	}

	@Override
	public void searchInfoAndTargetSteps(List<StepMeta> steps) {
		for (StreamInterface stream : getStepIOMeta().getTargetStreams()) {
			stream.setStepMeta(StepMeta.findStep(steps, (String) stream.getSubject()));
		}
	}

	@Override
	public void resetStepIoMeta() {
		// super.resetStepIoMeta();
	}

	/**
	 * Returns the Input/Output metadata for this step.
	 */
	@Override
	public StepIOMetaInterface getStepIOMeta() {

		// Since 7.1.0.22
		// StepIOMetaInterface stepIOMeta = this.getStepIOMeta(false);

		StepIOMetaInterface stepIOMeta = super.getStepIOMeta();
		if (stepIOMeta.isInputOptional()) {

			// ioMeta = new StepIOMeta(true, true, false, false, false, false);
			((StepIOMeta) stepIOMeta).setInputOptional(false);

			stepIOMeta.addStream(new Stream(StreamType.TARGET, null,
					BaseMessages.getString(PKG, "WhereMeta.TargetStream.True.Description"), StreamIcon.TRUE, null));
			stepIOMeta.addStream(new Stream(StreamType.TARGET, null,
					BaseMessages.getString(PKG, "WhereMeta.TargetStream.False.Description"), StreamIcon.FALSE, null));
			// setStepIOMeta(ioMeta);
		}

		return stepIOMeta;
	}

	private String getTargetStepName(int streamIndex) {
		StreamInterface stream = getStepIOMeta().getTargetStreams().get(streamIndex);
		return java.util.stream.Stream.of(stream.getStepname(), stream.getSubject()).filter(Objects::nonNull)
				.findFirst().map(Object::toString).orElse(null);
	}

	public String getTrueStepName() {
		return getTargetStepName(0);
	}

	@Injection(name = "TRUE_TARGET_STEP_NAME")
	public void setTrueStepName(final String stepname) {
		List<StreamInterface> targetStreams = getStepIOMeta().getTargetStreams();
		targetStreams.get(0).setSubject(stepname);
	}

	public String getFalseStepName() {
		return getTargetStepName(1);
	}

	@Injection(name = "FALSE_TARGET_STEP_NAME")
	public void setFalseStepName(final String stepname) {
		List<StreamInterface> targetStreams = getStepIOMeta().getTargetStreams();
		targetStreams.get(1).setSubject(stepname);
	}

	/**
	 * When an optional stream is selected, this method is called to handled the ETL
	 * metadata implications of that.
	 *
	 * @param stream The optional stream to handle.
	 */
	public void handleStreamSelection(final StreamInterface stream) {
		// This step targets another step.
		// Make sure that we don't specify the same step for true and false...
		// If the user requests false, we blank out true and vice versa
		//
		List<StreamInterface> targets = getStepIOMeta().getTargetStreams();
		int index = targets.indexOf(stream);
		if (index == 0) {
			// True
			StepMeta falseStep = targets.get(1).getStepMeta();
			if (falseStep != null && falseStep.equals(stream.getStepMeta())) {
				targets.get(1).setStepMeta(null);
			}
		}
		if (index == 1) {
			// False
			StepMeta trueStep = targets.get(0).getStepMeta();
			if (trueStep != null && trueStep.equals(stream.getStepMeta())) {
				targets.get(0).setStepMeta(null);
			}
		}

		// this.resetStepIoMeta(); // force stepIo to be recreated when it is next
		// needed.
	}

	@Override
	public boolean excludeFromCopyDistributeVerification() {
		return true;
	}
}