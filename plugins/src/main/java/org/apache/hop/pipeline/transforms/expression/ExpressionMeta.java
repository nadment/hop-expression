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

package org.apache.hop.pipeline.transforms.expression;

import java.util.ArrayList;
import java.util.List;

import org.apache.hop.core.CheckResult;
import org.apache.hop.core.Const;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.annotations.Transform;
import org.apache.hop.core.exception.HopTransformException;
import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.exception.HopXMLException;
import org.apache.hop.core.injection.InjectionDeep;
import org.apache.hop.core.injection.InjectionSupported;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaFactory;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.xml.XMLHandler;
import org.apache.hop.expression.Expression;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metastore.api.IMetaStore;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransformMeta;
import org.apache.hop.pipeline.transform.ITransformMeta;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.w3c.dom.Node;

/**
 * This transform create field value with expression.
 * 
 * @author Nicolas ADMENT
 *
 */
@Transform(
	id = "Expression",
	name = "Expression.Name",
	image = "Expression.svg",
	description = "Expression.Description",
	i18nPackageName = "org.apache.hop.pipeline.transforms.expression",
	keywords = {"Script","SQL"}
)
@InjectionSupported(localizationPrefix = "ExpressionMeta.Injection.")
public class ExpressionMeta extends BaseTransformMeta implements ITransformMeta<ExpressionTransform, ExpressionData> {

	private static final Class<?> PKG = ExpressionMeta.class; // for i18n purposes

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

	@Override
	public ExpressionTransform createTransform( TransformMeta transformMeta, ExpressionData data, int cnr, PipelineMeta tr,
              Pipeline pipeline ) {
		return new ExpressionTransform( transformMeta, this, data, cnr, tr, pipeline );
	}

	@Override
	public ExpressionData getTransformData() {
		return new ExpressionData();
	}

	@Override
	public String getDialogClassName() {
		return ExpressionDialog.class.getName();
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
	public void loadXML(Node transformNode, IMetaStore metaStore) throws HopXMLException {

		try {
			Node nodes = XMLHandler.getSubNode(transformNode, "fields");
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
					BaseMessages.getString(PKG, "ExpressionMeta.Exception.UnableToReadXML"), e);
		}

	}

	@Override
	public void getFields(IRowMeta rowMeta, String transformName, IRowMeta[] info, TransformMeta nextTransform,
			IVariables variables, IMetaStore metaStore) throws HopTransformException {
		try {
			// store the input stream meta
			IRowMeta unalteredInputRowMeta = rowMeta.clone();

			// add the output fields if specified
			for (ExpressionField field : this.getExpressionValues()) {
				if (!Utils.isEmpty(field.getName())) {

					// create ValueMeta
					IValueMeta vm = ValueMetaFactory.createValueMeta(field.getName(), field.getType());
					vm.setOrigin(transformName);
					vm.setLength(field.getLength(), field.getPrecision());

					// field already exist
					int index = unalteredInputRowMeta.indexOfValue(field.getName());
					if (index > 0) {
						rowMeta.removeValueMeta(index);
						rowMeta.addValueMeta(index, vm);
					} else {
						rowMeta.addValueMeta(vm);
					}
				}
			}
		} catch (Exception e) {
			throw new HopTransformException(e);
		}
	}

	@Override
	public void check(List<ICheckResult> remarks, PipelineMeta pipelineMeta, TransformMeta tranformMeta, IRowMeta prev,
			String input[], String output[], IRowMeta info, IVariables variables, IMetaStore metaStore) {

		// Look up fields in the input stream <prev>
		if (prev != null && prev.size() > 0) {
			remarks.add(new CheckResult(
					ICheckResult.TYPE_RESULT_OK, BaseMessages.getString(PKG,
							"ExpressionMeta.CheckResult.ReceivingFieldsFromPreviousTransforms", prev.size() + ""),
					tranformMeta));
		} else {
			remarks.add(new CheckResult(ICheckResult.TYPE_RESULT_ERROR,
					BaseMessages.getString(PKG, "ExpressionMeta.CheckResult.NotReceivingFieldsFromPreviousTransforms"),
					tranformMeta));
		}

		// See if we have input streams leading to this transform!
		if (input.length > 0) {
			remarks.add(new CheckResult(ICheckResult.TYPE_RESULT_OK,
					BaseMessages.getString(PKG, "ExpressionMeta.CheckResult.ReceivingInfoFromOtherTransforms"), tranformMeta));

		} else {
			remarks.add(new CheckResult(ICheckResult.TYPE_RESULT_ERROR,
					BaseMessages.getString(PKG, "ExpressionMeta.CheckResult.NotReceivingInfoFromOtherTransforms"),
					tranformMeta));

		}

		// Check expression
		for (ExpressionField field : this.fields) {
			try {
				Expression.parse(field.getExpression());
			} catch (Exception e) {
				remarks.add(new CheckResult(
						ICheckResult.TYPE_RESULT_ERROR, BaseMessages.getString(PKG,
								"ExpressionMeta.CheckResult.InvalidExpression", field.getName(), e.getMessage()),
						tranformMeta));
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