/*! ******************************************************************************
 *
 * Hop : The Hop Orchestration Platform
 *
 * http://www.project-hop.org
 *
 *******************************************************************************
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

package org.apache.hop.pipeline.transforms.clonerowexpression;

import org.apache.hop.core.CheckResult;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.annotations.Transform;
import org.apache.hop.core.exception.HopTransformException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.variables.Variables;
import org.apache.hop.expression.ExpressionBuilder;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.HopMetadataProperty;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransformMeta;
import org.apache.hop.pipeline.transform.TransformMeta;
import java.util.List;

@Transform(
    id = "CloneRowExpression",
    name = "CloneRow.Name",
    description = "CloneRow.Description",
    image = "clonerow.svg",
    categoryDescription = "i18n:org.apache.hop.pipeline.transform:BaseTransform.Category.Utility"
    )
public class CloneRowMeta extends BaseTransformMeta<CloneRow, CloneRowData> {

  private static Class<?> PKG = CloneRowMeta.class; // for i18n purposes, needed by Translator!!

  /** nr of clone rows */
  @HopMetadataProperty(
      key = "nrclones",
      injectionKeyDescription = "CloneRowMeta.Injection.NrClones")
  private String nrClones;

  /** Flag: add clone flag */
  @HopMetadataProperty(
      key = "addcloneflag",
      injectionKeyDescription = "CloneRowMeta.Injection.AddCloneFlag")
  private boolean addCloneFlag;

  /** clone flag field */
  @HopMetadataProperty(
      key = "cloneflagfield",
      injectionKeyDescription = "CloneRowMeta.Injection.CloneFlagField")
  private String cloneFlagField;
  
  @HopMetadataProperty(
      key = "addclonenum",
      injectionKeyDescription = "CloneRowMeta.Injection.AddCloneNum")
  private boolean addCloneNum;

  @HopMetadataProperty(
      key = "clonenumfield",
      injectionKeyDescription = "CloneRowMeta.Injection.CloneNumField")
  private String cloneNumField;

  public CloneRowMeta() {
    super(); // allocate BaseTransformMeta
  }

  @Override
  public Object clone() {
    Object retval = super.clone();
    return retval;
  }

  public String getNrClones() {
    return nrClones;
  }

  public void setNrClones(String nrclones) {
    this.nrClones = nrclones;
  }

  public boolean isAddCloneFlag() {
    return addCloneFlag;
  }

  public void setAddCloneFlag(boolean addcloneflag) {
    this.addCloneFlag = addcloneflag;
  }

  public boolean isAddCloneNum() {
    return addCloneNum;
  }

  public void setAddCloneNum(boolean addclonenum) {
    this.addCloneNum = addclonenum;
  }

  public String getCloneNumField() {
    return cloneNumField;
  }

  public void setCloneNumField(String clonenumfield) {
    this.cloneNumField = clonenumfield;
  }

  public String getCloneFlagField() {
    return cloneFlagField;
  }

  public void setCloneFlagField(String cloneflagfield) {
    this.cloneFlagField = cloneflagfield;
  }

  @Override
  public void setDefault() {
    nrClones = "0";
    cloneFlagField = null;
    addCloneFlag = false;
    addCloneNum = false;
    cloneNumField = null;
  }

  public String evaluate(String source, IVariables variables) throws HopTransformException {
    String value = variables.resolve(source);

    // If value start with '='  this is a expression
    if (value.charAt(0) != '=') {
      return value;
    }

    try {
      IExpressionContext context = new ExpressionContext(new Variables());
      IExpression expression = ExpressionBuilder.compile(context, value.substring(1));
      Object result = expression.getValue(new ExpressionContext(variables));      
      return String.valueOf(result);
    } catch (ExpressionException e) {
      throw new HopTransformException(BaseMessages.getString(PKG, "Unable to compile expression ''{0}''", source), e);
    }
  }

  @Override
  public void getFields(
      IRowMeta rowMeta,
      String origin,
      IRowMeta[] info,
      TransformMeta nextTransform,
      IVariables variables,
      IHopMetadataProvider metadataProvider)
      throws HopTransformException {
    // Output field (boolean) ?
    if (addCloneFlag) {
      String realfieldValue = String.valueOf(evaluate(cloneFlagField, variables));
      if (!Utils.isEmpty(realfieldValue)) {
        IValueMeta v = new ValueMetaBoolean(realfieldValue);
        v.setOrigin(origin);
        rowMeta.addValueMeta(v);
      }
    }
    // Output clone row number
    if (addCloneNum) {
      String realfieldValue = evaluate(cloneNumField, variables);
      if (!Utils.isEmpty(realfieldValue)) {
        IValueMeta v = new ValueMetaInteger(realfieldValue);
        v.setOrigin(origin);
        rowMeta.addValueMeta(v);
      }
    }
  }

  @Override
  public void check(
      List<ICheckResult> remarks,
      PipelineMeta pipelineMeta,
      TransformMeta transformMeta,
      IRowMeta prev,
      String[] input,
      String[] output,
      IRowMeta info,
      IVariables variables,
      IHopMetadataProvider metadataProvider) {
    CheckResult cr;
    String error_message = "";

    if (Utils.isEmpty(nrClones)) {
      error_message = BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.NrClonesdMissing");
      cr = new CheckResult(ICheckResult.TYPE_RESULT_ERROR, error_message, transformMeta);
    } else {
      // TODO: validate expression
      error_message = BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.NrClonesOK");
      cr = new CheckResult(ICheckResult.TYPE_RESULT_OK, error_message, transformMeta);
    }
    remarks.add(cr);

    if (addCloneFlag) {
      if (Utils.isEmpty(cloneFlagField)) {
        error_message =
            BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.CloneFlagFieldMissing");
        cr = new CheckResult(ICheckResult.TYPE_RESULT_ERROR, error_message, transformMeta);
      } else {
        // TODO: validate expression
        error_message = BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.CloneFlagFieldOk");
        cr = new CheckResult(ICheckResult.TYPE_RESULT_OK, error_message, transformMeta);
      }
      remarks.add(cr);
    }

    if (addCloneNum) {
      if (Utils.isEmpty(cloneNumField)) {
        error_message =
            BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.CloneNumFieldMissing");
        cr = new CheckResult(ICheckResult.TYPE_RESULT_ERROR, error_message, transformMeta);
      } else {
        // TODO: validate expression
        error_message = BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.CloneNumFieldOk");
        cr = new CheckResult(ICheckResult.TYPE_RESULT_OK, error_message, transformMeta);
      }
      remarks.add(cr);
    }

    if (prev == null || prev.size() == 0) {
      cr =
          new CheckResult(
              ICheckResult.TYPE_RESULT_WARNING,
              BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.NotReceivingFields"),
              transformMeta);
    } else {
      cr =
          new CheckResult(
              ICheckResult.TYPE_RESULT_OK,
              BaseMessages.getString(
                  PKG, "CloneRowMeta.CheckResult.TransformRecevingData", prev.size() + ""),
              transformMeta);
    }
    remarks.add(cr);

    // See if we have input streams leading to this transform!
    if (input.length > 0) {
      cr =
          new CheckResult(
              ICheckResult.TYPE_RESULT_OK,
              BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.TransformRecevingData2"),
              transformMeta);
    } else {
      cr =
          new CheckResult(
              ICheckResult.TYPE_RESULT_ERROR,
              BaseMessages.getString(
                  PKG, "CloneRowMeta.CheckResult.NoInputReceivedFromOtherTransforms"),
              transformMeta);
    }
    remarks.add(cr);
  }
}
