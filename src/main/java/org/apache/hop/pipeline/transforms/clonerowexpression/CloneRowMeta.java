/*
 * ! ******************************************************************************
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
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 ******************************************************************************/

package org.apache.hop.pipeline.transforms.clonerowexpression;

import java.util.List;
import lombok.Getter;
import lombok.Setter;
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
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionFactory;
import org.apache.hop.expression.IExpression;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.HopMetadataProperty;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransformMeta;
import org.apache.hop.pipeline.transform.TransformMeta;

@Setter
@Getter
@Transform(
    id = "CloneRowExpression",
    name = "CloneRow.Name",
    description = "CloneRow.Description",
    image = "clonerow.svg",
    categoryDescription = "i18n:org.apache.hop.pipeline.transform:BaseTransform.Category.Utility")
public class CloneRowMeta extends BaseTransformMeta<CloneRow, CloneRowData> {

  private static final Class<?> PKG =
      CloneRowMeta.class; // for i18n purposes, needed by Translator!!

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
  public void setDefault() {
    nrClones = "0";
    cloneFlagField = null;
    addCloneFlag = false;
    addCloneNum = false;
    cloneNumField = null;
  }

  public String evaluate(String source, IVariables variables) throws HopTransformException {
    String value = variables.resolve(source);

    // If value start with '=' this is an expression
    if (value.charAt(0) != '=') {
      return value;
    }

    try {
      IExpression expression =
          ExpressionFactory.create(new ExpressionContext(new Variables()), value.substring(1));
      Object result = expression.getValue();
      return String.valueOf(result);
    } catch (ExpressionException e) {
      throw new HopTransformException(
          BaseMessages.getString(PKG, "Unable to compile expression ''{0}''", source), e);
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

    if (Utils.isEmpty(nrClones)) {
      cr =
          new CheckResult(
              ICheckResult.TYPE_RESULT_ERROR,
              BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.NrClonesdMissing"),
              transformMeta);
    } else {
      // TODO: validate expression
      cr = new CheckResult(ICheckResult.TYPE_RESULT_OK, BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.NrClonesOK"), transformMeta);
    }
    remarks.add(cr);

    if (addCloneFlag) {
      if (Utils.isEmpty(cloneFlagField)) {
        cr = new CheckResult(ICheckResult.TYPE_RESULT_ERROR, BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.CloneFlagFieldMissing"), transformMeta);
      } else {
        // TODO: validate expression
        cr = new CheckResult(ICheckResult.TYPE_RESULT_OK, BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.CloneFlagFieldOk"), transformMeta);
      }
      remarks.add(cr);
    }

    if (addCloneNum) {
      if (Utils.isEmpty(cloneNumField)) {
        cr = new CheckResult(ICheckResult.TYPE_RESULT_ERROR, BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.CloneNumFieldMissing"), transformMeta);
      } else {
        // TODO: validate expression
        cr = new CheckResult(ICheckResult.TYPE_RESULT_OK, BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.CloneNumFieldOk"), transformMeta);
      }
      remarks.add(cr);
    }

    if (prev == null || prev.isEmpty()) {
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
