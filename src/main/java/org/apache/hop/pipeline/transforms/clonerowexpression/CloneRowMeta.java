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
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.type.TypeName;
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
          IExpression.of(new ExpressionContext(new Variables()), value.substring(1));
      Object result = expression.getValue();
      return String.valueOf(result);
    } catch (ExpressionException e) {
      throw new HopTransformException(
          BaseMessages.getString(PKG, "CloneRow.Error.UnableToCompileExpression", source), e);
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

    // Only the errors and the warnings are reported, a valid setting gives no remark
    if (Utils.isEmpty(nrClones)) {
      remarks.add(error(transformMeta, "CloneRowMeta.CheckResult.NrClonesdMissing"));
    } else {
      checkNrClones(remarks, transformMeta, prev, variables);
    }

    // The names of the output fields, not expressions
    if (addCloneFlag && Utils.isEmpty(cloneFlagField)) {
      remarks.add(error(transformMeta, "CloneRowMeta.CheckResult.CloneFlagFieldMissing"));
    }
    if (addCloneNum && Utils.isEmpty(cloneNumField)) {
      remarks.add(error(transformMeta, "CloneRowMeta.CheckResult.CloneNumFieldMissing"));
    }

    if (prev == null || prev.isEmpty()) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_WARNING,
              BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.NotReceivingFields"),
              transformMeta));
    }

    // See if we have input streams leading to this transform!
    if (input.length == 0) {
      remarks.add(
          error(transformMeta, "CloneRowMeta.CheckResult.NoInputReceivedFromOtherTransforms"));
    }
  }

  /** Returns an error of the check with a message of this transform. */
  private static CheckResult error(TransformMeta transformMeta, String key, Object... values) {
    return new CheckResult(
        ICheckResult.TYPE_RESULT_ERROR, BaseMessages.getString(PKG, key, values), transformMeta);
  }

  /**
   * Checks that the expression of the number of clones is valid and gives an integer. The fields of
   * the input stream are required to compile the expression, without them it is not checked.
   *
   * @param remarks the remarks to add an error to
   * @param transformMeta the transform to check
   * @param prev the fields of the input stream, or {@code null} if unknown
   * @param variables the variables to resolve in the expression
   */
  private void checkNrClones(
      List<ICheckResult> remarks,
      TransformMeta transformMeta,
      IRowMeta prev,
      IVariables variables) {
    if (prev == null) {
      return;
    }
    try {
      RowExpressionContext context = new RowExpressionContext(variables, prev);
      IExpression expression = IExpression.of(context, variables.resolve(nrClones));

      // A number is not coerced to an integer, it could lose its decimal part
      TypeName typeName = expression.getType().getName();
      if (!typeName.isCoercible(TypeName.INTEGER)) {
        remarks.add(error(transformMeta, "CloneRowMeta.CheckResult.NrClonesNotInteger", typeName));
      }
    } catch (ExpressionException e) {
      remarks.add(new CheckResult(ICheckResult.TYPE_RESULT_ERROR, e.getMessage(), transformMeta));
    }
  }
}
