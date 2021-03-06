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
import org.apache.hop.core.exception.HopXmlException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.xml.XmlHandler;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.IExpression;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransformMeta;
import org.apache.hop.pipeline.transform.ITransformMeta;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.w3c.dom.Node;
import java.util.List;

/*
 * Created on 27-06-2008
 *
 */
@Transform(
    id = "CloneRowExpression",
    name = "CloneRow.Name",
    description = "CloneRow.Description",
    image = "clonerow.svg",
    categoryDescription = "i18n:org.apache.hop.pipeline.transform:BaseTransform.Category.Utility"
    )
public class CloneRowMeta extends BaseTransformMeta
    implements ITransformMeta<CloneRow, CloneRowData> {

  private static Class<?> PKG = CloneRowMeta.class; // for i18n purposes, needed by Translator!!

  /** nr of clone rows */
  private String nrclones;

  /** Flag: add clone flag */
  private boolean addcloneflag;

  /** clone flag field */
  private String cloneflagfield;

  private boolean addclonenum;
  private String clonenumfield;

  public CloneRowMeta() {
    super(); // allocate BaseTransformMeta
  }

  @Override
  public String getXml() {
    StringBuilder retval = new StringBuilder();
    retval.append("    " + XmlHandler.addTagValue("nrclones", nrclones));
    retval.append("    " + XmlHandler.addTagValue("addcloneflag", addcloneflag));
    retval.append("    " + XmlHandler.addTagValue("cloneflagfield", cloneflagfield));
    retval.append("    " + XmlHandler.addTagValue("addclonenum", addclonenum));
    retval.append("    " + XmlHandler.addTagValue("clonenumfield", clonenumfield));

    return retval.toString();
  }

  @Override
  public void loadXml(Node transformNode, IHopMetadataProvider metadataProvider)
      throws HopXmlException {
    readData(transformNode);
  }

  public Object clone() {
    Object retval = super.clone();
    return retval;
  }

  public String getNrClones() {
    return nrclones;
  }

  public void setNrClones(String nrclones) {
    this.nrclones = nrclones;
  }

  public boolean isAddCloneFlag() {
    return addcloneflag;
  }

  public void setAddCloneFlag(boolean addcloneflag) {
    this.addcloneflag = addcloneflag;
  }

  public boolean isAddCloneNum() {
    return addclonenum;
  }

  public void setAddCloneNum(boolean addclonenum) {
    this.addclonenum = addclonenum;
  }

  public String getCloneNumField() {
    return clonenumfield;
  }

  public void setCloneNumField(String clonenumfield) {
    this.clonenumfield = clonenumfield;
  }

  public String getCloneFlagField() {
    return cloneflagfield;
  }

  public void setCloneFlagField(String cloneflagfield) {
    this.cloneflagfield = cloneflagfield;
  }

  private void readData(Node transformNode) throws HopXmlException {
    try {
      nrclones = XmlHandler.getTagValue(transformNode, "nrclones");
      addcloneflag = "Y".equalsIgnoreCase(XmlHandler.getTagValue(transformNode, "addcloneflag"));
      cloneflagfield = XmlHandler.getTagValue(transformNode, "cloneflagfield");
      addclonenum = "Y".equalsIgnoreCase(XmlHandler.getTagValue(transformNode, "addclonenum"));
      clonenumfield = XmlHandler.getTagValue(transformNode, "clonenumfield");

    } catch (Exception e) {
      throw new HopXmlException(
          BaseMessages.getString(PKG, "CloneRowMeta.Exception.UnableToReadTransformMeta"), e);
    }
  }

  @Override
  public void setDefault() {
    nrclones = "0";
    cloneflagfield = null;
    addcloneflag = false;
    addclonenum = false;
    clonenumfield = null;
  }

  public String evaluate(String source, IVariables variables) throws HopTransformException {
    String value = variables.resolve(source);

    // If value start with '='  this is a expression
    if (value.charAt(0) != '=') {
      return value;
    }

    try {
      IExpression expression = ExpressionParser.parse(value.substring(1));
      Object result = expression.eval(new ExpressionContext(variables));      
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
    if (addcloneflag) {
      String realfieldValue = String.valueOf(evaluate(cloneflagfield, variables));
      if (!Utils.isEmpty(realfieldValue)) {
        IValueMeta v = new ValueMetaBoolean(realfieldValue);
        v.setOrigin(origin);
        rowMeta.addValueMeta(v);
      }
    }
    // Output clone row number
    if (addclonenum) {
      String realfieldValue = evaluate(clonenumfield, variables);
      if (!Utils.isEmpty(realfieldValue)) {
        IValueMeta v = new ValueMetaInteger(realfieldValue);
        v.setOrigin(origin);
        rowMeta.addValueMeta(v);
      }
    }
  }

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

    if (Utils.isEmpty(nrclones)) {
      error_message = BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.NrClonesdMissing");
      cr = new CheckResult(ICheckResult.TYPE_RESULT_ERROR, error_message, transformMeta);
    } else {
      // TODO: validate expression
      error_message = BaseMessages.getString(PKG, "CloneRowMeta.CheckResult.NrClonesOK");
      cr = new CheckResult(ICheckResult.TYPE_RESULT_OK, error_message, transformMeta);
    }
    remarks.add(cr);

    if (addcloneflag) {
      if (Utils.isEmpty(cloneflagfield)) {
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

    if (addclonenum) {
      if (Utils.isEmpty(clonenumfield)) {
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

  @Override
  public CloneRow createTransform(
      TransformMeta transformMeta, CloneRowData data, int cnr, PipelineMeta tr, Pipeline pipeline) {
    return new CloneRow(transformMeta, this, data, cnr, tr, pipeline);
  }

  @Override
  public CloneRowData getTransformData() {
    return new CloneRowData();
  }
}
