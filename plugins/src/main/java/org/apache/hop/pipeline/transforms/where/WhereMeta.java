/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.pipeline.transforms.where;

import org.apache.hop.core.CheckResult;
import org.apache.hop.core.Const;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.annotations.Transform;
import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.exception.HopXmlException;
import org.apache.hop.core.injection.Injection;
import org.apache.hop.core.injection.InjectionSupported;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.core.xml.XmlHandler;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransformMeta;
import org.apache.hop.pipeline.transform.ITransformIOMeta;
import org.apache.hop.pipeline.transform.TransformIOMeta;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.pipeline.transform.stream.IStream;
import org.apache.hop.pipeline.transform.stream.IStream.StreamType;
import org.apache.hop.pipeline.transform.stream.Stream;
import org.apache.hop.pipeline.transform.stream.StreamIcon;
import org.w3c.dom.Node;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * This transform where filter rows with expression and keeps only rows where this expression is
 * true.
 */
@Transform(
    id = "Where",
    image = "where.svg",  
    name = "i18n::Where.Name",
    description = "i18n::Where.Description",
    categoryDescription = "i18n:org.apache.hop.pipeline.transform:BaseTransform.Category.Flow",
    documentationUrl = "/pipeline/transforms/where.html",
    keywords = "i18n::Where.Keywords")
@InjectionSupported(localizationPrefix = "Where.Injection.")
public class WhereMeta extends BaseTransformMeta<Where, WhereData> {

  private static final Class<?> PKG = WhereMeta.class; // for i18n purposes

  /** Constants */
  private static final String TAG_EXPRESSION = "expression";
  private static final String TAG_SEND_TRUE_TO = "send_true_to";
  private static final String TAG_SEND_FALSE_TO = "send_false_to";

  @Injection(name = "EXPRESSION")
  private String expression;

  public WhereMeta() {
    super();
  }

  public WhereMeta(WhereMeta other) {
    super();
    this.expression= other.expression;
    this.setTrueTransformName(other.getTrueTransformName());
    this.setFalseTransformName(other.getFalseTransformName());
  }
  
  @Override
  public void setDefault() {
    this.expression = "";
  }

  @Override
  public Object clone() {
    return new WhereMeta(this);
  }

  @Override
  public String getXml() throws HopValueException {

    StringBuilder xml = new StringBuilder(500);

    xml.append(XmlHandler.addTagValue(TAG_EXPRESSION, expression));
    xml.append(XmlHandler.addTagValue(TAG_SEND_TRUE_TO, getTrueTransformName()));
    xml.append(XmlHandler.addTagValue(TAG_SEND_FALSE_TO, getFalseTransformName()));
    return xml.toString();
  }

  @Override
  public void loadXml(Node stepNode, IHopMetadataProvider metadataProvider) throws HopXmlException {

    try {
      this.expression = XmlHandler.getTagValue(stepNode, TAG_EXPRESSION);
      this.setTrueTransformName(XmlHandler.getTagValue(stepNode, TAG_SEND_TRUE_TO));
      this.setFalseTransformName(XmlHandler.getTagValue(stepNode, TAG_SEND_FALSE_TO));
    } catch (Exception e) {
      throw new HopXmlException(
          BaseMessages.getString(PKG, "WhereMeta.Exception.UnableToReadStepInfoFromXML"), e);
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

    // See if there filter expression
    if (Utils.isEmpty(this.getExpression())) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_WARNING,
              BaseMessages.getString(PKG, "WhereMeta.CheckResult.EmptyFilterExpression"),
              transformMeta));
    }

    checkTarget(transformMeta, true, getTrueTransformName(), output).ifPresent(remarks::add);
    checkTarget(transformMeta, false, getFalseTransformName(), output).ifPresent(remarks::add);

    // Look up fields in the input stream <prev>
    if (prev != null && prev.size() > 0) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_OK,
              BaseMessages.getString(
                  PKG, "WhereMeta.CheckResult.StepReceivingFields", prev.size() + ""),
              transformMeta));

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
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_ERROR,
              BaseMessages.getString(
                  PKG, "WhereMeta.CheckResult.CouldNotReadFieldsFromPreviousTransform"),
              transformMeta));
    }

    // See if we have input streams leading to this transform!
    if (input.length > 0) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_OK,
              BaseMessages.getString(
                  PKG, "WhereMeta.CheckResult.StepReceivingInfoFromOtherTransforms"),
              transformMeta));

    } else {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_ERROR,
              BaseMessages.getString(
                  PKG, "WhereMeta.CheckResult.NoInputReceivedFromOtherTransforms"),
              transformMeta));
    }
  }

  private Optional<ICheckResult> checkTarget(
      TransformMeta transformMeta, boolean target, String targetTransformName, String[] output) {
    if (targetTransformName != null) {
      int trueTargetIdx = Const.indexOfString(targetTransformName, output);
      if (trueTargetIdx < 0) {
        return Optional.of(
            new CheckResult(
                ICheckResult.TYPE_RESULT_ERROR,
                BaseMessages.getString(
                    PKG,
                    "WhereMeta.CheckResult.TargetTransformInvalid",
                    target,
                    targetTransformName),
                transformMeta));
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
  public void searchInfoAndTargetTransforms(List<TransformMeta> transforms) {
    for (IStream stream : getTransformIOMeta().getTargetStreams()) {
      stream.setTransformMeta(
          TransformMeta.findTransform(transforms, stream.getSubject()));
    }
  }

  @Override
  public void resetTransformIoMeta() {
    // super.resetStepIoMeta();
  }

  /** Returns the Input/Output metadata for this step. */
  @Override
  public ITransformIOMeta getTransformIOMeta() {

    ITransformIOMeta transformIOMeta = super.getTransformIOMeta();
    if (transformIOMeta.isInputOptional()) {

      // ioMeta = new StepIOMeta(true, true, false, false, false, false);
      ((TransformIOMeta) transformIOMeta).setInputOptional(false);

      transformIOMeta.addStream(
          new Stream(
              StreamType.TARGET,
              null,
              BaseMessages.getString(PKG, "WhereMeta.TargetStream.True.Description"),
              StreamIcon.TRUE,
              null));
      transformIOMeta.addStream(
          new Stream(
              StreamType.TARGET,
              null,
              BaseMessages.getString(PKG, "WhereMeta.TargetStream.False.Description"),
              StreamIcon.FALSE,
              null));
    }

    return transformIOMeta;
  }

  private String getTargetTransformName(int streamIndex) {
    IStream stream = getTransformIOMeta().getTargetStreams().get(streamIndex);
    return java.util.stream.Stream.of(stream.getTransformName(), stream.getSubject())
        .filter(Objects::nonNull)
        .findFirst()
        .map(Object::toString)
        .orElse(null);
  }

  public String getTrueTransformName() {
    return getTargetTransformName(0);
  }

  @Injection(name = "TRUE_TARGET_TRANSFORM_NAME")
  public void setTrueTransformName(final String transformName) {
    List<IStream> targetStreams = getTransformIOMeta().getTargetStreams();
    targetStreams.get(0).setSubject(transformName);
  }

  public String getFalseTransformName() {
    return getTargetTransformName(1);
  }

  @Injection(name = "FALSE_TARGET_TRANSFORM_NAME")
  public void setFalseTransformName(final String transformName) {
    List<IStream> targetStreams = getTransformIOMeta().getTargetStreams();
    targetStreams.get(1).setSubject(transformName);
  }

  /**
   * When an optional stream is selected, this method is called to handled the ETL metadata
   * implications of that.
   *
   * @param stream The optional stream to handle.
   */
  @Override
  public void handleStreamSelection(final IStream stream) {
    // This transform targets another transform.
    // Make sure that we don't specify the same step for true and false...
    // If the user requests false, we blank out true and vice versa
    //
    List<IStream> targets = getTransformIOMeta().getTargetStreams();
    int index = targets.indexOf(stream);
    if (index == 0) {
      // True
      TransformMeta falseTransform = targets.get(1).getTransformMeta();
      if (falseTransform != null && falseTransform.equals(stream.getTransformMeta())) {
        targets.get(1).setTransformMeta(null);
      }
    }
    if (index == 1) {
      // False
      TransformMeta trueTransform = targets.get(0).getTransformMeta();
      if (trueTransform != null && trueTransform.equals(stream.getTransformMeta())) {
        targets.get(0).setTransformMeta(null);
      }
    }
  }

  @Override
  public boolean excludeFromCopyDistributeVerification() {
    return true;
  }
}
