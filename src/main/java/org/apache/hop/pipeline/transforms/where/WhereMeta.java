/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.hop.pipeline.transforms.where;

import java.util.List;
import lombok.Getter;
import lombok.Setter;
import org.apache.hop.core.CheckResult;
import org.apache.hop.core.Const;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.annotations.Transform;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionFactory;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.HopMetadataProperty;
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

/** This transform filter rows with expression and keeps only rows where this expression is true. */
@Setter
@Getter
@Transform(
    id = "Where",
    image = "where.svg",
    name = "i18n::Where.Name",
    description = "i18n::Where.Description",
    categoryDescription = "i18n:org.apache.hop.pipeline.transform:BaseTransform.Category.Flow",
    documentationUrl = "/pipeline/transforms/where.html",
    keywords = "i18n::Where.Keywords")
public class WhereMeta extends BaseTransformMeta<Where, WhereData> {

  private static final Class<?> PKG = WhereMeta.class; // for i18n purposes

  /** The target transform name if condition evaluate to true */
  @HopMetadataProperty(
      key = "send_true_to",
      injectionKey = "TRUE_TARGET_TRANSFORM_NAME",
      injectionKeyDescription = "WhereMeta.Injection.SEND_TRUE_TRANSFORM")
  private String trueTransformName;

  /** The target transform name if condition evaluate to false */
  @HopMetadataProperty(
      key = "send_false_to",
      injectionKey = "FALSE_TARGET_TRANSFORM_NAME",
      injectionKeyDescription = "WhereMeta.Injection.SEND_FALSE_TRANSFORM")
  private String falseTransformName;

  /** The condition expression */
  @HopMetadataProperty(
      key = "condition",
      injectionKey = "CONDITION",
      injectionKeyDescription = "WhereMeta.Injection.CONDITION")
  private String condition;

  public WhereMeta() {
    super();
  }

  public WhereMeta(WhereMeta other) {
    super();
    this.condition = other.condition;
    this.setTrueTransformName(other.getTrueTransformName());
    this.setFalseTransformName(other.getFalseTransformName());
  }

  @Override
  public void setDefault() {
    this.condition = "";
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

    // Check if we have input streams leading to this transform
    if (input.length == 0) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_ERROR,
              BaseMessages.getString(
                  PKG, "WhereMeta.CheckResult.NotReceivingInfoFromOtherTransforms"),
              transformMeta));
    }
    // Look up fields in the input stream <prev>
    else if (prev == null || prev.isEmpty()) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_ERROR,
              BaseMessages.getString(
                  PKG, "WhereMeta.CheckResult.NotReceivingFieldsFromPreviousTransforms"),
              transformMeta));
    }

    // Check if there is a condition expression
    if (Utils.isEmpty(this.getCondition())) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_WARNING,
              BaseMessages.getString(PKG, "WhereMeta.CheckResult.EmptyFilterCondition"),
              transformMeta));
    }

    // Check if the expression is valide
    else
      try {
        RowExpressionContext context = new RowExpressionContext(variables, prev);
        ExpressionFactory.create(context, variables.resolve(getCondition()));
      } catch (ExpressionException e) {
        remarks.add(new CheckResult(ICheckResult.TYPE_RESULT_ERROR, e.getMessage(), transformMeta));
      }

    // Check if there is at least one target (true, false or error)
    ITransformIOMeta transformIOMeta = getTransformIOMeta();
    int target = 0;
    for (IStream stream : transformIOMeta.getTargetStreams()) {
      if (stream.getTransformName() != null) {
        target++;
      }
    }
    if (target == 0) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_WARNING,
              "WhereMeta.CheckResult.CheckAtLeastOneTarget",
              transformMeta));
    }
  }

  @Override
  public void convertIOMetaToTransformNames() {
    List<IStream> streams = getTransformIOMeta().getTargetStreams();
    trueTransformName = Const.NVL(streams.get(0).getTransformName(), "");
    falseTransformName = Const.NVL(streams.get(1).getTransformName(), "");
  }

  @Override
  public void searchInfoAndTargetTransforms(List<TransformMeta> transforms) {
    List<IStream> streams = getTransformIOMeta().getTargetStreams();
    streams.get(0).setTransformMeta(TransformMeta.findTransform(transforms, trueTransformName));
    streams.get(1).setTransformMeta(TransformMeta.findTransform(transforms, falseTransformName));
  }

  @Override
  public boolean supportsErrorHandling() {
    return true;
  }

  /** Returns the Input/Output metadata for this transform. */
  @Override
  public ITransformIOMeta getTransformIOMeta() {
    ITransformIOMeta ioMeta = super.getTransformIOMeta(false);

    if (ioMeta == null) {
      ioMeta = new TransformIOMeta(true, false, false, false, false, false);
      ioMeta.addStream(
          new Stream(
              StreamType.TARGET,
              null,
              BaseMessages.getString(PKG, "WhereMeta.TargetStream.True.Description"),
              StreamIcon.TRUE,
              null));
      ioMeta.addStream(
          new Stream(
              StreamType.TARGET,
              null,
              BaseMessages.getString(PKG, "WhereMeta.TargetStream.False.Description"),
              StreamIcon.FALSE,
              null));
      setTransformIOMeta(ioMeta);
    }

    return ioMeta;
  }

  /**
   * When an optional stream is selected, this method is called to handle the ETL metadata
   * implications of that.
   *
   * @param stream The optional stream to handle.
   */
  @Override
  public void handleStreamSelection(final IStream stream) {
    // This transform targets another transform.
    // Make sure that we don't specify the same target transform for true and false...
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
