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
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.annotations.Transform;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.type.TypeName;
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

  /**
   * The target transform name if condition evaluate to true. The name is kept in the target stream,
   * this field only declares the metadata property.
   */
  @HopMetadataProperty(
      key = "send_true_to",
      injectionKey = "TRUE_TARGET_TRANSFORM_NAME",
      injectionKeyDescription = "WhereMeta.Injection.SEND_TRUE_TRANSFORM")
  private String trueTransformName;

  /**
   * The target transform name if condition evaluate to false. The name is kept in the target
   * stream, this field only declares the metadata property.
   */
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

    // Check if the expression is valide, the fields of the input stream are required
    else if (prev != null)
      try {
        RowExpressionContext context = new RowExpressionContext(variables, prev);
        IExpression expression = IExpression.of(context, variables.resolve(getCondition()));

        // The value of the condition must be converted to a Boolean without error
        TypeName typeName = expression.getType().getName();
        if (!typeName.isCoercible(TypeName.BOOLEAN)) {
          remarks.add(
              new CheckResult(
                  ICheckResult.TYPE_RESULT_ERROR,
                  BaseMessages.getString(
                      PKG, "WhereMeta.CheckResult.ConditionNotBoolean", typeName),
                  transformMeta));
        }
      } catch (ExpressionException e) {
        remarks.add(new CheckResult(ICheckResult.TYPE_RESULT_ERROR, e.getMessage(), transformMeta));
      }

    // Without TRUE or FALSE target, the rows are sent to the main output: no target to check
  }

  /**
   * Returns the name of the transform receiving the rows where the condition is true.
   *
   * @return the name of the transform, or null without TRUE target
   */
  public String getTrueTransformName() {
    return getTargetTransformName(getTransformIOMeta().getTargetStreams().getFirst());
  }

  /**
   * Sets the name of the transform receiving the rows where the condition is true. The transform is
   * found by {@link #searchInfoAndTargetTransforms(List)}.
   *
   * @param name the name of the transform
   */
  public void setTrueTransformName(String name) {
    setTargetTransformName(getTransformIOMeta().getTargetStreams().getFirst(), name);
  }

  /**
   * Returns the name of the transform receiving the rows where the condition is false or NULL.
   *
   * @return the name of the transform, or null without FALSE target
   */
  public String getFalseTransformName() {
    return getTargetTransformName(getTransformIOMeta().getTargetStreams().get(1));
  }

  /**
   * Sets the name of the transform receiving the rows where the condition is false or NULL. The
   * transform is found by {@link #searchInfoAndTargetTransforms(List)}.
   *
   * @param name the name of the transform
   */
  public void setFalseTransformName(String name) {
    setTargetTransformName(getTransformIOMeta().getTargetStreams().get(1), name);
  }

  /**
   * Returns the name of the target transform of a stream: the transform selected for the hop, or
   * the name read from the metadata until the transform is found.
   */
  private static String getTargetTransformName(IStream stream) {
    String name = stream.getTransformName();
    return name != null ? name : stream.getSubject();
  }

  /** Sets the name of the target transform of a stream, the transform is found later. */
  private static void setTargetTransformName(IStream stream, String name) {
    stream.setSubject(name);
    stream.setTransformMeta(null);
  }

  /**
   * The streams keep the names of the target transforms, they must not be reset. Hop GUI resets the
   * Input/Output metadata after the dialog, which would lose the TRUE and FALSE targets.
   */
  @Override
  public void resetTransformIoMeta() {
    // Do nothing
  }

  @Override
  public void searchInfoAndTargetTransforms(List<TransformMeta> transforms) {
    // The names are kept in the streams, they follow the transforms selected for the hops
    for (IStream stream : getTransformIOMeta().getTargetStreams()) {
      stream.setTransformMeta(
          TransformMeta.findTransform(transforms, getTargetTransformName(stream)));
    }
  }

  /** Returns the Input/Output metadata for this transform. */
  @Override
  public ITransformIOMeta getTransformIOMeta() {
    ITransformIOMeta ioMeta = super.getTransformIOMeta(false);

    if (ioMeta == null) {
      // The main output receives the rows where the condition is true, without TRUE or FALSE target
      ioMeta = new TransformIOMeta(true, true, false, false, false, false);
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
    // The name kept in the stream is also cleared, otherwise the target would be found again
    //
    List<IStream> targets = getTransformIOMeta().getTargetStreams();
    int index = targets.indexOf(stream);
    if (index == 0) {
      // True
      TransformMeta falseTransform = targets.get(1).getTransformMeta();
      if (falseTransform != null && falseTransform.equals(stream.getTransformMeta())) {
        setTargetTransformName(targets.get(1), null);
      }
    }
    if (index == 1) {
      // False
      TransformMeta trueTransform = targets.getFirst().getTransformMeta();
      if (trueTransform != null && trueTransform.equals(stream.getTransformMeta())) {
        setTargetTransformName(targets.getFirst(), null);
      }
    }
  }

  @Override
  public boolean cleanAfterHopFromRemove() {
    return true;
  }

  @Override
  public boolean cleanAfterHopFromRemove(TransformMeta toTransform) {
    for (IStream stream : getTransformIOMeta().getTargetStreams()) {
      // A stream without target transform has no transform meta
      if (toTransform.equals(stream.getTransformMeta())) {
        stream.setSubject(null);
        stream.setTransformMeta(null);
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean excludeFromCopyDistributeVerification() {
    return true;
  }
}
