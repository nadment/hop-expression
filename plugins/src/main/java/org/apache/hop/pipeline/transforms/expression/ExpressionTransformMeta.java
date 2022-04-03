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
package org.apache.hop.pipeline.transforms.expression;

import org.apache.hop.core.CheckResult;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.annotations.Transform;
import org.apache.hop.core.exception.HopTransformException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaFactory;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.HopMetadataProperty;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransformMeta;
import org.apache.hop.pipeline.transform.TransformMeta;
import java.util.ArrayList;
import java.util.List;

/**
 * This transform create field value with expression.
 */
@Transform(
    id = "Expression",
    name = "i18n::Expression.Name",
    description = "i18n::Expression.Description",
    image = "expression.svg",
    categoryDescription = "i18n:org.apache.hop.pipeline.transform:BaseTransform.Category.Scripting",
    documentationUrl = "/pipeline/transforms/expresssion.html",
    keywords = "i18n::Expression.Keywords")
public class ExpressionTransformMeta extends BaseTransformMeta<ExpressionTransform, ExpressionTransformData> {

  private static final Class<?> PKG = ExpressionTransformMeta.class;
 
  @HopMetadataProperty(groupKey = "fields", key = "field", injectionGroupDescription = "ExpressionMeta.Injection.Fields", injectionKeyDescription = "ExpressionMeta.Injection.Field")
  private List<ExpressionField> fields;

  public ExpressionTransformMeta() {
    super();
  }
  
  public ExpressionTransformMeta(ExpressionTransformMeta other) {
    super();
    
    this.fields = new ArrayList<>();
    for (ExpressionField field : other.getFields()) {
      fields.add(new ExpressionField(field));
    }
  }

  @Override
  public void setDefault() {
    this.fields = new ArrayList<>();
  }

  @Override
  public Object clone() {
    return new ExpressionTransformMeta(this);
  }

  @Override
  public void getFields(
      IRowMeta rowMeta,
      String transformName,
      IRowMeta[] info,
      TransformMeta nextTransform,
      IVariables variables,
      IHopMetadataProvider metadataProvider)
      throws HopTransformException {
    try {
      // store the input stream meta
      IRowMeta unalteredInputRowMeta = rowMeta.clone();

      // add the output fields if specified
      for (ExpressionField field : this.getFields()) {
        if (!Utils.isEmpty(field.getName())) {

          // create ValueMeta
          IValueMeta vm = ValueMetaFactory.createValueMeta(field.getName(), ValueMetaFactory.getIdForValueMeta(field.getType()));
          vm.setOrigin(transformName);
          vm.setLength(field.getLength(), field.getPrecision());

          // field already exist
          int index = unalteredInputRowMeta.indexOfValue(field.getName());
          if (index >= 0) {
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

    // Look up fields in the input stream <prev>
    if (prev != null && prev.size() > 0) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_OK,
              BaseMessages.getString(
                  PKG,
                  "ExpressionMeta.CheckResult.ReceivingFieldsFromPreviousTransforms",
                  prev.size() + ""),
              transformMeta));
    } else {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_ERROR,
              BaseMessages.getString(
                  PKG, "ExpressionMeta.CheckResult.NotReceivingFieldsFromPreviousTransforms"),
              transformMeta));
    }

    // See if we have input streams leading to this transform!
    if (input.length > 0) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_OK,
              BaseMessages.getString(
                  PKG, "ExpressionMeta.CheckResult.ReceivingInfoFromOtherTransforms"),
              transformMeta));

    } else {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_ERROR,
              BaseMessages.getString(
                  PKG, "ExpressionMeta.CheckResult.NotReceivingInfoFromOtherTransforms"),
              transformMeta));
    }

    // Check expression
    for (ExpressionField field : this.fields) {
      try {
        ExpressionParser.parse(field.getExpression());
      } catch (Exception e) {
        remarks.add(
            new CheckResult(
                ICheckResult.TYPE_RESULT_ERROR,
                BaseMessages.getString(
                    PKG,
                    "ExpressionMeta.CheckResult.InvalidExpression",
                    field.getName(),
                    e.getMessage()),
                transformMeta));
      }
    }
  }

  public List<ExpressionField> getFields() {
    return this.fields;
  }

  public void setFields(final List<ExpressionField> values) {
    this.fields = values;
  }
}
