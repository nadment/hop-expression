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

package org.apache.hop.pipeline.transforms.aggregate;

import org.apache.hop.core.CheckResult;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.annotations.Transform;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Expressions;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IRowExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.Operator;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.HopMetadataProperty;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransformMeta;
import org.apache.hop.pipeline.transform.TransformMeta;
import java.util.ArrayList;
import java.util.List;

@Transform(id = "Aggregate", image = "aggregate.svg", name = "i18n::Aggregate.Name",
    description = "i18n::Aggregate.Description",
    categoryDescription = "i18n:org.apache.hop.pipeline.transform:BaseTransform.Category.Statistics",
    keywords = "i18n::Aggregate.Keywords", documentationUrl = "/pipeline/transforms/aggregate.html")
public class AggregateMeta extends BaseTransformMeta<AggregateTransform, AggregateData> {
  private static final Class<?> PKG = AggregateMeta.class; // For Translator

  /** Fields to group over */
  @HopMetadataProperty(groupKey = "groups", key = "field",
      injectionGroupDescription = "AggregateMeta.Injection.GROUPS")
  private List<GroupField> groupFields;

  @HopMetadataProperty(groupKey = "aggregates", key = "field",
      injectionGroupDescription = "AggregateMeta.Injection.AGGREGATES")
  private List<AggregateField> aggregateFields;

  /** Flag to indicate that we always give back one row. Defaults to true for existing pipelines. */
  @HopMetadataProperty(key = "ALWAYSGIVINGBACKONEROW")
  private boolean alwaysGivingBackOneRow;

  public AggregateMeta() {
    super();
  }

  public AggregateMeta(AggregateMeta other) {
    super();

    this.alwaysGivingBackOneRow = other.alwaysGivingBackOneRow;

    this.groupFields = new ArrayList<>();
    for (GroupField field : other.getGroupFields()) {
      groupFields.add(new GroupField(field.getName()));
    }

    this.aggregateFields = new ArrayList<>();
    for (AggregateField field : other.getAggregateFields()) {
      aggregateFields.add(new AggregateField(field));
    }
  }


  @Override
  public void setDefault() {
    this.groupFields = new ArrayList<>();
    this.aggregateFields = new ArrayList<>();
  }

  /**
   * @return Returns the list of aggregates.
   */
  public List<AggregateField> getAggregateFields() {
    return aggregateFields;
  }

  /**
   * @param aggregates The list of aggregates to set.
   */
  public void setAggregateFields(List<AggregateField> aggregates) {
    this.aggregateFields = aggregates;
  }

  /**
   * @return Returns the group by field.
   */
  public List<GroupField> getGroupFields() {
    return groupFields;
  }

  /**
   * @param groupField The group by field to set.
   */
  public void setGroupFields(List<GroupField> groups) {
    this.groupFields = groups;
  }

  @Override
  public void getFields(IRowMeta rowMeta, String transformName, IRowMeta[] info,
      TransformMeta nextTransform, IVariables variables, IHopMetadataProvider metadataProvider) {

    // re-assemble a new row of metadata
    //
    IRowMeta fields = new RowMeta();

    // Add the grouping fields in the correct order...
    //
    for (GroupField field : groupFields) {
      IValueMeta valueMeta = rowMeta.searchValueMeta(field.getName());
      if (valueMeta != null) {
        valueMeta.setStorageType(IValueMeta.STORAGE_TYPE_NORMAL);
        fields.addValueMeta(valueMeta);
      }
    }

    // Add aggregates
    //
    for (AggregateField field : aggregateFields) {

      IRowExpressionContext context = new RowExpressionContext(variables, rowMeta);
      // Compile expression
      try {
        IExpression expression = Expressions.build(context, field.getExpression());
        if (!expression.is(Kind.CALL)) {
          throw new ExpressionException("Not an aggregation expression");
        }
        Operator operator = ((Call) expression).getOperator();
        if (!(operator instanceof AggregateFunction)) {
          throw new ExpressionException("Not an aggregation expression");
        }

        IValueMeta valueMeta = Expressions.createValueMeta(field.getName(), expression.getType());
        valueMeta.setOrigin(transformName);
        valueMeta.setStorageType(IValueMeta.STORAGE_TYPE_NORMAL);
        fields.addValueMeta(valueMeta);
      } catch (ExpressionException e) {
        String message = BaseMessages.getString(PKG, "Aggregate.Exception.ExpressionError",
            field.getName(), field.getExpression(), e.getMessage());
        this.logError(message);
      }
    }

    // Now that we have all the fields we want, we should clear the original row and replace the
    // values...
    //
    rowMeta.clear();
    rowMeta.addRowMeta(fields);
  }


  @Override
  public void check(List<ICheckResult> remarks, PipelineMeta pipelineMeta,
      TransformMeta transformMeta, IRowMeta prev, String[] input, String[] output, IRowMeta info,
      IVariables variables, IHopMetadataProvider metadataProvider) {
    CheckResult cr;

    if (input.length > 0) {
      cr = new CheckResult(ICheckResult.TYPE_RESULT_OK,
          BaseMessages.getString(PKG, "AggregateMeta.CheckResult.ReceivingInfoOK"), transformMeta);
      remarks.add(cr);
    } else {
      cr = new CheckResult(ICheckResult.TYPE_RESULT_ERROR,
          BaseMessages.getString(PKG, "AggregateMeta.CheckResult.NoInputError"), transformMeta);
      remarks.add(cr);
    }
  }

  /**
   * @return the alwaysGivingBackOneRow
   */
  public boolean isAlwaysGivingBackOneRow() {
    return alwaysGivingBackOneRow;
  }

  /**
   * @param alwaysGivingBackOneRow the alwaysGivingBackOneRow to set
   */
  public void setAlwaysGivingBackOneRow(boolean alwaysGivingBackOneRow) {
    this.alwaysGivingBackOneRow = alwaysGivingBackOneRow;
  }
}
