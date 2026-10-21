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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import lombok.Getter;
import lombok.Setter;
import org.apache.hop.core.CheckResult;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.annotations.Transform;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IRowExpressionContext;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.type.Types;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.HopMetadataProperty;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransformMeta;
import org.apache.hop.pipeline.transform.TransformMeta;

@Setter
@Getter
@Transform(
    id = "Aggregate",
    image = "aggregate.svg",
    name = "i18n::Aggregate.Name",
    description = "i18n::Aggregate.Description",
    categoryDescription =
        "i18n:org.apache.hop.pipeline.transform:BaseTransform.Category.Statistics",
    keywords = "i18n::Aggregate.Keywords",
    documentationUrl = "/pipeline/transforms/aggregate.html")
public class AggregateMeta extends BaseTransformMeta<AggregateTransform, AggregateData> {
  private static final Class<?> PKG = AggregateMeta.class; // For Translator

  /** Fields to group over */
  @HopMetadataProperty(
      groupKey = "groups",
      key = "field",
      injectionGroupDescription = "AggregateMeta.Injection.GROUPS")
  private List<GroupField> groupFields;

  /** The aggregate fields */
  @HopMetadataProperty(
      groupKey = "aggregates",
      key = "field",
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

  @Override
  public void getFields(
      IRowMeta rowMeta,
      String transformName,
      IRowMeta[] info,
      TransformMeta nextTransform,
      IVariables variables,
      IHopMetadataProvider metadataProvider) {

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
    IRowExpressionContext context = new RowExpressionContext(variables, rowMeta);
    for (AggregateField field : aggregateFields) {
      // Compile expression, with the variables resolved like the transform
      try {
        IExpression expression = compileAggregate(context, variables, field.getExpression());

        IValueMeta valueMeta = Types.createValueMeta(field.getName(), expression.getType());
        valueMeta.setOrigin(transformName);
        valueMeta.setStorageType(IValueMeta.STORAGE_TYPE_NORMAL);
        fields.addValueMeta(valueMeta);
      } catch (ExpressionException e) {
        String message =
            BaseMessages.getString(
                PKG,
                "Aggregate.Exception.ExpressionError",
                field.getName(),
                field.getExpression(),
                e.getMessage());
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

    if (input.length == 0) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_ERROR,
              BaseMessages.getString(PKG, "AggregateMeta.CheckResult.NoInputError"),
              transformMeta));
    }

    // A field written several times is renamed in the output row, the names are not case-sensitive
    Set<String> names = new TreeSet<>(String.CASE_INSENSITIVE_ORDER);
    for (GroupField field : groupFields) {
      checkDuplicateName(remarks, transformMeta, names, field.getName());
    }
    for (AggregateField field : aggregateFields) {
      checkDuplicateName(remarks, transformMeta, names, field.getName());
    }

    // Check the aggregate expressions, the fields of the input stream are required to compile them
    IRowExpressionContext context =
        (prev == null) ? null : new RowExpressionContext(variables, prev);
    for (AggregateField field : aggregateFields) {
      if (Utils.isEmpty(field.getExpression())) {
        remarks.add(
            new CheckResult(
                ICheckResult.TYPE_RESULT_ERROR,
                BaseMessages.getString(
                    PKG, "AggregateMeta.CheckResult.ExpressionEmpty", field.getName()),
                transformMeta));
      } else if (context != null) {
        try {
          compileAggregate(context, variables, field.getExpression());
        } catch (ExpressionException e) {
          remarks.add(
              new CheckResult(
                  ICheckResult.TYPE_RESULT_ERROR,
                  BaseMessages.getString(
                      PKG,
                      "AggregateMeta.CheckResult.ExpressionError",
                      field.getName(),
                      e.getMessage()),
                  transformMeta));
        }
      }
    }
  }

  /** Reports an error if the name of a field of the output row is already used. */
  private static void checkDuplicateName(
      List<ICheckResult> remarks, TransformMeta transformMeta, Set<String> names, String name) {
    if (!Utils.isEmpty(name) && !names.add(name)) {
      remarks.add(
          new CheckResult(
              ICheckResult.TYPE_RESULT_ERROR,
              BaseMessages.getString(PKG, "AggregateMeta.CheckResult.DuplicateField", name),
              transformMeta));
    }
  }

  /**
   * Compiles an aggregate expression, with the variables resolved like the transform.
   *
   * @param context the context with the input fields
   * @param variables the variables to resolve in the expression
   * @param source the aggregate expression
   * @return the compiled expression, a call to an aggregate function
   * @throws ExpressionException if the expression is invalid or is not a call to an aggregate
   *     function
   */
  private static IExpression compileAggregate(
      IRowExpressionContext context, IVariables variables, String source) {
    IExpression expression = IExpression.of(context, variables.resolve(source));
    if (!expression.isKind(Kind.CALL)
        || !(((Call) expression).getOperator() instanceof AggregateFunction)) {
      throw new ExpressionException(ErrorCode.NOT_AN_AGGREGATE_EXPRESSION);
    }
    return expression;
  }
}
