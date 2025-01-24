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

import java.time.ZonedDateTime;
import java.util.Date;
import java.util.HashMap;
import org.apache.hop.core.Const;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.ValueDataUtil;
import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.ErrorCode;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionFactory;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionProcessor;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.operator.CountFunction;
import org.apache.hop.expression.type.TypeName;
import org.apache.hop.expression.type.Types;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransform;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.pipeline.transforms.aggregate.AggregateData.AggregateKey;

/** Aggregates information based on expression */
public class AggregateTransform extends BaseTransform<AggregateMeta, AggregateData> {
  private static final Class<?> PKG = AggregateMeta.class; // For Translator

  private boolean allNullsAreZero = false;

  public AggregateTransform(
      TransformMeta transformMeta,
      AggregateMeta meta,
      AggregateData data,
      int copyNr,
      PipelineMeta pipelineMeta,
      Pipeline pipeline) {
    super(transformMeta, meta, data, copyNr, pipelineMeta, pipeline);
  }

  public Date convertToDate(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof ZonedDateTime dt) {
      return Date.from(dt.toInstant());
    }
    throw new IllegalArgumentException(
        ErrorCode.UNSUPPORTED_CONVERSION.message(value, TypeName.fromValue(value), Types.DATE));
  }

  @Override
  public boolean processRow() throws HopException {

    Object[] row = getRow();

    if (first) {
      if ((row == null) && (!meta.isAlwaysGivingBackOneRow())) {
        setOutputDone();
        return false;
      }

      // What is the output looking like?
      //
      data.inputRowMeta = this.getInputRowMeta();

      // In case we have 0 input rows, we still want to send out a single row aggregate
      // However... the problem then is that we don't know the layout from receiving it from the
      // previous transform over the
      // row set.
      // So we need to calculated based on the metadata...
      //
      if (data.inputRowMeta == null) {
        data.inputRowMeta = getPipelineMeta().getPrevTransformFields(this, getTransformMeta());
      }

      data.outputRowMeta = data.inputRowMeta.clone();
      meta.getFields(data.outputRowMeta, getTransformName(), null, null, this, metadataProvider);

      // Do all the work we can beforehand
      // Calculate indexes, loop up fields, etc.
      //
      data.groupIndex = new int[meta.getGroupFields().size()];
      data.aggregates = new Call[meta.getAggregateFields().size()];
      data.functions = new AggregateFunction[meta.getAggregateFields().size()];
      data.context = new RowExpressionContext(this, data.inputRowMeta);

      // If the transform does not receive any rows, we can not look up field position indexes
      // if (row != null) {
      data.groupMeta = new RowMeta();
      for (int index = 0; index < meta.getGroupFields().size(); index++) {
        String fieldName = meta.getGroupFields().get(index).getName();
        data.groupIndex[index] = data.inputRowMeta.indexOfValue(fieldName);
        if (data.groupIndex[index] < 0) {
          logError(
              BaseMessages.getString(
                  PKG, "AggregateTransform.Exception.GroupFieldNotFound", fieldName));
          setErrors(1);
          stopAll();
          return false;
        }

        IValueMeta valueMeta = data.inputRowMeta.getValueMeta(data.groupIndex[index]);
        data.groupMeta.addValueMeta(valueMeta);
      }

      data.aggregateMeta = new RowMeta();
      for (int index = 0; index < meta.getAggregateFields().size(); index++) {
        AggregateField field = meta.getAggregateFields().get(index);

        // Resolve variable
        String source = resolve(field.getExpression());

        // Compile expression
        try {
          IValueMeta valueMeta = data.outputRowMeta.searchValueMeta(field.getName());
          data.aggregateMeta.addValueMeta(valueMeta);

          IExpression expression = ExpressionFactory.create(data.context, source);
          Call call = null;
          AggregateFunction function = null;
          if (expression.is(Kind.CALL)) {
            call = (Call) expression;
            if (call.getOperator() instanceof AggregateFunction operator) {
              function = operator;
            }
          }
          if (function == null) {
            throw new ExpressionException(ErrorCode.NOT_AN_AGGREGATE_EXPRESSION);
          }
          data.aggregates[index] = call;
          data.functions[index] = function;
        } catch (Exception e) {
          String message =
              BaseMessages.getString(
                  PKG,
                  "Aggregate.Exception.ExpressionError",
                  field.getName(),
                  field.getExpression(),
                  e.getMessage());
          logError(message);
          if (isDebug()) {
            logError(Const.getStackTracker(e));
          }
          setErrors(1);
          stopAll();
          setOutputDone();
          return false;
        }
      }
    }

    // Here is where we start to do the real work...
    //
    if (row == null) { // no more input to be expected... (or none received in the first place)
      handleLastOfGroup();

      setOutputDone();
      return false;
    }

    if (first || data.newBatch) {
      first = false;
      data.newBatch = false;
    }

    processAggregate(row);

    if (checkFeedback(getLinesRead())) {
      if (isBasic()) {
        logBasic(BaseMessages.getString(PKG, "Aggregate.LineNumber") + getLinesRead());
      }
    }

    return true;
  }

  private void handleLastOfGroup() throws HopException {
    // Dump the content of the map...
    //
    for (AggregateKey key : data.map.keySet()) {

      Object[] outputRow = new Object[data.outputRowMeta.size()];

      int index = 0;
      for (int i = 0; i < data.groupMeta.size(); i++) {
        outputRow[index++] =
            data.groupMeta.getValueMeta(i).convertToNormalStorageType(key.getValues()[i]);
      }

      IExpressionProcessor[] aggregators = data.map.get(key);
      Object[] results = getAggregateResult(aggregators);
      for (int i = 0; i < data.aggregateMeta.size(); i++) {
        Object value = results[i];

        // Value meta doesn't support ZonedDateTime
        int type = data.aggregateMeta.getValueMeta(i).getType();
        if (type == IValueMeta.TYPE_DATE) {
          value = convertToDate(value);
        } else if (type == IValueMeta.TYPE_TIMESTAMP) {
          value = convertToDate(value);
        }

        outputRow[index++] = data.aggregateMeta.getValueMeta(i).convertToNormalStorageType(value);
      }

      putRow(data.outputRowMeta, outputRow);
    }

    // If we need to give back one row ?
    // This means we give back 0 for COUNT function, null for everything else
    //
    if (data.map.isEmpty() && meta.isAlwaysGivingBackOneRow()) {
      Object[] outputRow = new Object[data.outputRowMeta.size()];
      int index = 0;
      for (int i = 0; i < data.groupMeta.size(); i++) {
        outputRow[index++] = null;
      }
      for (int i = 0; i < data.aggregateMeta.size(); i++) {
        Object value = null;

        if (data.aggregates[i].getOperator().is(CountFunction.COUNT_VALUE)) {
          value = 0L;
        }
        outputRow[index++] = value;
      }

      putRow(data.outputRowMeta, outputRow);
    }
  }

  /**
   * Process each row
   *
   * @param row
   * @throws HopException
   */
  protected void processAggregate(Object[] row) throws HopException {

    AggregateKey key = data.createAggregateKey(row);

    IExpressionProcessor[] aggregators = data.map.get(key);
    if (aggregators == null) {

      // Create a new processors...
      aggregators = createAggregate();

      // Store it in the map!
      data.map.put(key, aggregators);
    }

    try {
      data.context.setRow(row);
      for (int i = 0; i < aggregators.length; i++) {
        aggregators[i].process(data.aggregates[i].getOperands());
      }
    } catch (Exception e) {
      throw new ExpressionException(ErrorCode.CALL_FUNCTION_ERROR, e.getMessage());
    }
  }

  /**
   * Create new aggregate
   *
   * @throws HopException
   */
  protected IExpressionProcessor[] createAggregate() throws HopException {

    IExpressionProcessor[] processors = new IExpressionProcessor[data.functions.length];

    for (int i = 0; i < data.functions.length; i++) {
      processors[i] =
          data.functions[i].createProcessor(data.context, data.aggregates[i].getOperands());
    }

    return processors;
  }

  protected Object[] getAggregateResult(IExpressionProcessor[] aggregators) throws HopException {
    Object[] result = new Object[aggregators.length];
    try {
      for (int i = 0; i < aggregators.length; i++) {
        Object value = aggregators[i].getValue();

        if (value == null && allNullsAreZero) {
          // seems all rows for min function was nulls...
          IValueMeta valueMeta = data.aggregateMeta.getValueMeta(i);
          value = ValueDataUtil.getZeroForValueMetaType(valueMeta);
        }
        result[i] = value;
      }
    } catch (Exception e) {
      throw new ExpressionException(ErrorCode.CALL_FUNCTION_ERROR, e.getMessage());
    }
    return result;
  }

  @Override
  public boolean init() {

    if (super.init()) {

      allNullsAreZero = this.getVariableBoolean(Const.HOP_AGGREGATION_ALL_NULLS_ARE_ZERO, false);
      // minNullIsValued = this.getVariableBoolean(Const.HOP_AGGREGATION_MIN_NULL_IS_VALUED, false);
      data.map = new HashMap<>(5000);
      return true;
    }
    return false;
  }

  @Override
  public void dispose() {
    super.dispose();
    data.clear();
  }

  @Override
  public void batchComplete() throws HopException {
    // Empty the hash table
    //
    handleLastOfGroup();

    // Clear the complete cache...
    //
    data.map.clear();

    data.newBatch = true;
  }
}
