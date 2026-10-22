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

import java.time.ZoneId;
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
import org.apache.hop.expression.IAggregateProcessor;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.Kind;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.expression.operator.CountFunction;
import org.apache.hop.expression.type.DateType;
import org.apache.hop.expression.type.TypeName;
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

  /**
   * Converts the value of a date aggregate to the value of a date field.
   *
   * @param value the value of the aggregate, a ZonedDateTime or null
   * @return the date at the same local date-time in the system time zone, or null
   * @throws IllegalArgumentException if the value is not a ZonedDateTime
   */
  private static Date convertToDate(final Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof ZonedDateTime dt) {
      // The same local date-time in the system time zone, as a date field is read
      return Date.from(dt.toLocalDateTime().atZone(ZoneId.systemDefault()).toInstant());
    }
    throw new IllegalArgumentException(
        ErrorCode.UNSUPPORTED_CONVERSION.message(value, TypeName.fromValue(value), DateType.DATE));
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
              BaseMessages.getString(PKG, "Aggregate.Exception.GroupFieldNotFound", fieldName));
          setErrors(1);
          stopAll();
          setOutputDone();
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
          // The aggregates follow the group fields, an aggregate with the name of another field is
          // renamed in the output row, so the value meta is found by its position
          IValueMeta valueMeta = data.outputRowMeta.getValueMeta(data.groupMeta.size() + index);
          data.aggregateMeta.addValueMeta(valueMeta);

          IExpression expression = IExpression.of(data.context, source);
          Call call = null;
          AggregateFunction function = null;
          if (expression.isKind(Kind.CALL)) {
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
    try {
      if (row == null) { // no more input to be expected... (or none received in the first place)
        handleLastOfGroup();

        // The first row is still expected when the transform receives no row at all
        if (first && meta.isAlwaysGivingBackOneRow()) {
          putEmptyResult();
        }

        setOutputDone();
        return false;
      }

      first = false;

      processAggregate(row);
    } catch (HopException | RuntimeException e) {
      // An error while calculating an aggregate stops the transform, like an invalid expression
      // The message of a Hop exception is logged without the messages of its causes
      logError((e instanceof HopException he) ? he.getSuperMessage() : e.getMessage());
      if (isDebug()) {
        logError(Const.getStackTracker(e));
      }
      setErrors(1);
      stopAll();
      setOutputDone();
      return false;
    }

    if (checkFeedback(getLinesRead()) && isBasic()) {
      logBasic(BaseMessages.getString(PKG, "Aggregate.LineNumber") + getLinesRead());
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

      IAggregateProcessor[] aggregators = data.map.get(key);
      Object[] results = getAggregateResult(aggregators);
      for (int i = 0; i < data.aggregateMeta.size(); i++) {
        Object value = results[i];

        // Value meta doesn't support ZonedDateTime
        // The aggregates are never of type Timestamp, the expressions only have the type DATE
        if (data.aggregateMeta.getValueMeta(i).getType() == IValueMeta.TYPE_DATE) {
          try {
            value = convertToDate(value);
          } catch (IllegalArgumentException e) {
            throw aggregateError(i, e);
          }
        }

        outputRow[index++] = data.aggregateMeta.getValueMeta(i).convertToNormalStorageType(value);
      }

      putRow(data.outputRowMeta, outputRow);
    }
  }

  /**
   * Gives back one row when the transform receives no row: 0 for the COUNT function, null for
   * everything else. A batch without row gives back no row.
   */
  private void putEmptyResult() throws HopException {
    Object[] outputRow = new Object[data.outputRowMeta.size()];
    int index = 0;
    for (int i = 0; i < data.groupMeta.size(); i++) {
      outputRow[index++] = null;
    }
    for (int i = 0; i < data.aggregateMeta.size(); i++) {
      Object value = null;

      if (data.aggregates[i].getOperator().is(CountFunction.COUNT_ALL)) {
        value = 0L;
      }
      outputRow[index++] = value;
    }

    putRow(data.outputRowMeta, outputRow);
  }

  /**
   * Returns the error of an aggregate, with the name of its field and its expression to identify
   * it. The message of an expression error, like a conversion error of an argument, is kept as is,
   * the other errors are reported with the name of the function.
   *
   * @param index the index of the aggregate
   * @param e the error of the aggregate function
   * @return the error to log, its cause is the error of the aggregate function
   */
  private HopException aggregateError(int index, Exception e) {
    String message =
        (e instanceof ExpressionException)
            ? e.getMessage()
            : ErrorCode.CALL_FUNCTION_ERROR.message(
                data.functions[index].getName(), e.getMessage());
    AggregateField field = meta.getAggregateFields().get(index);
    return new HopException(
        BaseMessages.getString(
            PKG,
            "Aggregate.Exception.ExpressionError",
            field.getName(),
            field.getExpression(),
            message),
        e);
  }

  /**
   * Process each row
   *
   * @param row the row to process
   * @throws HopException
   */
  protected void processAggregate(Object[] row) throws HopException {
    data.context.setRow(row);

    AggregateKey key = data.createAggregateKey(row);

    IAggregateProcessor[] aggregators = data.map.get(key);
    if (aggregators == null) {

      // Create a new processors...
      aggregators = createAggregate();

      // Store it in the map!
      data.map.put(key, aggregators);
    }

    for (int i = 0; i < aggregators.length; i++) {
      try {
        aggregators[i].process(data.aggregates[i].getOperands());
      } catch (Exception e) {
        throw aggregateError(i, e);
      }
    }
  }

  /** Create new aggregate */
  protected IAggregateProcessor[] createAggregate() throws HopException {

    IAggregateProcessor[] processors = new IAggregateProcessor[data.functions.length];

    for (int i = 0; i < data.functions.length; i++) {
      try {
        processors[i] =
            data.functions[i].createProcessor(data.context, data.aggregates[i].getOperands());
      } catch (Exception e) {
        throw aggregateError(i, e);
      }
    }

    return processors;
  }

  protected Object[] getAggregateResult(IAggregateProcessor[] aggregators) throws HopException {
    Object[] result = new Object[aggregators.length];
    for (int i = 0; i < aggregators.length; i++) {
      try {
        Object value = aggregators[i].getValue();

        if (value == null && allNullsAreZero) {
          // seems all rows for min function was nulls...
          // Only the numbers and the strings have a zero value, the other types stay NULL
          IValueMeta valueMeta = data.aggregateMeta.getValueMeta(i);
          if (valueMeta.isNumeric() || valueMeta.isString()) {
            value = ValueDataUtil.getZeroForValueMetaType(valueMeta);
          }
        }
        result[i] = value;
      } catch (Exception e) {
        throw aggregateError(i, e);
      }
    }
    return result;
  }

  @Override
  public boolean init() {

    if (super.init()) {

      allNullsAreZero = this.getVariableBoolean(Const.HOP_AGGREGATION_ALL_NULLS_ARE_ZERO, false);
      data.map = HashMap.newHashMap(5000);
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
  }
}
