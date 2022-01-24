/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.pipeline.transforms.expression;

import org.apache.hop.core.Const;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.exception.HopTransformException;
import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.optimizer.Optimizer;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransform;
import org.apache.hop.pipeline.transform.TransformMeta;
import java.util.Arrays;
import java.util.Date;

public class ExpressionTransform extends BaseTransform<ExpressionTransformMeta, ExpressionTransformData> {
  private static final Class<?> PKG = ExpressionTransformMeta.class;

  public ExpressionTransform(TransformMeta transformMeta, ExpressionTransformMeta meta, ExpressionTransformData data,
      int copyNr, PipelineMeta pipelineMeta, Pipeline pipeline) {
    super(transformMeta, meta, data, copyNr, pipelineMeta, pipeline);
  }

  @Override
  public boolean init() {

    if (log.isDebug()) {
      logDebug(BaseMessages.getString(PKG, "ExpressionTransform.Log.Init"));
    }

    return super.init();
  }

  @Override
  public void initBeforeStart() throws HopTransformException {

    super.initBeforeStart();

    if (log.isDebug()) {
      logDebug(BaseMessages.getString(PKG, "ExpressionTransform.Log.InitBeforeStartProcessing"));
    }
  }

  @Override
  public boolean processRow() throws HopException {

    // get incoming row, getRow() potentially blocks waiting for more rows,
    // returns null if no more rows expected
    Object[] row = getRow();

    // if no more rows are expected, indicate step is finished and
    // processRow() should not be called again
    if (row == null) {
      setOutputDone();
      return false;
    }

    // the "first" flag is inherited from the base step implementation
    // it is used to guard some processing tasks, like figuring out field
    // indexes
    // in the row structure that only need to be done once
    if (first) {
      first = false;

      if (log.isDebug()) {
        logDebug(BaseMessages.getString(PKG, "ExpressionTransform.Log.StartedProcessing"));
      }


      // Clone the input row structure and place it in our data object
      data.outputRowMeta = getInputRowMeta().clone();

      // Use meta.getFields() to change it, so it reflects the output row structure
      meta.getFields(data.outputRowMeta, this.getTransformName(), null, null, this,
          metadataProvider);
      data.expressions = new IExpression[data.outputRowMeta.size()];

      ExpressionContext context = new ExpressionContext(this, data.outputRowMeta);
      data.expressionContext = context;

      Optimizer optimizer = new Optimizer();
      
      // For all fields expression
      for (ExpressionField field : meta.getFields()) {
        int index = data.outputRowMeta.indexOfValue(field.getName());

        // Resolve variable
        String source = resolve(field.getExpression());

        if (log.isDetailed()) {
          logDetailed("field [" + field.getName() + "] has expression [" + source + "]");
        }

        // Ignore expression if value meta type is NONE
        IValueMeta valueMeta = data.outputRowMeta.getValueMeta(index);
        if (valueMeta.getType() == IValueMeta.TYPE_NONE) {
          source = "NULL";
        }

        // Parse and optimize expression
        try {
          IExpression expression = ExpressionParser.parse(source);
          data.expressions[index] = optimizer.optimize(context, expression);
        } catch (Exception ex) {
          String message = BaseMessages.getString(PKG, "ExpressionTransform.Exception.ParseExpressionError",
              field.getName(), field.getExpression(), ex.toString());

          logError( BaseMessages.getString( PKG, "ExpressionTransform.Log.UnexpectedeError" ) + " : " + ex.toString() );
          logError( BaseMessages.getString( PKG, "ExpressionTransform.Log.ErrorStackTrace" )
            + Const.CR + Const.getSimpleStackTrace( ex ) + Const.CR + Const.getStackTracker( ex ) );
          
          throw new HopException(message, ex);
        }
      }
    }

    // Copies row into outputRowValues and pads extra null-default slots for the
    // output values
    Object[] outputRowValues = Arrays.copyOf(row, data.outputRowMeta.size());

    // Evaluate expression
    ExpressionContext context = data.expressionContext;

    // Use output row as context, so second expression can use result from the first
    context.setRow(outputRowValues);

    for (ExpressionField field : meta.getFields()) {

      int index = data.outputRowMeta.indexOfValue(field.getName());

      try {
        IExpression expression = data.expressions[index];
        Object value = expression.eval(context);

        if (log.isDetailed()) {
          logDetailed("field [" + field.getName() + "] has expression [" + value + "]");
        }

        IValueMeta valueMeta = data.outputRowMeta.getValueMeta(index);
        outputRowValues[index] = convertValue(valueMeta, value);
      } catch (Exception ex) {
        String message = BaseMessages.getString(PKG, "ExpressionTransform.Exception.EvaluateExpressionError",
            field.getName(), field.getExpression(), ex.toString());
        logError(message);       
        logError(Const.getStackTracker(ex));
        setErrors(1);
        stopAll();
        setOutputDone(); // signal end to receiver(s)
        return false;                
      }
    }

    // Put the row to the output row stream
    putRow(data.outputRowMeta, outputRowValues);

    // Log progress if it is time to to so
    if (checkFeedback(getLinesRead())) {
      if (log.isBasic()) {
        logBasic(BaseMessages.getString(PKG, "ExpressionTransform.Log.LineNumber", getLinesRead()));
      }
    }

    // indicate that processRow() should be called again
    return true;
  }
  
  /**
   * We can't use valueMeta.convertData(valueMeta,value) because doesn't support ZonedDateTime
   * 
   * @param meta
   * @param value
   * @return
   * @throws HopException
   */
  public Object convertValue(IValueMeta meta, Object value) throws HopException {

    if (value == null)
      return null;

    switch (meta.getType()) {
      case IValueMeta.TYPE_NONE:
        return null;
      case IValueMeta.TYPE_STRING:
        return DataType.toString(value);
      case IValueMeta.TYPE_NUMBER:
        return DataType.toNumber(value);
      case IValueMeta.TYPE_INTEGER:
        return DataType.toInteger(value);
      case IValueMeta.TYPE_DATE:
        return Date.from(DataType.toDate(value).toInstant());
      case IValueMeta.TYPE_TIMESTAMP:
        return java.sql.Timestamp.from(DataType.toDate(value).toInstant());
      case IValueMeta.TYPE_BIGNUMBER:
        return DataType.toBigNumber(value);
      case IValueMeta.TYPE_BOOLEAN:
        return DataType.toBoolean(value);
      case IValueMeta.TYPE_BINARY:
        return DataType.toBinary(value);
      default:
        throw new HopValueException(
            value + " : I can't convert the specified value to data type : " + meta.getType());
    }
  }
}
