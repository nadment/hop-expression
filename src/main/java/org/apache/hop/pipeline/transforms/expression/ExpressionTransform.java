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

import com.fasterxml.jackson.databind.JsonNode;
import java.math.BigDecimal;
import java.net.InetAddress;
import java.sql.Timestamp;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Date;
import org.apache.hop.core.Const;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionFactory;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransform;
import org.apache.hop.pipeline.transform.TransformMeta;

public class ExpressionTransform extends BaseTransform<ExpressionMeta, ExpressionData> {
  private static final Class<?> PKG = ExpressionMeta.class;

  public ExpressionTransform(
      TransformMeta transformMeta,
      ExpressionMeta meta,
      ExpressionData data,
      int copyNr,
      PipelineMeta pipelineMeta,
      Pipeline pipeline) {
    super(transformMeta, meta, data, copyNr, pipelineMeta, pipeline);
  }

  @Override
  public boolean processRow() throws HopException {

    // get incoming row, getRow() potentially blocks waiting for more rows,
    // returns null if no more rows expected
    Object[] row = getRow();

    // if no more rows are expected, indicate transform is finished and
    // processRow() should not be called again
    if (row == null) {
      setOutputDone();
      return false;
    }

    // the "first" flag is inherited from the base step implementation
    // it is used to guard some processing tasks, like figuring out field
    // indexes in the row structure that only need to be done once
    if (first) {
      first = false;

      if (isDebug()) {
        logDebug(BaseMessages.getString(PKG, "ExpressionTransform.Log.StartedProcessing"));
      }

      // Clone the input row structure and place it in our data object
      data.outputRowMeta = getInputRowMeta().clone();

      // Use meta.getFields() to change it, so it reflects the output row structure
      meta.getFields(
          data.outputRowMeta, this.getTransformName(), null, null, this, metadataProvider);
      data.expressions = new IExpression[data.outputRowMeta.size()];

      data.context = new RowExpressionContext(this, data.outputRowMeta);

      // For all fields expression
      for (ExpressionField field : meta.getFields()) {
        int index = data.outputRowMeta.indexOfValue(field.getName());

        String source = field.getExpression();

        // Ignore expression if value meta type is NONE
        IValueMeta valueMeta = data.outputRowMeta.getValueMeta(index);
        if (valueMeta.getType() == IValueMeta.TYPE_NONE) {
          source = "";
        }

        // Compile expression
        try {
          data.expressions[index] = ExpressionFactory.create(data.context, resolve(source));
        } catch (Exception e) {
          String message =
              BaseMessages.getString(
                  PKG,
                  "ExpressionTransform.Exception.CompileExpression",
                  field.getName(),
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

    String name = null;

    try {
      // Copies row into outputRowValues and pads extra null-default slots for the
      // output values
      Object[] outputRowValues = Arrays.copyOf(row, data.outputRowMeta.size());

      // Use output row as context, so second expression can use result from the first
      data.context.setRow(outputRowValues);

      for (ExpressionField field : meta.getFields()) {
        name = field.getName();
        int index = data.outputRowMeta.indexOfValue(name);

        IExpression expression = data.expressions[index];
        IValueMeta valueMeta = data.outputRowMeta.getValueMeta(index);
        outputRowValues[index] = processValue(expression, valueMeta);
      }

      // Put the row to the output row stream
      putRow(data.outputRowMeta, outputRowValues);

    } catch (Exception e) {
      String message =
          BaseMessages.getString(
              PKG, "ExpressionTransform.Exception.EvaluateExpression", name, e.getMessage());
      logError(message, e);
      if (isDebug()) {
        logError(Const.getStackTracker(e));
      }
      setErrors(1);
      stopAll();
      setOutputDone();
      return false;
    }

    // indicate that processRow() should be called again
    return true;
  }

  public Object processValue(IExpression expression, IValueMeta meta)
      throws HopException, ExpressionException {
    switch (meta.getType()) {
      case IValueMeta.TYPE_NONE:
        return null;
      case IValueMeta.TYPE_STRING:
        return expression.getValue(String.class);
      case IValueMeta.TYPE_NUMBER:
        BigDecimal number = expression.getValue(BigDecimal.class);
        if (number == null) {
          return null;
        }
        return number.doubleValue();
      case IValueMeta.TYPE_INTEGER:
        return expression.getValue(Long.class);
      case IValueMeta.TYPE_DATE:
        {
          ZonedDateTime date = expression.getValue(ZonedDateTime.class);
          if (date == null) {
            return null;
          }
          return Date.from(date.toInstant());
        }
      case IValueMeta.TYPE_TIMESTAMP:
        {
          ZonedDateTime date = expression.getValue(ZonedDateTime.class);
          if (date == null) {
            return null;
          }
          return Timestamp.from(date.toInstant());
        }
      case IValueMeta.TYPE_BIGNUMBER:
        return expression.getValue(BigDecimal.class);
      case IValueMeta.TYPE_BOOLEAN:
        return expression.getValue(Boolean.class);
      case IValueMeta.TYPE_BINARY:
        return expression.getValue(byte[].class);
      case org.apache.hop.core.row.value.ValueMetaJson.TYPE_JSON:
        return expression.getValue(JsonNode.class);
      case IValueMeta.TYPE_INET:
        return expression.getValue(InetAddress.class);
      default:
        throw new HopValueException(
            BaseMessages.getString(
                PKG,
                "ExpressionTransform.Exception.ConversionError",
                meta.getName(),
                meta.getType()));
    }
  }
}
