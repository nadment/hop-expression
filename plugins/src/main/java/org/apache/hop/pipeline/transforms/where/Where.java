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

import org.apache.hop.core.Const;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.util.Utils;
import org.apache.hop.expression.ExpressionBuilder;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineHopMeta;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransform;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.apache.hop.pipeline.transform.stream.IStream;
import java.util.List;

public class Where extends BaseTransform<WhereMeta, WhereData> {

  private static final Class<?> PKG = WhereMeta.class;

  public Where(TransformMeta transformMeta, WhereMeta meta, WhereData data, int copyNr,
      PipelineMeta pipelineMeta, Pipeline pipeline) {
    super(transformMeta, meta, data, copyNr, pipelineMeta, pipeline);
  }

  @Override
  public boolean processRow() throws HopException {

    // get incoming row, getRow() potentially blocks waiting for more rows,
    // returns null if no more rows expected
    Object[] row = getRow();

    // if no more rows are expected, indicate Transform is finished and
    // processRow() should not be called again
    if (row == null) {
      setOutputDone();
      return false;
    }

    try {
      // the "first" flag is inherited from the base Transform implementation
      // it is used to guard some processing tasks, like figuring out field
      // indexes
      // in the row structure that only need to be done once
      if (first) {
        if (log.isDebug()) {
          logDebug(BaseMessages.getString(PKG, "Where.Log.StartedProcessing"));
        }

        first = false;

        // clone the input row structure and place it in our data object
        data.outputRowMeta = getInputRowMeta().clone();
        data.context = new ExpressionContext(this, getInputRowMeta());

        // Resolve variable
        String expression = resolve(meta.getCondition());

        // Compile expression
        data.condition = ExpressionBuilder.build(data.context, expression);

        // Cache the position of the RowSet for the output.
        List<IStream> streams = meta.getTransformIOMeta().getTargetStreams();

        // Find TRUE output row set
        if (!Utils.isEmpty(streams.get(0).getTransformName())) {
          TransformMeta to = streams.get(0).getTransformMeta();
          PipelineHopMeta hop = getPipelineMeta().findPipelineHop(getTransformMeta(), to);
          if (hop != null && hop.isEnabled()) {
            data.trueRowSet = findOutputRowSet(getTransformName(), getCopy(), to.getName(), 0);
            if (data.trueRowSet == null) {
              throw new HopException(BaseMessages.getString(PKG,
                  "Where.Exception.TargetTransformInvalid", streams.get(0).getTransformName()));
            }
          }
        } else {
          data.trueRowSet = null;
        }

        // Find FALSE output row set
        if (!Utils.isEmpty(streams.get(1).getTransformName())) {
          TransformMeta to = streams.get(1).getTransformMeta();
          PipelineHopMeta hop = getPipelineMeta().findPipelineHop(getTransformMeta(), to);
          if (hop != null && hop.isEnabled()) {
            data.falseRowSet = findOutputRowSet(getTransformName(), getCopy(), to.getName(), 0);
            if (data.falseRowSet == null) {
              throw new HopException(BaseMessages.getString(PKG,
                  "Where.Exception.TargetTransformInvalid", streams.get(1).getTransformName()));
            }
          }
        } else {
          data.falseRowSet = null;
        }
      }

      data.context.setRow(row);
      Boolean predicat = data.condition.getValue(data.context, Boolean.class);

      if (predicat) {
        // put the row to the TRUE output row stream
        if (data.trueRowSet != null) {
          if (log.isRowLevel()) {
            logRowlevel(BaseMessages.getString(PKG, "Where.Log.KeepRow",
                data.trueRowSet.getDestinationTransformName(), getInputRowMeta().getString(row)));
          }
          putRowTo(data.outputRowMeta, row, data.trueRowSet);
        }
      } else {
        // put the row to the FALSE output row stream
        if (data.falseRowSet != null) {
          if (log.isRowLevel()) {
            logRowlevel(BaseMessages.getString(PKG, "Where.Log.FilterRow",
                data.falseRowSet.getDestinationTransformName(), getInputRowMeta().getString(row)));
          }
          putRowTo(data.outputRowMeta, row, data.falseRowSet);
        }
      }
    } catch (HopException e) {
      logError(e.getMessage());
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
}
