/*
 * ! ******************************************************************************
 *
 * Hop : The Hop Orchestration Platform
 *
 * http://www.project-hop.org
 *
 *******************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 *
 ******************************************************************************/

package org.apache.hop.pipeline.transforms.clonerowexpression;

import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.row.RowDataUtil;
import org.apache.hop.core.util.Utils;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionFactory;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransform;
import org.apache.hop.pipeline.transform.TransformMeta;

/** Clone input row. */
public class CloneRow extends BaseTransform<CloneRowMeta, CloneRowData> {

  private static final Class<?> PKG = CloneRowMeta.class; // for i18n purposes, needed by

  // Translator!!

  public CloneRow(
      TransformMeta transformMeta,
      CloneRowMeta meta,
      CloneRowData data,
      int copyNr,
      PipelineMeta pipelineMeta,
      Pipeline pipeline) {
    super(transformMeta, meta, data, copyNr, pipelineMeta, pipeline);
  }

  public Object evaluate(String source) throws HopException {
    String value = variables.resolve(source);

    if (value.charAt(0) != '=') {
      return value;
    }
    ExpressionContext context = new ExpressionContext(this);
    IExpression expression;
    try {
      expression = ExpressionFactory.create(context, value.substring(1));
    } catch (ExpressionException e) {
      throw new HopException(
          BaseMessages.getString(PKG, "Unable to compile expression ''{0}''", source), e);
    }

    return expression.getValue();
  }

  @Override
  public boolean processRow() throws HopException {

    Object[] r = getRow(); // get row, set busy!

    // if no more input to be expected set done.
    if (r == null) {
      setOutputDone();
      return false;
    }

    if (first) {
      first = false;
      data.outputRowMeta = getInputRowMeta().clone();
      data.nrPrevFields = getInputRowMeta().size();
      meta.getFields(data.outputRowMeta, getTransformName(), null, null, this, metadataProvider);
      data.addInfosToRow = (meta.isAddCloneFlag() || meta.isAddCloneNum());

      if (meta.isAddCloneFlag()) {
        String realflagfield = evaluate(meta.getCloneFlagField()).toString();
        if (Utils.isEmpty(realflagfield)) {
          logError(BaseMessages.getString(PKG, "CloneRow.Error.CloneFlagFieldMissing"));
          throw new HopException(
              BaseMessages.getString(PKG, "CloneRow.Error.CloneFlagFieldMissing"));
        }
      }
      if (meta.isAddCloneNum()) {
        String realnumfield = evaluate(meta.getCloneNumField()).toString();
        if (Utils.isEmpty(realnumfield)) {
          logError(BaseMessages.getString(PKG, "CloneRow.Error.CloneNumFieldMissing"));
          throw new HopException(
              BaseMessages.getString(PKG, "CloneRow.Error.CloneNumFieldMissing"));
        }
      }

      data.context = new RowExpressionContext(this, getInputRowMeta());

      String nrclonesString = resolve(meta.getNrClones());
      try {
        data.numberOfClones = ExpressionFactory.create(data.context, nrclonesString);
      } catch (ExpressionException e) {
        throw new HopException(
            BaseMessages.getString(PKG, "Unable to compile expression ''{0}''", meta.getNrClones()),
            e);
      }
      if (isDebug()) {
        logDebug(BaseMessages.getString(PKG, "CloneRow.Log.NrClones", "" + data.numberOfClones));
      }
    }

    Object[] outputRowData = r;

    if (data.addInfosToRow) {
      // It's the original row.
      // We need here to add some infos in order to identify this row
      outputRowData = RowDataUtil.createResizedCopy(r, data.outputRowMeta.size());
      int rowIndex = data.nrPrevFields;
      if (meta.isAddCloneFlag()) {
        // This row is not a clone but the original row
        outputRowData[rowIndex] = false;
        rowIndex++;
      }
      if (meta.isAddCloneNum()) {
        // This row is the original so let's identify it as the first one (zero)
        outputRowData[rowIndex] = 0L;
      }
    }

    putRow(data.outputRowMeta, outputRowData); // copy row to output rowset(s);

    data.context.setRow(r);
    long nrClones = data.numberOfClones.getValue(Long.class);

    for (int i = 0; i < nrClones && !isStopped(); i++) {
      // Output now all clones row
      outputRowData = r.clone();
      if (data.addInfosToRow) {
        // We need here to add more infos about clone rows
        outputRowData = RowDataUtil.createResizedCopy(r, data.outputRowMeta.size());
        int rowIndex = data.nrPrevFields;
        if (meta.isAddCloneFlag()) {
          // This row is a clone row
          outputRowData[rowIndex] = true;
          rowIndex++;
        }
        if (meta.isAddCloneNum()) {
          // Let's add to clone number
          // Clone starts at number 1 (0 is for the original row)
          long clonenum = i + 1L;
          outputRowData[rowIndex] = clonenum;
        }
      }
      putRow(data.outputRowMeta, outputRowData); // copy row to output rowset(s);
    }

    if (isDetailed() && checkFeedback(getLinesRead())) {
      logDetailed(BaseMessages.getString(PKG, "CloneRow.Log.LineNumber", "" + getLinesRead()));
    }

    return true;
  }
}
