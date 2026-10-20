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

package org.apache.hop.pipeline.transforms.route;

import java.util.ArrayList;
import java.util.BitSet;
import org.apache.commons.lang3.StringUtils;
import org.apache.hop.core.Const;
import org.apache.hop.core.IRowSet;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.expression.CommonSubexpressionEliminator;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransform;
import org.apache.hop.pipeline.transform.TransformMeta;
import org.jspecify.annotations.Nullable;

/** Route input rows base on conditions expressions. */
public class RouteTransform extends BaseTransform<RouteMeta, RouteData> {
  private static final Class<?> PKG = RouteMeta.class; // For Translator

  /**
   * Creates the transform.
   *
   * @param transformMeta the transform in the pipeline
   * @param meta the routes and the default target
   * @param data the data of the transform
   * @param copyNr the number of this copy of the transform
   * @param pipelineMeta the pipeline
   * @param pipeline the running pipeline
   */
  public RouteTransform(
      TransformMeta transformMeta,
      RouteMeta meta,
      RouteData data,
      int copyNr,
      PipelineMeta pipelineMeta,
      Pipeline pipeline) {
    super(transformMeta, meta, data, copyNr, pipelineMeta, pipeline);
  }

  /**
   * {@inheritDoc}
   *
   * <p>Sends the row to the target of the first route whose condition is true, or to the default
   * target. Without default target, a row matching no condition is ignored.
   */
  @Override
  public boolean processRow() throws HopException {

    Object[] row = getRow(); // Get next usable row from input rowset(s)!
    if (row == null) { // no more input to be expected...

      setOutputDone();
      return false;
    }

    // Prepare transform for execution
    if (first) {
      first = false;

      // clone the input row structure and place it in our data object
      data.rowMeta = getInputRowMeta().clone();
      data.context = new RowExpressionContext(this, data.rowMeta);
      data.targets = new ArrayList<>();

      for (Route route : meta.getRoutes()) {

        if (StringUtils.isEmpty(route.getTransformName())) {
          return stopWithError(
              BaseMessages.getString(
                  PKG, "Route.Exception.NoTargetTransformSpecified", route.getCondition()),
              null);
        }

        IRowSet rowSet = findOutputRowSet(route.getTransformName());
        if (rowSet == null) {
          return stopWithError(
              BaseMessages.getString(
                  PKG, "Route.Exception.UnableToFindTargetTransform", route.getTransformName()),
              null);
        }

        // Compile expression
        try {
          IExpression expression = IExpression.of(data.context, resolve(route.getCondition()));
          data.targets.add(new RouteTarget(route, expression, rowSet));
        } catch (Exception e) {
          return stopWithError(
              BaseMessages.getString(
                  PKG,
                  "Route.Exception.ConditionError",
                  route.getTransformName(),
                  route.getCondition(),
                  e.getMessage()),
              e);
        }
      }

      // Evaluate once per row the sub-expressions common to several routes.
      // Each condition is compiled alone, so a call repeated in several conditions is not yet
      // shared. For example with the routes "UPPER(COUNTRY)='FR'" and "UPPER(COUNTRY)='DE'",
      // UPPER(COUNTRY) is evaluated by the first condition and its value reused by the second.
      // The row is not modified by this transform, so there are no modified fields to exclude.
      // The targets are created again as their expression can't be replaced.
      IExpression[] expressions = new IExpression[data.targets.size()];
      for (int i = 0; i < expressions.length; i++) {
        expressions[i] = data.targets.get(i).getExpression();
      }
      CommonSubexpressionEliminator.eliminate(data.context, expressions, new BitSet());
      for (int i = 0; i < expressions.length; i++) {
        RouteTarget target = data.targets.get(i);
        data.targets.set(i, new RouteTarget(target.getRoute(), expressions[i], target.getRowSet()));
      }

      if (StringUtils.isNotEmpty(meta.getDefaultTargetTransformName())) {
        IRowSet rowSet = findOutputRowSet(meta.getDefaultTargetTransformName());
        if (rowSet == null) {
          return stopWithError(
              BaseMessages.getString(
                  PKG,
                  "Route.Exception.UnableToFindDefaultTargetTransform",
                  meta.getDefaultTargetTransformName()),
              null);
        }
        data.defaultRowSet = rowSet;
      }
    }

    boolean toDefault = true;
    data.context.setRow(row);

    for (RouteTarget target : data.targets) {

      try {
        Boolean predicate = target.getExpression().getValue(Boolean.class);

        if (Boolean.TRUE.equals(predicate)) {
          toDefault = false;
          putRowTo(data.rowMeta, row, target.getRowSet());
          break;
        }
      } catch (Exception e) {
        return stopWithError(
            BaseMessages.getString(
                PKG,
                "Route.Exception.ConditionError",
                target.getRoute().getTransformName(),
                target.getRoute().getCondition(),
                e.getMessage()),
            e);
      }
    }

    // A row matching no condition goes to the default target, or is ignored without default target
    if (toDefault && data.defaultRowSet != null) {
      putRowTo(data.rowMeta, row, data.defaultRowSet);
    }

    return true;
  }

  /**
   * Logs an error and stops the pipeline.
   *
   * @param message the message of the error
   * @param e the cause of the error, or null
   * @return false, the transform has finished
   */
  private boolean stopWithError(String message, @Nullable Exception e) {
    logError(message);
    if (e != null && isDebug()) {
      logError(Const.getStackTracker(e));
    }
    setErrors(1);
    stopAll();
    setOutputDone();
    return false;
  }
}
