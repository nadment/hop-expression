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
import org.apache.commons.lang.StringUtils;
import org.apache.hop.core.Const;
import org.apache.hop.core.IRowSet;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.exception.HopTransformException;
import org.apache.hop.expression.ExpressionFactory;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransform;
import org.apache.hop.pipeline.transform.TransformMeta;

/** Route input rows base on conditions expressions. */
public class RouteTransform extends BaseTransform<RouteMeta, RouteData> {
  private static final Class<?> PKG = RouteMeta.class; // For Translator

  public RouteTransform(
      TransformMeta transformMeta,
      RouteMeta meta,
      RouteData data,
      int copyNr,
      PipelineMeta pipelineMeta,
      Pipeline pipeline) {
    super(transformMeta, meta, data, copyNr, pipelineMeta, pipeline);
  }

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
          throw new HopTransformException(
              BaseMessages.getString(
                  PKG, "Route.Exception.NoTargetTransformSpecified", route.getCondition()));
        }

        IRowSet rowSet = findOutputRowSet(route.getTransformName());
        if (rowSet == null) {
          throw new HopTransformException(
              BaseMessages.getString(
                  PKG, "Route.Exception.UnableToFindTargetTransform", route.getTransformName()));
        }

        // Compile expression
        try {
          IExpression expression =
              ExpressionFactory.create(data.context, resolve(route.getCondition()));
          data.targets.add(new RouteTarget(route, expression, rowSet));
        } catch (Exception e) {
          String message =
              BaseMessages.getString(
                  PKG,
                  "Route.Exception.ConditionError",
                  route.getTransformName(),
                  route.getCondition(),
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

      if (StringUtils.isNotEmpty(meta.getDefaultTargetTransformName())) {
        IRowSet rowSet = findOutputRowSet(meta.getDefaultTargetTransformName());
        if (rowSet == null) {
          throw new HopTransformException(
              BaseMessages.getString(
                  PKG,
                  "Route.Exception.UnableToFindDefaultTargetTransform",
                  meta.getDefaultTargetTransformName()));
        }
        data.defaultRowSet = rowSet;
      }
    }

    boolean toDefault = true;
    data.context.setRow(row);

    for (RouteTarget target : data.targets) {

      try {
        boolean predicate = target.getExpression().getValue(Boolean.class);

        if (predicate) {
          toDefault = false;
          putRowTo(data.rowMeta, row, target.getRowSet());
          break;
        }
      } catch (Exception e) {
        String message =
            BaseMessages.getString(
                PKG,
                "Route.Exception.ConditionError",
                target.getRoute().getTransformName(),
                target.getRoute().getCondition(),
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

    if (toDefault) {
      putRowTo(data.rowMeta, row, data.defaultRowSet);
    }

    return true;
  }
}
