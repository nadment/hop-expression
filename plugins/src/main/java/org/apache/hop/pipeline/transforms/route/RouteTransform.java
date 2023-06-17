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

import org.apache.commons.lang.StringUtils;
import org.apache.hop.core.Const;
import org.apache.hop.core.IRowSet;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.expression.ExpressionBuilder;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.RowExpressionContext;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransform;
import org.apache.hop.pipeline.transform.TransformMeta;
import java.util.ArrayList;
import java.util.Arrays;

/** Route input rows base on conditions. */
public class RouteTransform extends BaseTransform<RouteMeta, RouteData> {
  private static final Class<?> PKG = RouteMeta.class; // For Translator

  public RouteTransform(TransformMeta transformMeta, RouteMeta meta, RouteData data, int copyNr,
      PipelineMeta pipelineMeta, Pipeline pipeline) {
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
          throw new HopException(BaseMessages.getString(PKG,
              "Route.Exception.NoTargetTransformSpecified", route.getCondition()));
        }

        IRowSet rowSet = findOutputRowSet(route.getTransformName());
        if (rowSet == null) {
          throw new HopException(BaseMessages.getString(PKG,
              "Route.Exception.UnableToFindTargetTransform", route.getTransformName()));
        }

        // Resolve variable
        String source = resolve(route.getCondition());

        // Compile expression
        try {
          IExpression expression = ExpressionBuilder.build(data.context, source);
          data.targets.add(new RouteTarget(route, expression, rowSet));
        } catch (ExpressionException e) {
          String message = BaseMessages.getString(PKG, "Route.Exception.ConditionError",
              route.getTransformName(), route.getCondition(), e.getMessage());
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
        if (rowSet != null) {
          data.defaultRowSet = rowSet;
        }
      }
    }

    boolean toDefault = true;
    data.context.setRow(row);

    for (RouteTarget target : data.targets) {

      try {
        Boolean predicat = target.expression.getValue(data.context, Boolean.class);

        if ( predicat ) {
          toDefault = false;
          putRowTo(data.rowMeta, row, target.rowSet);
          break;
        }
      } catch (HopException e) {
        String message = BaseMessages.getString(PKG, "Route.Exception.ConditionError",
            target.route.getTransformName(), target.route.getCondition(), e.getMessage());
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

  protected static Object prepareObjectType(Object o) {
    return (o instanceof byte[]) ? Arrays.hashCode((byte[]) o) : o;
  }
}
