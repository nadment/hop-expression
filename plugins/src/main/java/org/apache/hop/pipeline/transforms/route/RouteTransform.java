/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.hop.pipeline.transforms.route;

import org.apache.commons.lang.StringUtils;
import org.apache.hop.core.IRowSet;
import org.apache.hop.core.exception.HopException;
import org.apache.hop.expression.ExpressionBuilder;
import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.IExpression;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.pipeline.Pipeline;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.BaseTransform;
import org.apache.hop.pipeline.transform.TransformMeta;
import java.util.ArrayList;
import java.util.Arrays;

/** Filters input rows base on conditions. */
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

    if (first) {
      first = false;
      prepareRoute();
    }
  
    Object result=Boolean.FALSE;
    for (RouteRow route: data.targets) {
      data.context.setRow(row);
      result = route.condition.eval(data.context);      
      if ( result==Boolean.TRUE) {
        putRowTo(data.outputRowMeta, row, route.rowSet);
        break;
      }
    }
    
    if ( result==Boolean.FALSE) {
      putRowTo(data.outputRowMeta, row, data.defaultRowSet);
    }
    
    if (checkFeedback(getLinesRead()) && log.isBasic()) {
        logBasic(BaseMessages.getString(PKG, "Route.Log.LineNumber", getLinesRead()));      
    }

    return true;
  }

  /**
   * This will prepare transform for execution:
   * @throws HopException if something goes wrong during transform preparation.
   */
  private void prepareRoute() throws HopException {

    // clone the input row structure and place it in our data object
    data.outputRowMeta = getInputRowMeta().clone();

    data.context = new ExpressionContext(this, getInputRowMeta());
    
    try {
      data.targets = new ArrayList<>();
      
      for (Route route : meta.getRoutes()) {
        // Resolve variable and parse expression
        IExpression condition = ExpressionBuilder.compile(data.context, resolve(route.getCondition()));

        if (StringUtils.isEmpty(route.getTransformName())) {
          throw new HopException(
              BaseMessages.getString(
                  PKG, "Route.Log.NoTargetTransformSpecified", route.getCondition()));
        }
 
        IRowSet rowSet = findOutputRowSet(route.getTransformName());
        if (rowSet == null) {
          throw new HopException(
              BaseMessages.getString(
                  PKG,
                  "Route.Log.UnableToFindTargetTransform",
                  route.getTransformName()));
        }
        
        data.targets.add(new RouteRow(condition, rowSet));        
      }
      
      if (StringUtils.isNotEmpty(meta.getDefaultTargetTransformName())) {
        IRowSet rowSet = findOutputRowSet(meta.getDefaultTargetTransformName());
        if (rowSet != null) {
          data.defaultRowSet=rowSet;
        }
      }
    } catch (Exception e) {
      throw new HopException(e);
    }
  }

  protected static Object prepareObjectType(Object o) {
    return (o instanceof byte[]) ? Arrays.hashCode((byte[]) o) : o;
  }
}
