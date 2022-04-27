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
import org.apache.hop.core.CheckResult;
import org.apache.hop.core.Const;
import org.apache.hop.core.ICheckResult;
import org.apache.hop.core.annotations.Transform;
import org.apache.hop.core.exception.HopTransformException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.util.Utils;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.i18n.BaseMessages;
import org.apache.hop.metadata.api.HopMetadataProperty;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.pipeline.PipelineMeta;
import org.apache.hop.pipeline.transform.*;
import org.apache.hop.pipeline.transform.stream.IStream;
import org.apache.hop.pipeline.transform.stream.IStream.StreamType;
import org.apache.hop.pipeline.transform.stream.Stream;
import org.apache.hop.pipeline.transform.stream.StreamIcon;

import java.util.ArrayList;
import java.util.List;

@Transform(
    id = "Route",
    image = "route.svg", 
    name = "i18n::Route.Name",
    description = "i18n::Route.Description",
    categoryDescription = "i18n:org.apache.hop.pipeline.transform:BaseTransform.Category.Flow",
    keywords = "i18n::Route.Keywords", 
    documentationUrl = "/pipeline/transforms/route.html"
)
public class RouteMeta extends BaseTransformMeta<RouteTransform, RouteData> {
  private static final Class<?> PKG = RouteMeta.class; // For Translator


  private static IStream newDefaultStream = new Stream(StreamType.TARGET, null,
      BaseMessages.getString(PKG, "RouteMeta.Route.DefaultTarget.Description"), StreamIcon.TARGET, null);
  private static IStream newTargetStream = new Stream(StreamType.TARGET, null,
      BaseMessages.getString(PKG, "RouteMeta.Route.NewTarget.Description"), StreamIcon.TARGET,
      null);

  /** The targets to switch over */
  @HopMetadataProperty(groupKey = "routes", key = "route", injectionGroupDescription="RouteMeta.Injection.ROUTES")
  private List<Route> routes;

  /** The default target transform name (only used during serialization) */
  @HopMetadataProperty(key = "default_target_transform",
      injectionKey = "DEFAULT_TARGET_TRANSFORM_NAME",
      injectionKeyDescription = "RouteMeta.Injection.DEFAULT_TARGET_TRANSFORM_NAME")
  private String defaultTargetTransformName;

  public RouteMeta() {
    routes = new ArrayList<>();
  }

  public RouteMeta(RouteMeta m) {
    this();
    this.defaultTargetTransformName = m.defaultTargetTransformName;
    for (Route rule : this.routes) {
      this.routes.add(new Route(rule));
    }
  }

  @Override
  public RouteMeta clone() {
    return new RouteMeta(this);
  }

  @Override
  public void getFields(IRowMeta rowMeta, String origin, IRowMeta[] info,
      TransformMeta nextTransform, IVariables variables, IHopMetadataProvider metadataProvider)
      throws HopTransformException {
    // Default: nothing changes to rowMeta
  }

  @Override
  public void check(List<ICheckResult> remarks, PipelineMeta pipelineMeta,
      TransformMeta transformMeta, IRowMeta prev, String[] input, String[] output, IRowMeta info,
      IVariables variables, IHopMetadataProvider metadataProvider) {
    CheckResult cr;

    for (Route rule : routes) {

      if (Utils.isEmpty(rule.getCondition())) {
        cr = new CheckResult(ICheckResult.TYPE_RESULT_ERROR, BaseMessages.getString(PKG,
            "RouteMeta.CheckResult.ConditionInvalid", rule.getTransformName()), transformMeta);
        remarks.add(cr);
      }

      TransformMeta check = pipelineMeta.findTransform(rule.getTransformName());
      if (check == null) {
        cr = new CheckResult(
            ICheckResult.TYPE_RESULT_ERROR, BaseMessages.getString(PKG,
                "RouteMeta.CheckResult.TargetTransformInvalid", rule.getCondition()),
            transformMeta);
        remarks.add(cr);
      }
    }

    TransformMeta check = pipelineMeta.findTransform(this.getDefaultTargetTransformName());
    if (check == null) {
      cr = new CheckResult(ICheckResult.TYPE_RESULT_ERROR,
          BaseMessages.getString(PKG, "RouteMeta.CheckResult.DefaultTargetTransformInvalid"),
          transformMeta);
      remarks.add(cr);
    }


    // See if we have input streams leading to this transform!
    if (input.length > 0) {
      cr = new CheckResult(ICheckResult.TYPE_RESULT_OK, BaseMessages.getString(PKG,
          "RouteMeta.CheckResult.TransformReceivingInfoFromOtherTransforms"), transformMeta);
      remarks.add(cr);
    } else {
      cr = new CheckResult(ICheckResult.TYPE_RESULT_ERROR,
          BaseMessages.getString(PKG, "RouteMeta.CheckResult.NoInputReceivedFromOtherTransforms"),
          transformMeta);
      remarks.add(cr);
    }
  }

  /** @return the defaultTargetTransformName */
  public String getDefaultTargetTransformName() {
    return defaultTargetTransformName;
  }

  /** @param defaultTargetTransformName the defaultTargetTransformName to set */
  public void setDefaultTargetTransformName(String defaultTargetTransformName) {
    this.defaultTargetTransformName = defaultTargetTransformName;
  }

  /** Returns the Input/Output metadata for this transform. */
  @Override
  public ITransformIOMeta getTransformIOMeta() {
    ITransformIOMeta ioMeta = super.getTransformIOMeta(false);
    if (ioMeta == null) {

      ioMeta = new TransformIOMeta(true, false, false, false, false, true);

      // Add the routes...
      //
      for (Route route : routes) {
        IStream stream = new Stream(
            StreamType.TARGET, null, BaseMessages.getString(PKG,
                "RouteMeta.Route.SetTarget.Description", Const.NVL(route.getCondition(), "")),
            StreamIcon.TARGET, route.getTransformName());
        ioMeta.addStream(stream);
      }

      // Add the default target transform
      //
      if (StringUtils.isNotEmpty(defaultTargetTransformName)) {
        ioMeta.addStream(new Stream(StreamType.TARGET, null,
            BaseMessages.getString(PKG, "RouteMeta.Route.DefaultTarget.Description"), StreamIcon.TARGET,
            defaultTargetTransformName));
      }
      setTransformIOMeta(ioMeta);
    }

    return ioMeta;
  }

  @Override
  public void searchInfoAndTargetTransforms(List<TransformMeta> transforms) {
    List<IStream> targetStreams = getTransformIOMeta().getTargetStreams();
    int index = 0;
    for (Route target : routes) {
      IStream stream = targetStreams.get(index++);

      TransformMeta transformMeta =
          TransformMeta.findTransform(transforms, target.getTransformName());
      stream.setTransformMeta(transformMeta);
    }
    // Extra one is the default target (if any)...
    //
    if (StringUtils.isNotEmpty(defaultTargetTransformName)) {
      IStream stream = targetStreams.get(index);
      TransformMeta transformMeta =
          TransformMeta.findTransform(transforms, defaultTargetTransformName);
      stream.setTransformMeta(transformMeta);
    }
  }

  @Override
  public void convertIOMetaToTransformNames() {
    // TODO
  }

  @Override
  public List<IStream> getOptionalStreams() {
    List<IStream> list = new ArrayList<>();

    list.add(newTargetStream);
    if (StringUtils.isEmpty(defaultTargetTransformName)) {
      list.add(newDefaultStream);
    }

    return list;
  }

  @Override
  public void handleStreamSelection(IStream stream) {
    if (stream == newDefaultStream) {
      defaultTargetTransformName = stream.getTransformMeta().getName();

      IStream newStream = new Stream(StreamType.TARGET, stream.getTransformMeta(),
          BaseMessages.getString(PKG, "RouteMeta.Route.DefaultTarget.Description"), StreamIcon.TARGET,
          stream.getTransformMeta().getName());
      getTransformIOMeta().addStream(newStream);
    } else if (stream == newTargetStream) {
      // Add the target..
      //
      Route route = new Route();
      route.setTransformName(stream.getTransformMeta().getName());
      routes.add(route);
      IStream newStream = new Stream(StreamType.TARGET, stream.getTransformMeta(),
          BaseMessages.getString(PKG, "RouteMeta.Route.SetTarget.Description",
              Const.NVL(route.getCondition(), "")),
          StreamIcon.TARGET, stream.getTransformMeta().getName());
      getTransformIOMeta().addStream(newStream);
    } else {
      // A target was selected...
      //
      List<IStream> targetStreams = getTransformIOMeta().getTargetStreams();
      for (int i = 0; i < targetStreams.size(); i++) {
        if (stream == targetStreams.get(i)) {
          if (i < routes.size()) {
            routes.get(i).setTransformName(stream.getTransformMeta().getName());
          } else {
            defaultTargetTransformName = stream.getTransformMeta().getName();
          }
        }
      }
    }
  }

  /** @return the routes list */
  public List<Route> getRoutes() {
    return routes;
  }

  /** @param routes the routes list to set */
  public void setRoutes(List<Route> routes) {
    this.routes = routes;
  }

  @Override
  public boolean excludeFromCopyDistributeVerification() {
    return true;
  }
}
