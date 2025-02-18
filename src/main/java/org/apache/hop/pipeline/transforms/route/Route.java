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

import lombok.Getter;
import lombok.Setter;
import org.apache.hop.metadata.api.HopMetadataProperty;

/** Utility class that contains the routing condition and the target transform name */
@Setter
@Getter
public class Route {
  /** The condition expression for routing to */
  @HopMetadataProperty(
      key = "condition",
      injectionKey = "ROUTE.CONDITION",
      injectionKeyDescription = "RouteMeta.Injection.ROUTE.CONDITION")
  private String condition;

  /** The target transform name */
  @HopMetadataProperty(
      key = "target_transform",
      injectionKey = "ROUTE.TARGET_TRANSFORM_NAME",
      injectionKeyDescription = "RouteMeta.Injection.ROUTE.TARGET_TRANSFORM_NAME")
  private String transformName;

  public Route() {}

  public Route(Route other) {
    this.condition = other.condition;
    this.transformName = other.transformName;
  }
}
