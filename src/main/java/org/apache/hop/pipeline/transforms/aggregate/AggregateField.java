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

package org.apache.hop.pipeline.transforms.aggregate;

import lombok.Getter;
import lombok.Setter;
import org.apache.hop.metadata.api.HopMetadataProperty;

@Setter
@Getter
public final class AggregateField {
  /** The aggregate expression */
  @HopMetadataProperty(
      key = "expression",
      injectionKey = "AGGREGATE.EXPRESSION",
      injectionKeyDescription = "AggregateMeta.Injection.AGGREGATE.EXPRESSION")
  private String expression;

  /** The column name */
  @HopMetadataProperty(
      key = "name",
      injectionKey = "AGGREGATE.NAME",
      injectionKeyDescription = "AggregateMeta.Injection.AGGREGATE.NAME")
  private String name;

  public AggregateField() {}

  public AggregateField(AggregateField t) {
    this.expression = t.expression;
    this.name = t.name;
  }
}
