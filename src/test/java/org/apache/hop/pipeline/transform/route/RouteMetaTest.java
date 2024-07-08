/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.apache.hop.pipeline.transform.route;

import static org.junit.Assert.assertEquals;

import org.apache.hop.pipeline.transform.TransformSerializationTestUtil;
import org.apache.hop.pipeline.transforms.route.RouteMeta;
import org.junit.Test;

public class RouteMetaTest {

  @Test
  public void testSerialization() throws Exception {
    RouteMeta meta =
        TransformSerializationTestUtil.testSerialization("/route-transform.xml", RouteMeta.class);

    assertEquals(meta.getDefaultTargetTransformName(), "Condition default");
    assertEquals(meta.getRoutes().size(), 4);
  }
}
