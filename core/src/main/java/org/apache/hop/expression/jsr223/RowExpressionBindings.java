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
 */
package org.apache.hop.expression.jsr223;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.script.Bindings;

public class RowExpressionBindings implements Bindings {

  /** The <code>Map</code> field stores the attributes. */
  private Map<String, Object> map;

  /** Default constructor uses a <code>HashMap</code>. */
  public RowExpressionBindings() {
    this(new HashMap<String, Object>());
  }

  /**
   * Constructor uses an existing <code>Map</code> to store the values.
   *
   * @param m The <code>Map</code> backing this <code>SimpleBindings</code>.
   * @throws NullPointerException if m is null
   */
  public RowExpressionBindings(Map<String, Object> m) {
    if (m == null) {
      throw new NullPointerException();
    }
    this.map = m;
  }

  @Override
  public int size() {
    return map.size();
  }

  @Override
  public boolean isEmpty() {
    return map.isEmpty();
  }

  @Override
  public boolean containsValue(Object value) {
    return map.containsValue(value);
  }

  @Override
  public void clear() {
    map.clear();
  }

  @Override
  public Set<String> keySet() {
    return map.keySet();
  }

  @Override
  public Collection<Object> values() {
    return map.values();
  }

  @Override
  public Set<Entry<String, Object>> entrySet() {
    return map.entrySet();
  }

  @Override
  public Object put(String name, Object value) {
    return map.put(name, value);
  }

  @Override
  public void putAll(Map<? extends String, ? extends Object> toMerge) {
    if (toMerge == null) {
      throw new NullPointerException("toMerge map is null");
    }
    for (Map.Entry<? extends String, ? extends Object> entry : toMerge.entrySet()) {
      String key = entry.getKey();
      put(key, entry.getValue());
    }
  }

  public boolean containsKey(Object key) {
    return map.containsKey(key);
  }

  @Override
  public Object get(Object key) {
    return map.get(key);
  }

  @Override
  public Object remove(Object key) {
    return map.remove(key);
  }
}
