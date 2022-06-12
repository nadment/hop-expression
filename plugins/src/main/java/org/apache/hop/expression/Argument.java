/*
 * Licensed to the Apache Software Foundation (ASF) under one or more contributor license
 * agreements. See the NOTICE file distributed with this work for additional information regarding
 * copyright ownership. The ASF licenses this file to You under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.apache.hop.expression;

import org.apache.hop.metadata.api.HopMetadataProperty;

public class Argument {

  @HopMetadataProperty
  private String name;
  
  @HopMetadataProperty
  private DataTypeName type;
  
  public Argument() {
    super();
  }

  public Argument(String name, DataTypeName type) {
    super();
    this.name = name;
    this.type = type;
  }
  
  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  } 
  
  public DataTypeName getType() {
    return type;
  }

  public void setType(DataTypeName type) {
    this.type = type;
  }

  @Override
  public int hashCode() {
    return name.hashCode();
  }

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if (o == null)
      return false;
    if (getClass() != o.getClass())
      return false;

    return name.equals(((Argument) o).name);
  }

  @Override
  public String toString() {
    return this.name;
  }
}
