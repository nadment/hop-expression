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
package org.apache.hop.expression;

import org.apache.hop.metadata.api.HopMetadata;
import org.apache.hop.metadata.api.HopMetadataBase;
import org.apache.hop.metadata.api.HopMetadataProperty;
import org.apache.hop.metadata.api.IHopMetadata;
import java.util.ArrayList;
import java.util.List;

/**
 * A user-defined function (UDF) is a function provided by the user 
 */
@HopMetadata(
    key = "udf",
    name = "User Defined Function",
    description = "User defined function for expression",
    image = "function.svg")
public class UdfMeta extends HopMetadataBase implements IHopMetadata {

  @HopMetadataProperty
  private String description;
  
  @HopMetadataProperty
  private String source;
  
  @HopMetadataProperty(key = "arguments")
  private List<Argument> arguments;

  public UdfMeta() {
    super();
    arguments = new ArrayList<>();
  }
  
  /**
   * Gets Udf description
   *
   * @return value of description
   */
  public String getDescription() {
    return description;
  }

  /**
   * Set the Udf description
   *  
   * @param description The description to set
   */
  public void setDescription(String description) {
    this.description = description;
  }
  
  public List<Argument> getArguments() {
    return arguments;
  }

  public void setArguments(List<Argument> arguments) {
    this.arguments = arguments;
  }
  
  public String getSource() {
    return source;
  }

  public void setSource(String source) {
    this.source = source;
  }

 
}
