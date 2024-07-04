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
package org.apache.hop.pipeline.transforms.expression;

import java.util.Objects;
import org.apache.commons.lang.StringUtils;
import org.apache.hop.metadata.api.HopMetadataProperty;

/** Contains the properties of the fields to set with expression. */
public class ExpressionField {

  public ExpressionField() {
    super();
  }

  public ExpressionField(ExpressionField other) {
    super();

    this.expression = other.expression;
    this.name = other.name;
    this.type = other.type;
    this.length = other.length;
    this.precision = other.precision;
  }

  /** The target field name */
  @HopMetadataProperty(
      key = "name",
      injectionKeyDescription = "ExpressionMeta.Injection.Field.Name")
  private String name;

  @HopMetadataProperty(
      key = "expression",
      injectionKeyDescription = "ExpressionMeta.Injection.Field.Expression")
  private String expression;

  @HopMetadataProperty(injectionKeyDescription = "ExpressionMeta.Injection.Field.Type")
  private String type;

  @HopMetadataProperty(injectionKeyDescription = "ExpressionMeta.Injection.Field.Length")
  private int length = -1;

  @HopMetadataProperty(injectionKeyDescription = "ExpressionMeta.Injection.Field.Precision")
  private int precision = -1;

  public String getName() {
    return name;
  }

  public void setName(final String name) {
    this.name = StringUtils.stripToNull(name);
  }

  public String getExpression() {
    return expression;
  }

  public void setExpression(final String expression) {
    this.expression = expression;
  }

  public String getType() {
    return type;
  }

  public void setType(String type) {
    this.type = StringUtils.stripToNull(type);
  }

  public int getLength() {
    return length;
  }

  public void setLength(int length) {
    this.length = length;
  }

  public int getPrecision() {
    return precision;
  }

  public void setPrecision(int precision) {
    this.precision = precision;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ExpressionField that = (ExpressionField) o;
    return Objects.equals(name, that.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }

  @Override
  public String toString() {
    return name + ":" + type + "(" + length + "," + precision + ")";
  }
}
