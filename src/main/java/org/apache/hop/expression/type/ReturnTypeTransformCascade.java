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
package org.apache.hop.expression.type;

import static java.util.Objects.requireNonNull;

import org.apache.hop.expression.Call;

/**
 * Strategy to infer the type of an operator call from the type of the operands by using one {@link
 * IReturnTypeInference} rule and a combination of {@link ITypeTransform}s.
 */
public class ReturnTypeTransformCascade implements IReturnTypeInference {

  private final IReturnTypeInference rule;
  private final ITypeTransform[] transforms;

  ReturnTypeTransformCascade(IReturnTypeInference rule, ITypeTransform... transforms) {
    this.rule = requireNonNull(rule, "rule");
    this.transforms = requireNonNull(transforms, "transforms");
  }

  @Override
  public Type inferReturnType(Call call) {
    Type ret = rule.inferReturnType(call);
    for (ITypeTransform transform : transforms) {
      ret = transform.transformType(ret);
    }
    return ret;
  }
}
