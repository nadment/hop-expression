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

import org.apache.hop.expression.Call;

/**
 * Calculate return type precision and scale for x * y
 */
public class MultiplyOperatorReturnTypeInference implements IReturnTypeInference {

  public MultiplyOperatorReturnTypeInference() {
    super();
  }

  @Override
  public DataType getReturnType(Call call) {
    DataType x = call.getOperand(0).getType();
    DataType y = call.getOperand(1).getType();

    // Return type precision
    int xp = x.getPrecision();
    int yp = y.getPrecision();
    int p = Math.min(DataName.NUMBER.getMaxPrecision(), xp + yp);

    // Return type scale
    int xs = x.getScale();
    int ys = y.getScale();
    int s = xs + ys;

    return new NumberDataType(p, s);
  }
}
