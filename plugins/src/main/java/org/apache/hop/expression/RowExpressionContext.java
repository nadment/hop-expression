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

import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.variables.IVariables;
import java.util.Objects;

public class RowExpressionContext extends ExpressionContext implements IRowExpressionContext {

  private IRowMeta rowMeta;
  private Object[] row;

  public RowExpressionContext(IVariables variables, IRowMeta rowMeta) {
    super(variables);

    this.rowMeta = Objects.requireNonNull(rowMeta);
  }

  @Override
  public IRowMeta getRowMeta() {
    return rowMeta;
  }

  @Override
  public Object[] getRow() {
    return this.row;
  }

  @Override
  public void setRow(Object[] row) {
    this.row = row;
  }
}

