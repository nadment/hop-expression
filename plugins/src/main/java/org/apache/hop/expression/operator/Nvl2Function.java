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
package org.apache.hop.expression.operator;

import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;

/**
 * Returns the second argument when the first argument is not NULL. If the first argument is NULL,
 * the third argument is returned.
 */
@FunctionPlugin
public class Nvl2Function extends Function {

  public Nvl2Function() {
    super("NVL2", ReturnTypes.ARG1_OR_ARG2, OperandTypes.ANY_SAME_SAME,
        OperatorCategory.CONDITIONAL, "/docs/nvl2.html");
  }

  @Override
  public Object eval(IExpression[] operands)
      throws Exception {
    Object condition = operands[0].getValue();

    if (condition == null) {
      return operands[2].getValue();
    }

    return operands[1].getValue();
  }
}
