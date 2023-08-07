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

import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.exception.ConversionException;
import org.apache.hop.expression.type.Type;
import java.time.DateTimeException;

/**
 * Converts a value of one data type into another data type
 * <code>TRY_CAST(value AS type [FORMAT format])</code>.
 * 
 * @see CastFunction
 * @see CastOperator
 */
@FunctionPlugin
public class TryCastFunction extends CastFunction {

  public TryCastFunction() {
    super("TRY_CAST");
  }

  @Override
  public Object eval(final IExpression[] operands) {
    Object value = operands[0].getValue();
    if (value == null)
      return null;

    Type type = operands[1].getValue(Type.class);

    String format = null;
    if (operands.length == 3) {
      format = operands[2].getValue(String.class);
    }

    try {
      return type.cast(value, format);
    } catch (ConversionException | DateTimeException e) {
      return null;
    }
  }

}
