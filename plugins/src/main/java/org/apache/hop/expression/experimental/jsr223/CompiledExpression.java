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
package org.apache.hop.expression.experimental.jsr223;

import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.IExpression;

public class CompiledExpression extends CompiledScript {

  private final ExpressionScriptEngine engine;
  private final IExpression expression;

  public CompiledExpression(ExpressionScriptEngine engine, IExpression expression) {
    this.engine = engine;
    this.expression = expression;
  }

  @Override
  public Object eval(ScriptContext context) throws ScriptException {
    try {
      return expression.getValue();
    } catch (ExpressionException e) {
      throw new ScriptException(e.getMessage());
    }
  }

  @Override
  public ScriptEngine getEngine() {
    return engine;
  }
}
