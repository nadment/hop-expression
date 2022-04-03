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
package org.apache.hop.expression.experimental.jsr223;

import org.apache.hop.expression.ExpressionContext;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.ExpressionParser;
import org.apache.hop.expression.IExpression;
import java.io.IOException;
import java.io.Reader;
import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;

/**
 * Implements the Expression ScriptEngine for JSR-223.
 */
public final class ExpressionScriptEngine extends AbstractScriptEngine
    implements Compilable {

  private ExpressionScriptEngineFactory factory;

  @Override
  public synchronized ScriptEngineFactory getFactory() {
    if (this.factory == null) {
      this.factory = new ExpressionScriptEngineFactory();
    }
    return this.factory;
  }

  @Override
  public Bindings createBindings() {
    return new RowExpressionBindings();
  }

  @Override
  public Object eval(String script, ScriptContext context) throws ScriptException {
    try {
      IExpression expression = ExpressionParser.parse(script);
      return expression.eval((ExpressionContext) context);
    } catch (Exception e) {
      throw new ScriptException(e);
    }
  }

  @Override
  public Object eval(Reader reader, ScriptContext context) throws ScriptException {
    return this.eval(readFully(reader), context);
  }

  private static String readFully(Reader reader) throws ScriptException {
    char[] buffer = new char[8192];
    StringBuilder builder = new StringBuilder();

    int numChars;
    try {
      while ((numChars = reader.read(buffer, 0, buffer.length)) > 0) {
        builder.append(buffer, 0, numChars);
      }
    } catch (IOException e) {
      throw new ScriptException(e);
    }

    return builder.toString();
  }

  @Override
  public CompiledScript compile(String script) throws ScriptException {
    try {
      IExpression expression = ExpressionParser.parse(script);
      return new CompiledExpression(this, expression);
    } catch (ExpressionException e) {
      throw new ScriptException("Unable to compile expression: " + script);
    }
  }

  @Override
  public CompiledScript compile(Reader reader) throws ScriptException {
    return this.compile(readFully(reader));
  }
}
