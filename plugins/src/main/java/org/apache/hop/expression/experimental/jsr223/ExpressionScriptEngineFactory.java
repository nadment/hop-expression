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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;

public class ExpressionScriptEngineFactory implements ScriptEngineFactory {
  private static final String ENGINE_NAME = "HOP Expression Language";
  private static final String ENGINE_VERSION = "0.1";
  private static final String LANGUAGE_NAME = "hop-expression";
  private static final String LANGUAGE_VERSION = "0.1";

  @Override
  public String getLanguageName() {
    return LANGUAGE_NAME;
  }

  @Override
  public String getLanguageVersion() {
    return LANGUAGE_VERSION;
  }

  @Override
  public String getEngineName() {
    return ENGINE_NAME;
  }

  @Override
  public String getEngineVersion() {
    return ENGINE_VERSION;
  }

  @Override
  public List<String> getExtensions() {
    return Collections.unmodifiableList(Arrays.asList("hxp"));
  }

  @Override
  public List<String> getMimeTypes() {
    return Collections.unmodifiableList(Arrays.asList("application/x-hop-expression"));
  }

  @Override
  public List<String> getNames() {
    return Collections.unmodifiableList(Arrays.asList("hop-expression"));
  }

  @Override
  public Object getParameter(String key) {
    switch (key) {
      case ScriptEngine.ENGINE:
        return getEngineName();
      case ScriptEngine.ENGINE_VERSION:
        return getEngineVersion();
      case ScriptEngine.NAME:
        return getNames();
      case ScriptEngine.LANGUAGE:
        return getLanguageName();
      case ScriptEngine.LANGUAGE_VERSION:
        return getLanguageVersion();
      default:
        return null;
    }
  }

  @Override
  public String getMethodCallSyntax(String obj, String m, String... args) {
    return null;
  }

  @Override
  public String getOutputStatement(String toDisplay) {
    return null;
  }

  @Override
  public String getProgram(final String... statements) {
    final StringBuilder sb = new StringBuilder();
    for (final String statement : statements) {
      sb.append(statement.trim());
      if (!statement.endsWith(";")) {
        sb.append(';');
      }
    }
    return sb.toString();
  }

  @Override
  public ScriptEngine getScriptEngine() {
    return new ExpressionScriptEngine();
  }
}
