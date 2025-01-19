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
    return List.of("hxp");
  }

  @Override
  public List<String> getMimeTypes() {
    return List.of("application/x-hop-expression");
  }

  @Override
  public List<String> getNames() {
    return List.of("hop-expression");
  }

  @Override
  public Object getParameter(String key) {
    return switch (key) {
      case ScriptEngine.ENGINE -> getEngineName();
      case ScriptEngine.ENGINE_VERSION -> getEngineVersion();
      case ScriptEngine.NAME -> getNames();
      case ScriptEngine.LANGUAGE -> getLanguageName();
      case ScriptEngine.LANGUAGE_VERSION -> getLanguageVersion();
      default -> null;
    };
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
