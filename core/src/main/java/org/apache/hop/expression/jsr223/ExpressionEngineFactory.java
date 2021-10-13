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
package org.apache.hop.expression.jsr223;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;

public class ExpressionEngineFactory implements ScriptEngineFactory {
  private static final String ENGINE_NAME = "HOP Expression Language";
  private static final String ENGINE_VERSION = "0.1";
  private static final String LANGUAGE_NAME = "expression";
  private static final String LANGUAGE_VERSION = "0.1";

  private static final List<String> NAMES;
  private static final List<String> EXTENSIONS;
  private static final List<String> MIME_TYPES;

  private static final ExpressionEngine EXPRESSION_ENGINE = new ExpressionEngine();

  static {
    List<String> n = new ArrayList<>(1);
    n.add(LANGUAGE_NAME);
    NAMES = Collections.unmodifiableList(n);

    EXTENSIONS = NAMES;

    n = new ArrayList<>(1);
    n.add("application/x-" + LANGUAGE_NAME);
    MIME_TYPES = Collections.unmodifiableList(n);
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
    return EXTENSIONS;
  }

  @Override
  public List<String> getMimeTypes() {
    return MIME_TYPES;
  }

  @Override
  public List<String> getNames() {
    return NAMES;
  }

  @Override
  public String getLanguageName() {
    return LANGUAGE_NAME;
  }

  @Override
  public String getLanguageVersion() {
    return LANGUAGE_VERSION;
  }

  @Override
  public Object getParameter(String key) {
    if (ScriptEngine.NAME.equals(key)) {
      return getLanguageName();
    } else if (ScriptEngine.ENGINE.equals(key)) {
      return getEngineName();
    } else if (ScriptEngine.ENGINE_VERSION.equals(key)) {
      return getEngineVersion();
    } else if (ScriptEngine.LANGUAGE.equals(key)) {
      return getLanguageName();
    } else if (ScriptEngine.LANGUAGE_VERSION.equals(key)) {
      return getLanguageVersion();
    } else {
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
  public String getProgram(String... statements) {
    return null;
  }

  @Override
  public ScriptEngine getScriptEngine() {
    return EXPRESSION_ENGINE;
  }
}
