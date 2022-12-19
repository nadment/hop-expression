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

import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.logging.ILogChannel;
import org.apache.hop.core.logging.LogChannel;
import org.apache.hop.core.plugins.JarCache;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.metadata.api.IHopMetadataProvider;
import org.apache.hop.metadata.api.IHopMetadataSerializer;
import org.apache.hop.metadata.util.HopMetadataUtil;
import org.jboss.jandex.AnnotationInstance;
import org.jboss.jandex.ClassInfo;
import org.jboss.jandex.IndexView;
import java.io.File;
import java.util.HashMap;
import java.util.Set;

public class FunctionRegistry {
  private static final ILogChannel log = new LogChannel("Expression");

  /** Set of functions or alias by name. */
  private static final HashMap<String, Function> functions = new HashMap<>(256);

  public static boolean isFunction(final String name) {
    return getFunction(name) != null;
  }

  public static Set<Function> getFunctions() {
    return Set.copyOf(functions.values());
  }

  /**
   * Get function by name or alias (ignore case)
   * 
   * @param name
   * @return
   */
  public static Function getFunction(final String name) {
    if (name == null)
      return null;

    return functions.get(name.toUpperCase());
  }

  public static Set<String> getFunctionNames() {
    return functions.keySet();
  }

  /**
   * Initialize the registry, keep private to keep this a singleton
   */
  private FunctionRegistry() {}

  private static void register(final ClassInfo classInfo) {
    try {
      String className = classInfo.name().toString();
      Class<?> clazz = FunctionRegistry.class.getClassLoader().loadClass(className);
      Function function = (Function) clazz.getDeclaredConstructor().newInstance();

      // Register function with it's unique name
      register(function.getId(), function);

      // Register functions with alias names
      FunctionPlugin annotation = clazz.getAnnotation(FunctionPlugin.class);
      for (String name : annotation.names()) {
        register(name, function);
      }
    } catch (Exception e) {
      log.logError("Error registring function " + classInfo.simpleName(), e);
    }
  }
  
  /**
   * Discovery and register built-in and plugin functions
   */
  public static void registerBuilInFunctions() throws HopException {
    JarCache cache = JarCache.getInstance();

    try {
      // Search annotation in native jar
      for (File jarFile : cache.getNativeJars()) {
        IndexView index = cache.getIndex(jarFile);
        for (AnnotationInstance info : index.getAnnotations(FunctionPlugin.class)) {
          register(info.target().asClass());
        }
      }

      // Search annotation in plugins
      for (File jarFile : cache.getPluginJars()) {
        IndexView index = cache.getIndex(jarFile);
        for (AnnotationInstance info : index.getAnnotations(FunctionPlugin.class)) {
          register(info.target().asClass());
        }
      }
    } catch (Exception e) {
      throw new HopException("Error discovering annoted functions", e);
    }
  }

  public static void register(final String name, final Function function) {
    if (name == null)
      return;

    if (functions.containsKey(name)) {
      log.logError("Function '{0}' already registred", name);
      return;
    }

    if (log.isDebug()) {
      log.logDebug("Register function '{0}'", name);
    }

    functions.put(name, function);
  }

  public static Function unregister(final String name) {
    if (name == null)
      return null;

    Function function = functions.remove(name);
    if (function == null) {
      log.logError("Function '{0}' not registred", name);
    }

    return function;
  }

  public static void registerUserDefinedFunctions(IVariables variables) {
    // Unregister User Defined Functions
    for (Function function : FunctionRegistry.getFunctions()) {
      if (function instanceof UserDefinedFunction) {
        FunctionRegistry.unregister(function.getName());
      }
    }

    // Register User Defined Functions
    try {
      IHopMetadataProvider metadataProvider =
          HopMetadataUtil.getStandardHopMetadataProvider(variables);
      IHopMetadataSerializer<UserDefinedFunctionMeta> serializer =
          metadataProvider.getSerializer(UserDefinedFunctionMeta.class);

      for (String name : serializer.listObjectNames()) {
        try {
          if (log.isDebug()) {
            log.logBasic("Register user defined function: " + name);
          }
          UserDefinedFunctionMeta udfMeta = serializer.load(name);
          FunctionRegistry.register(name, new UserDefinedFunction(udfMeta));
        } catch (Exception e) {
          log.logError("Error registring User-defined function " + name, e);
        }
      }

    } catch (HopException e) {
      log.logError("Error registring User-defined functions", e);
    }
  }
}
