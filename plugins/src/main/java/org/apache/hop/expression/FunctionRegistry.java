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
import org.jboss.jandex.AnnotationTarget;
import org.jboss.jandex.ClassInfo;
import org.jboss.jandex.DotName;
import org.jboss.jandex.IndexView;
import org.jboss.jandex.MethodInfo;
import java.io.File;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
  
  /**
   * Discovery and register built-in and plugin functions
   */
  public static void registerBuilInFunctions() {
    if (log.isDebug()) {
      log.logDebug("Register built-in functions");
    }
    try {
      List<Method> methods = findAnnotatedMethods(ScalarFunction.class);
      for (Method method : methods) {
        try {
          ScalarFunction annotation = method.getAnnotation(ScalarFunction.class);
          Class<?> clazz = method.getDeclaringClass();
          Object instance = null;

          // If method is not static, need an instance to be invoked.
          // Operator combined with function is not static.
          try {
            instance = clazz.getDeclaredConstructor().newInstance();
          } catch (Exception e) {
            // If class doesn't have constructor, method should be static
          }

          // Create function and register primary name
          Function function = new Function(annotation.id(), annotation.id(),
              annotation.deterministic(), instance, method, annotation.minArgs(),
              annotation.maxArgs(), annotation.category(), annotation.documentationUrl());
          register(function.getId(), function);

          // Create function and register alias name
          for (String name : annotation.names()) {
            function = new Function(annotation.id(), name, annotation.deterministic(), instance,
                method, annotation.minArgs(), annotation.maxArgs(), annotation.category(),
                annotation.documentationUrl());
            register(name, function);
          }
        } catch (Exception e) {
          log.logError("Error registring function " + method, e);
        }
      }
    } catch (Exception e) {
      log.logError("Error discovering annoted functions", e);
    }
  }


    
  private static Method getAnnotatedMethod(MethodInfo methodInfo)
      throws ClassNotFoundException, NoSuchMethodException, SecurityException {
    ClassInfo classInfo = methodInfo.declaringClass();
    Class<?> clazz = Class.forName(classInfo.name().toString());
    return clazz.getMethod(methodInfo.name(), IExpressionContext.class, IExpression[].class);
  }

  protected static List<Method> findAnnotatedMethods(Class<? extends Annotation> annotationClass)
      throws HopException {
    List<Method> methods = new ArrayList<>();
    JarCache cache = JarCache.getInstance();
    DotName annotationName = DotName.createSimple(annotationClass.getName());

    try {

      // Search native jar
      for (File jarFile : cache.getNativeJars()) {
        IndexView index = cache.getIndex(jarFile);

        // find annotations
        for (AnnotationInstance instance : index.getAnnotations(annotationName)) {
          if (instance.target().kind() != AnnotationTarget.Kind.METHOD) {
            continue;
          }
          MethodInfo methodInfo = instance.target().asMethod();
          methods.add(getAnnotatedMethod(methodInfo));
        }
      }

      // Search in plugins
      for (File jarFile : cache.getPluginJars()) {
        IndexView index = cache.getIndex(jarFile);

        // find annotations
        for (AnnotationInstance instance : index.getAnnotations(annotationName)) {
          if (instance.target().kind() != AnnotationTarget.Kind.METHOD) {
            continue;
          }
          MethodInfo methodInfo = instance.target().asMethod();
          methods.add(getAnnotatedMethod(methodInfo));
        }
      }

    } catch (Exception e) {
      throw new HopException("Error finding annotation " + annotationName, e);
    }
    return methods;
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
  
  public static void reloadUserDefinedFunctions(IVariables variables) {
    // Unregister User Defined Functions
    for (Function function : FunctionRegistry.getFunctions()) {
      if (function instanceof Udf) {
        FunctionRegistry.unregister(function.getName());
      }
    }
    
     // Register User Defined Functions
    try {
      IHopMetadataProvider metadataProvider = HopMetadataUtil.getStandardHopMetadataProvider(variables);
      IHopMetadataSerializer<UdfMeta> serializer = metadataProvider.getSerializer(UdfMeta.class);

      for (String name : serializer.listObjectNames()) {
        try {
          if (log.isDebug()) {
            log.logBasic("Register user defined function: " + name);
          }                   
          UdfMeta udfMeta =serializer.load(name);
          FunctionRegistry.register(name, new Udf(udfMeta));
        } catch (Exception e) {
          log.logError("Error registring User-defined function " + name, e);
        }
      }

    } catch (HopException e) {
      log.logError("Error registring User-defined functions", e);
    }
  }
}
