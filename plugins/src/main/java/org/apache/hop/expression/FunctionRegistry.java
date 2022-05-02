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
  private static final HashMap<String, Function> functions  = new HashMap<>(256);
  
//  static {
//    init();
//  }

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
  private FunctionRegistry() {
  }

  /**
   * Register functions
   */
  public static void init() {
    if (log.isDebug()) {
      log.logDebug("Init expression FunctionRegistry");
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

          if (functions.containsKey(annotation.id())) {
            log.logError("Function '{0}' already registred", annotation.id());
            continue;
          }

          if (log.isDebug()) {
            log.logDebug("Register function '{0}'", annotation.id());
          }

          // Create function
          Function function = new Function(annotation.id(), annotation.id(), annotation.deterministic(),
              instance, method, annotation.minArgs(), annotation.maxArgs(), annotation.category(), annotation.documentationUrl());
          functions.put(function.getId(), function);
  
          // Create function alias name
          for (String name : annotation.names()) {
            function = new Function(annotation.id(), name, annotation.deterministic(), instance,
                method, annotation.minArgs(), annotation.maxArgs(), annotation.category(), annotation.documentationUrl());
            functions.put(name, function);

            if (log.isDebug()) {
              log.logDebug("Register alias {1} to function {0}" + function.getId(), function.getName());
            }
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
}
