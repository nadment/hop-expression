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
import org.apache.hop.expression.operator.Function;
import org.jboss.jandex.AnnotationInstance;
import org.jboss.jandex.AnnotationTarget;
import org.jboss.jandex.DotName;
import org.jboss.jandex.IndexView;
import org.jboss.jandex.MethodInfo;
import java.io.File;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class OperatorRegistry {

  private static final ILogChannel log = new LogChannel("ExpressionRegistry");

  private static final OperatorRegistry registry = new OperatorRegistry();

  public boolean isFunctionName(String name) {
    return getFunction(name) != null;
  }

  /**
   * Initialize the registry, keep private to keep this a singleton
   */
  private OperatorRegistry() {
    init();
  }

  /**
   * The operator registry instance
   */
  public static OperatorRegistry getInstance() {
    return registry;
  }

  /** Set of functions or alias by name. */
  private final HashMap<String, Function> functions = new HashMap<>(256);

  /** Set of operators. */
  private final Set<Operator> operators = new TreeSet<>(Arrays.asList(Operator.ADD,
      Operator.SUBTRACT, Operator.MULTIPLY, Operator.DIVIDE, Operator.BITAND, Operator.BITOR,
      Operator.BITNOT, Operator.BITXOR, Operator.CAST, Operator.MODULUS, Operator.EQUAL,
      Operator.GREATER_THAN, Operator.GREATER_THAN_OR_EQUAL, Operator.ILIKE, Operator.LESS_THAN,
      Operator.LESS_THAN_OR_EQUAL, Operator.LESS_THAN_OR_GREATER_THAN, Operator.NOT_EQUAL,
      Operator.BOOLAND, Operator.BETWEEN, Operator.CASE, Operator.CONCAT, Operator.IN, Operator.IS,
      Operator.LIKE, Operator.BOOLNOT, Operator.BOOLOR, Operator.BOOLXOR));

  public Set<Operator> getOperators() {
    return operators;
  }

  /**
   * Get function by name or alias (ignore case)
   * 
   * @param name
   * @return
   */
  public Function getFunction(final String name) {
    if (name == null)
      return null;

    return functions.get(name.toUpperCase());
  }

  /**
   * Register functions
   */
  protected void init() {
    try {

      List<Method> methods = findAnnotatedMethods(ScalarFunction.class);
      for (Method method : methods) {
        try {
          ScalarFunction annotation = method.getAnnotation(ScalarFunction.class);
          Class<?> clazz = method.getDeclaringClass();
          Object instance = null;

          try {
            instance = (Operator) clazz.newInstance();
          } catch (Exception e) {
            // Ignnore
          }


          if ( functions.containsKey(annotation.name()) ) {
            log.logError("Function already registred " + annotation.name());
            continue;
          }

         // log.logBasic("Register function " + annotation.name());
          
          // Create function
          Function function = new Function(annotation.name(), null, annotation.deterministic(),
              instance, method, annotation.minArgs(), annotation.maxArgs(), annotation.category());
          operators.add(function);
          functions.put(function.getName(), function);

          // Create function alias
          for (String alias : annotation.alias()) {
            // log.logBasic("Register alias " + alias + " to function " + annotation.name());

            function = new Function(annotation.name(), alias, annotation.deterministic(), instance,
                method, annotation.minArgs(), annotation.maxArgs(), annotation.category());
            operators.add(function);
            functions.put(alias, function);
          }
        } catch (Exception e) {
          log.logError("Error registring function " + method, e);
        }
      }


    } catch (Exception e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  private List<Method> findAnnotatedMethods(Class<?> clazz,
      Class<? extends Annotation> annotationClass) {
    Method[] methods = clazz.getMethods();
    List<Method> annotatedMethods = new ArrayList<>(methods.length);
    for (Method method : methods) {
      if (method.isAnnotationPresent(annotationClass)) {
        annotatedMethods.add(method);
      }
    }
    return annotatedMethods;
  }

  protected List<Method> findAnnotatedMethods(Class<? extends Annotation> annotationClass)
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

          MethodInfo info = instance.target().asMethod();
          Class<?> clazz = Class.forName(info.declaringClass().name().toString());
          methods.addAll(findAnnotatedMethods(clazz, annotationClass));
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

          MethodInfo info = instance.target().asMethod();
          Class<?> clazz = Class.forName(info.declaringClass().name().toString());
          methods.addAll(findAnnotatedMethods(clazz, annotationClass));
        }
      }

    } catch (Exception e) {
      throw new HopException("Error finding annotation " + annotationName, e);
    }
    return methods;
  }


}