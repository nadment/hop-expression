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

import org.apache.hop.core.logging.ILogChannel;
import org.apache.hop.core.logging.LogChannel;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class OperatorRegistry {
  
  private static final ILogChannel log = new LogChannel( "OperatorRegistry");
  
  private static final OperatorRegistry registry = new OperatorRegistry();
  
  /**
   * Initialize the registry, keep private to keep this a singleton
   */
  private OperatorRegistry() {
     super();
     
     List<Method> methods = findAnnotatedMethods(Function.class, ScalarFunction.class);
     for (Method method : methods) {
       try {
         ScalarFunction annotation = method.getAnnotation(ScalarFunction.class);
         
         log.logBasic("Register function " + annotation.name());
         
         Kind kind = Kind.valueOf(annotation.name());
                  
         // Create function
         createFunction(kind, annotation.name(), false, annotation.deterministic(), method, annotation.minArgs(), annotation.maxArgs(), annotation.category());
         
         // Create alias
         for (String name : annotation.alias()) {
           createFunction(kind, name, true, annotation.deterministic(), method, annotation.minArgs(), annotation.maxArgs(), annotation.category());
         }
       } catch (Exception e) {
         log.logError("Error registring function " + method, e);
       }
     }
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
  private final Set<Operator> operators = new TreeSet<>(Arrays.asList(Operator.ADD, Operator.SUBTRACT,
      Operator.MULTIPLY, Operator.DIVIDE, Operator.POWER, Operator.BITAND, Operator.BITOR, Operator.BITNOT, Operator.BITXOR, Operator.CAST, Operator.MODULUS, Operator.EQUAL, Operator.GREATER_THAN,
      Operator.GREATER_THAN_OR_EQUAL, Operator.ILIKE, Operator.LESS_THAN, Operator.LESS_THAN_OR_EQUAL, Operator.LESS_THAN_OR_GREATER_THAN,
      Operator.NOT_EQUAL, Operator.BOOLAND, Operator.BETWEEN, Operator.CASE, Operator.CONCAT, Operator.IN, Operator.IS, Operator.LIKE, Operator.BOOLNOT, Operator.BOOLOR, Operator.BOOLXOR));

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
  

  private List<Method> findAnnotatedMethods(Class<?> clazz,
      Class<? extends Annotation> annotationClass) {
    Method[] methods = clazz.getMethods();
    List<Method> annotatedMethods = new ArrayList<Method>(methods.length);
    for (Method method : methods) {
      if (method.isAnnotationPresent(annotationClass)) {
        annotatedMethods.add(method);
      }
    }
    return annotatedMethods;
  }
  
  private void createFunction(Kind kind, String name, boolean isAlias,
      boolean isDeterministic, Method method, int min, int max, String category) {
    Function function = new Function(kind, name, isAlias, isDeterministic, method, min, max, category);
    operators.add(function);
    functions.put(name, function);
  }
}
