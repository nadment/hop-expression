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
import org.apache.hop.expression.operator.Add;
import org.apache.hop.expression.operator.Between;
import org.apache.hop.expression.operator.BitAnd;
import org.apache.hop.expression.operator.BitNot;
import org.apache.hop.expression.operator.BitOr;
import org.apache.hop.expression.operator.BitXor;
import org.apache.hop.expression.operator.BoolAnd;
import org.apache.hop.expression.operator.BoolNot;
import org.apache.hop.expression.operator.BoolOr;
import org.apache.hop.expression.operator.BoolXor;
import org.apache.hop.expression.operator.Case;
import org.apache.hop.expression.operator.Cast;
import org.apache.hop.expression.operator.Concat;
import org.apache.hop.expression.operator.Divide;
import org.apache.hop.expression.operator.Equal;
import org.apache.hop.expression.operator.Extract;
import org.apache.hop.expression.operator.GreaterThan;
import org.apache.hop.expression.operator.GreaterThanOrEqual;
import org.apache.hop.expression.operator.ILike;
import org.apache.hop.expression.operator.In;
import org.apache.hop.expression.operator.Is;
import org.apache.hop.expression.operator.LessThan;
import org.apache.hop.expression.operator.LessThanOrEqual;
import org.apache.hop.expression.operator.LessThanOrGreaterThan;
import org.apache.hop.expression.operator.Like;
import org.apache.hop.expression.operator.Mod;
import org.apache.hop.expression.operator.Multiply;
import org.apache.hop.expression.operator.Negative;
import org.apache.hop.expression.operator.NotEqual;
import org.apache.hop.expression.operator.Position;
import org.apache.hop.expression.operator.Subtract;
import org.apache.hop.expression.operator.TryCast;
import org.apache.hop.expression.operator.AtTimeZone;
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class OperatorRegistry {

  private static final ILogChannel log = new LogChannel("Expression");

  // -------------------------------------------------------------
  // BITWISE OPERATORS
  // -------------------------------------------------------------
  public static final Operator BITAND = new BitAnd();
  public static final Operator BITOR = new BitOr();
  public static final Operator BITNOT = new BitNot();
  public static final Operator BITXOR = new BitXor();

  // -------------------------------------------------------------
  // LOGICAL OPERATORS
  // -------------------------------------------------------------
  public static final Operator BOOLNOT = new BoolNot();
  public static final Operator BOOLOR = new BoolOr();
  public static final Operator BOOLAND = new BoolAnd();
  public static final Operator BOOLXOR = new BoolXor();

  // -------------------------------------------------------------
  // CONDITIONAL OPERATORS
  // -------------------------------------------------------------
  public static final Operator CASE = new Case();
  
  // -------------------------------------------------------------
  // COMPARISON OPERATORS
  // -------------------------------------------------------------
  public static final Operator IS = new Is();
  public static final Operator IN = new In();
  public static final Operator LIKE = new Like();
  public static final Operator ILIKE = new ILike();
  public static final Operator BETWEEN = new Between();
  public static final Operator EQUAL = new Equal();
  public static final Operator NOT_EQUAL = new NotEqual();
  public static final Operator LESS_THAN_OR_GREATER_THAN = new LessThanOrGreaterThan();
  public static final Operator LESS_THAN = new LessThan();
  public static final Operator LESS_THAN_OR_EQUAL = new LessThanOrEqual();
  public static final Operator GREATER_THAN = new GreaterThan();
  public static final Operator GREATER_THAN_OR_EQUAL = new GreaterThanOrEqual();

  // -------------------------------------------------------------
  // ARITHMETIC OPERATORS
  // -------------------------------------------------------------
  public static final Operator NEGATIVE = new Negative();
  public static final Operator MULTIPLY = new Multiply();
  public static final Operator DIVIDE = new Divide();
  public static final Operator MODULUS = new Mod();
  public static final Operator ADD = new Add();
  public static final Operator SUBTRACT = new Subtract();

  // -------------------------------------------------------------
  // SPECIAL OPERATORS
  // -------------------------------------------------------------
  public static final Operator CAST = new Cast();
  public static final Operator TRY_CAST = new TryCast();
  public static final Operator AT_TIME_ZONE = new AtTimeZone();
  public static final Operator CONCAT = new Concat();
  public static final Operator EXTRACT = new Extract();
  public static final Operator POSITION = new Position();
  
  static {
    operators = new TreeSet<>(Arrays.asList(ADD, SUBTRACT, MULTIPLY, DIVIDE, BITAND, BITOR, BITNOT,
        BITXOR, CAST, MODULUS, EQUAL, GREATER_THAN, GREATER_THAN_OR_EQUAL, ILIKE, LESS_THAN,
        LESS_THAN_OR_EQUAL, LESS_THAN_OR_GREATER_THAN, NOT_EQUAL, BOOLAND, BETWEEN, CASE, CONCAT,
        IN, IS, LIKE, BOOLNOT, BOOLOR, BOOLXOR));
   
    functions = new HashMap<>(256);

    init();
  }

  public static boolean isFunctionName(String name) {
    return getFunction(name) != null;
  }

  /** Set of operators. */
  private static final Set<Operator> operators;

  /** Set of functions or alias by name. */
  private static final HashMap<String, Function> functions;

  public static Set<Operator> getOperators() {
    return operators;
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
  private OperatorRegistry() {
  }

  /**
   * Register functions
   */
  private static void init() {
    try {
      
     // functions.put(CAST.getName(), CAST);
      
      List<Method> methods = findAnnotatedMethods(ScalarFunction.class);
      for (Method method : methods) {
        try {
          ScalarFunction annotation = method.getAnnotation(ScalarFunction.class);
          Class<?> clazz = method.getDeclaringClass();
          Object instance = null;

          // If method is not static, need an instance to be invoked.
          // Operator combined with function is not static.
          try {
            instance = clazz.newInstance();
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
          operators.add(function);
          functions.put(function.getId(), function);
  
          // Create function alias name
          for (String name : annotation.names()) {
            function = new Function(annotation.id(), name, annotation.deterministic(), instance,
                method, annotation.minArgs(), annotation.maxArgs(), annotation.category(), annotation.documentationUrl());
            operators.add(function);
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
