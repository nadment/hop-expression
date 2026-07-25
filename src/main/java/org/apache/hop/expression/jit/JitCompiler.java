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
package org.apache.hop.expression.jit;

import java.lang.reflect.Constructor;
import java.util.concurrent.atomic.AtomicLong;
import org.apache.hop.core.logging.ILogChannel;
import org.apache.hop.core.logging.LogChannel;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.type.Type;
import org.codehaus.janino.SimpleCompiler;
import org.jspecify.annotations.NullMarked;

/**
 * Compiles a validated and optimized {@link IExpression} tree into JVM bytecode at runtime, using
 * <a href="https://janino-compiler.github.io/janino/">Janino</a> as a lightweight, embeddable Java
 * compiler.
 *
 * <p>This is a prototype: only a subset of operators currently support code generation (see the
 * various {@code generateCode} overrides in {@code org.apache.hop.expression.operator}). Any
 * operator, sub-expression, or failure not covered falls back transparently to the existing
 * tree-walking interpreter, so enabling the JIT can only add speed, never change behavior or
 * cause a hard failure: {@link #tryCompile} never throws and always returns a usable expression.
 *
 * <p>Compilation cost is only worth paying when the expression will be evaluated many times (for
 * example once per row of a transform), since building and loading a class takes milliseconds.
 * Callers should compile once per expression instance and reuse the result, never per row.
 */
@NullMarked
public final class JitCompiler {

  private static final ILogChannel LOG = new LogChannel("Expression JIT");
  private static final AtomicLong CLASS_COUNTER = new AtomicLong();

  private JitCompiler() {}

  /**
   * Attempts to JIT-compile the given expression.
   *
   * @param expression a validated and optimized expression (as returned by {@link
   *     IExpression#of})
   * @return a compiled expression behaviorally equivalent to the input, or the input itself
   *     unchanged if nothing could be compiled or compilation failed for any reason
   */
  public static IExpression tryCompile(IExpression expression) {
    // Only Call trees can benefit: a bare literal or field is already O(1) to evaluate.
    if (!(expression instanceof Call)) {
      return expression;
    }

    try {
      JitContext context = new JitContext();
      JitCodeGenerator generator = new JitCodeGenerator(context);
      String body = expression.accept(generator);

      // If not a single operator in the tree supports code generation, the "compiled" class
      // would do nothing but forward to the interpreter through a leaf. Not worth the overhead.
      if (!generator.hasGeneratedCode()) {
        return expression;
      }

      String className = "JitExpression" + CLASS_COUNTER.incrementAndGet();
      String source = generateSource(className, body);

      SimpleCompiler compiler = new SimpleCompiler();
      compiler.cook(source);
      Class<?> clazz = compiler.getClassLoader().loadClass(className);
      Constructor<?> constructor = clazz.getConstructor(IExpression[].class, Type[].class);

      IJitExpression compiled =
          (IJitExpression)
              constructor.newInstance(
                  context.getLeaves().toArray(new IExpression[0]),
                  context.getTypes().toArray(new Type[0]));

      return new JitCompiledExpression(expression, compiled);
    } catch (Exception e) {
      LOG.logDebug(
          "JIT compilation failed for expression [{0}], falling back to interpreted evaluation: {1}",
          expression, e.getMessage());
      return expression;
    }
  }

  private static String generateSource(String className, String body) {
    return """
        public final class %s implements org.apache.hop.expression.jit.IJitExpression {
          private final org.apache.hop.expression.IExpression[] leaves;
          private final org.apache.hop.expression.type.Type[] types;

          public %s(org.apache.hop.expression.IExpression[] leaves, org.apache.hop.expression.type.Type[] types) {
            this.leaves = leaves;
            this.types = types;
          }

          public Object eval() {
            return %s;
          }
        }
        """
        .formatted(className, className, body);
  }
}
