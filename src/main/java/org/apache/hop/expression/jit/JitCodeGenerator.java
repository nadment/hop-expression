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

import org.apache.hop.expression.Array;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionVisitor;
import org.apache.hop.expression.Identifier;
import org.apache.hop.expression.Literal;
import org.jspecify.annotations.NullMarked;

/**
 * Translates a validated and optimized {@link org.apache.hop.expression.IExpression} tree into a
 * Java source expression suitable for compilation by {@link JitCompiler}.
 *
 * <p>Every generated snippet evaluates, at runtime, to a (possibly {@code null}) {@code Object} -
 * exactly like {@link org.apache.hop.expression.IExpression#getValue()} - so operators can be
 * added incrementally without needing to model Java primitive types in the generator.
 *
 * <p>Nodes this generator does not know how to translate (because the operator has no {@link
 * org.apache.hop.expression.Operator#generateCode} support, or because the node is a leaf) are not
 * an error: they are simply captured as-is and evaluated through the normal interpreted path at
 * runtime, via {@link JitContext#addLeaf}. This makes JIT support for new operators strictly
 * additive.
 */
@NullMarked
public final class JitCodeGenerator implements IExpressionVisitor<String> {

  private final JitContext context;

  /** Tracks whether at least one node was actually translated to generated code. */
  private boolean hasGeneratedCode;

  public JitCodeGenerator(JitContext context) {
    this.context = context;
  }

  /** Returns {@code true} if at least one {@link Call} was translated by an operator. */
  public boolean hasGeneratedCode() {
    return hasGeneratedCode;
  }

  @Override
  public String visitIdentifier(Identifier identifier) {
    return leaf(identifier);
  }

  @Override
  public String visitLiteral(Literal literal) {
    return leaf(literal);
  }

  @Override
  public String visitArray(Array array) {
    return leaf(array);
  }

  @Override
  public String visitCall(Call call) {
    String[] operandCode = new String[call.getOperandCount()];
    for (int i = 0; i < operandCode.length; i++) {
      operandCode[i] = call.getOperand(i).accept(this);
    }

    String generated = call.getOperator().generateCode(context, call, operandCode);
    if (generated != null) {
      hasGeneratedCode = true;
      return "(" + generated + ")";
    }

    // No code generation support for this operator: fall back to interpreted evaluation for
    // this whole sub-tree.
    return leaf(call);
  }

  private String leaf(IExpression expression) {
    int index = context.addLeaf(expression);
    return "leaves[" + index + "].getValue()";
  }
}
