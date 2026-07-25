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

import org.jspecify.annotations.NullMarked;
import org.jspecify.annotations.Nullable;

/**
 * Implemented by classes generated at runtime by {@link JitCompiler} from an {@link
 * org.apache.hop.expression.IExpression} tree.
 *
 * <p>An implementation is bound to a specific expression instance: leaves that could not be
 * inlined (fields, literals, or unsupported sub-calls) are captured at construction time, so
 * {@link #eval()} takes no arguments and simply reads the current row through those captured
 * leaves.
 */
@NullMarked
public interface IJitExpression {

  @Nullable Object eval();
}
