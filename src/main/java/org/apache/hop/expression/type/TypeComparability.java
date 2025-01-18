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
package org.apache.hop.expression.type;

/**
 * TypeComparability is an enumeration of the categories of comparison operators which types may
 * support.
 *
 * <p>The order of values of this enumeration is* significant (from least inclusive to most
 * inclusive) and should not be changed.
 */
public enum TypeComparability {
  /** No comparisons allowed */
  NONE,
  /** Only equals/not-equals allowed (if the type supports <code>equalTo</code>) */
  UNORDERED,
  /** All comparisons allowed (if the type supports <code>compareTo</code>) */
  ALL
}
