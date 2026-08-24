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
package org.apache.hop.expression.util;

import org.apache.hop.expression.IExpression;

public abstract class BaseFormat {

  protected static final Class<?> PKG = IExpression.class; // for i18n purposes

  protected BaseFormat() {
    super();
  }

  protected static boolean startsWithIgnoreCase(String str, int offset, String prefix) {
    if (prefix.length() > str.length() - offset) {
      return false;
    }

    return str.regionMatches(true, offset, prefix, 0, prefix.length());
  }

  protected static boolean startsWithIgnoreCase(String str, int offset, String... substrings) {
    for (String substring : substrings) {
      if (str.regionMatches(true, offset, substring, 0, substring.length())) return true;
    }
    return false;
  }

  protected static boolean endsWithIgnoreCase(String str, String suffix) {
    if (suffix.length() > str.length()) {
      return false;
    }
    int offset = str.length() - suffix.length();
    return str.regionMatches(true, offset, suffix, 0, suffix.length());
  }
}
