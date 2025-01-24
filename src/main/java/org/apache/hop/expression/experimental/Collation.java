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
package org.apache.hop.expression.experimental;

import java.nio.charset.Charset;
import java.text.Collator;
import java.util.Locale;
import lombok.Getter;

public class Collation {

  @Getter private final String name;
  @Getter private final Charset charset;
  private final Locale locale;

  public Collation(Locale locale, Charset charset) {
    this.locale = locale;
    this.charset = charset;
    this.name = charset.name().toUpperCase(Locale.ROOT) + "$" + locale;
  }

  @Override
  public boolean equals(Object o) {
    return this == o || o instanceof Collation && name.equals(((Collation) o).name);
  }

  @Override
  public int hashCode() {
    return name.hashCode();
  }

  public final Locale getLocale() {
    return locale;
  }

  /**
   * Returns the {@link Collator} to compare values having the current collation, or {@code null} if
   * no specific {@link Collator} is needed, in which case {@link String#compareTo} will be used.
   */
  public Collator getCollator() {
    return null;
  }
}
