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
package org.apache.hop.expression;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.hop.junit.rules.RestoreHopEnvironmentExtension;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Checks that the user manual documents the registered functions: each function has its page, each
 * name of a function, an alias included, is documented, and the index of the functions lists only
 * registered functions.
 */
@ExtendWith({RestoreHopEnvironmentExtension.class, ExpressionExtension.class})
class DocumentationTest {

  /** The pages of the user manual, relative to the project directory where the tests run. */
  private static final Path PAGES = Path.of("src/main/doc/hop-user-manual/expression");

  /** A function in the index, like {@code xref:abs.adoc["ABS",role=fun]}. */
  private static final Pattern INDEX_ENTRY =
      Pattern.compile("xref:([a-z0-9_-]+)\\.adoc\\[\"([A-Z0-9_]+)\",role=fun\\]");

  /** The page of each function listed in the index, by name. */
  private static final Map<String, String> INDEX = new TreeMap<>();

  @BeforeAll
  static void readIndex() throws IOException {
    Matcher matcher = INDEX_ENTRY.matcher(Files.readString(PAGES.resolve("functions.adoc")));
    while (matcher.find()) {
      INDEX.put(matcher.group(2), matcher.group(1));
    }
  }

  /** Returns the name of the page of a function, from its documentation URL. */
  private static String page(Function function) {
    String url = function.getDocumentationUrl();
    return url.substring(url.lastIndexOf('/') + 1, url.length() - ".html".length());
  }

  @Test
  void everyFunctionHasPage() {
    List<String> missing = new ArrayList<>();
    for (Function function : FunctionRegistry.getFunctions()) {
      if (!Files.exists(PAGES.resolve(page(function) + ".adoc"))) {
        missing.add(function.getName() + " → " + page(function) + ".adoc");
      }
    }
    assertEquals(List.of(), missing, "Functions without page");
  }

  @Test
  void everyFunctionNameIsDocumented() throws IOException {
    // A name is listed in the index, or mentioned on the page of its function like an alias
    List<String> missing = new ArrayList<>();
    for (String name : FunctionRegistry.getFunctionNames()) {
      Function function = FunctionRegistry.getFunction(name);
      String text = Files.readString(PAGES.resolve(page(function) + ".adoc"));
      if (!INDEX.containsKey(name) && !text.contains("`" + name + "`")) {
        missing.add(name + " → " + page(function) + ".adoc");
      }
    }
    assertEquals(List.of(), missing, "Function names not documented");
  }

  @Test
  void everyIndexEntryIsRegisteredFunction() {
    // An operator listed in the index, like CASE, uses the role op
    List<String> wrong = new ArrayList<>();
    for (Map.Entry<String, String> entry : INDEX.entrySet()) {
      Function function = FunctionRegistry.getFunction(entry.getKey());
      if (function == null) {
        wrong.add(entry.getKey() + " is not a function");
      } else if (!page(function).equals(entry.getValue())) {
        wrong.add(entry.getKey() + " → " + entry.getValue() + ".adoc instead of " + page(function));
      }
    }
    assertEquals(List.of(), wrong, "Wrong entries in functions.adoc");
  }
}
