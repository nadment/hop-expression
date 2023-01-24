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

import org.apache.hop.core.search.BaseMetadataSearchableAnalyser;
import org.apache.hop.core.search.ISearchQuery;
import org.apache.hop.core.search.ISearchResult;
import org.apache.hop.core.search.ISearchable;
import org.apache.hop.core.search.ISearchableAnalyser;
import org.apache.hop.core.search.SearchableAnalyserPlugin;
import java.util.ArrayList;
import java.util.List;

@SearchableAnalyserPlugin(id = "UserDefinedFunctionSearchableAnalyser",
    name = "Search in user defined function metadata")
public class UserDefinedFunctionSearchableAnalyser
    extends BaseMetadataSearchableAnalyser<UserDefinedFunctionMeta>
    implements ISearchableAnalyser<UserDefinedFunctionMeta> {

  @Override
  public Class<UserDefinedFunctionMeta> getSearchableClass() {
    return UserDefinedFunctionMeta.class;
  }

  @Override
  public List<ISearchResult> search(ISearchable<UserDefinedFunctionMeta> searchable,
      ISearchQuery searchQuery) {
    UserDefinedFunctionMeta meta = searchable.getSearchableObject();
    String component = getMetadataComponent();

    List<ISearchResult> results = new ArrayList<>();

    matchProperty(searchable, results, searchQuery, "Function name", meta.getName(), component);
    matchProperty(searchable, results, searchQuery, "Function source", meta.getSource(), component);
    return results;
  }
}
