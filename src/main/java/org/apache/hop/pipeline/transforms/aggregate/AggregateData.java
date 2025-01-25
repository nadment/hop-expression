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

package org.apache.hop.pipeline.transforms.aggregate;

import java.util.HashMap;
import java.util.Map;
import lombok.Getter;
import org.apache.hop.core.exception.HopValueException;
import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.expression.AggregateFunction;
import org.apache.hop.expression.Call;
import org.apache.hop.expression.IExpressionProcessor;
import org.apache.hop.expression.IRowExpressionContext;
import org.apache.hop.pipeline.transform.BaseTransformData;
import org.apache.hop.pipeline.transform.ITransformData;

public final class AggregateData extends BaseTransformData implements ITransformData {
  @Getter
  public class AggregateKey {
    private final Object[] values;

    public AggregateKey(Object[] groupData) {
      this.values = groupData;
    }

    @Override
    public boolean equals(Object obj) {
      if (obj == null) return false;

      if (this.getClass() != obj.getClass()) return false;

      AggregateKey entry = (AggregateKey) obj;

      try {
        return groupMeta.compare(values, entry.values) == 0;
      } catch (HopValueException e) {
        throw new RuntimeException(e);
      }
    }

    @Override
    public int hashCode() {
      try {
        return groupMeta.hashCode(getHashValue());
      } catch (HopValueException e) {
        throw new RuntimeException(e);
      }
    }

    private Object[] getHashValue() throws HopValueException {
      Object[] groupDataHash = new Object[groupMeta.size()];
      for (int i = 0; i < groupMeta.size(); i++) {
        IValueMeta valueMeta = groupMeta.getValueMeta(i);
        groupDataHash[i] = valueMeta.convertToNormalStorageType(values[i]);
      }
      return groupDataHash;
    }
  }

  public Map<AggregateKey, IExpressionProcessor[]> map;

  public IRowMeta groupMeta;
  public int[] groupIndex;

  public IRowExpressionContext context;
  public Call[] aggregates;
  public AggregateFunction[] functions;
  public IRowMeta aggregateMeta;

  public boolean firstRead;

  public boolean hasOutput;

  public IRowMeta inputRowMeta;
  public IRowMeta outputRowMeta;

  public boolean newBatch;

  public AggregateData() {
    super();
  }

  public AggregateKey createAggregateKey(Object[] row) {
    Object[] groupData = new Object[groupIndex.length];
    for (int i = 0; i < groupIndex.length; i++) {
      groupData[i] = row[groupIndex[i]];
    }

    return new AggregateKey(groupData);
  }

  /** Method responsible for clearing out memory hogs */
  public void clear() {
    map = new HashMap<>();
  }
}
