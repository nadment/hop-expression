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

import org.apache.hop.core.row.IRowMeta;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.RowMeta;
import org.apache.hop.core.row.value.ValueMetaBigNumber;
import org.apache.hop.core.row.value.ValueMetaBinary;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaJson;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.core.row.value.ValueMetaString;
import java.util.List;

public class Udf extends Function {
  private UdfMeta meta;

  public Udf(UdfMeta meta) {
    // super(name, name, 10, true, true, "i18n::Operator.Category.Udf", "/docs/udf.html");
    super(meta.getName(), meta.getName(), true, null, null, meta.getArguments().size(), meta.getArguments().size(),
        "i18n::Operator.Category.Udf", "/docs/udf.html");
    this.meta = meta;
  }

  @Override
  public Object eval(IExpressionContext context, IExpression[] operands)
      throws ExpressionException {

    return null;
  }

  public String getSource() {
    return meta.getSource();
  }

  public List<Argument> getArguments() {
    return meta.getArguments();
  }
    
  public  IRowMeta createRowMeta() {
    return createRowMeta(meta.getArguments());
  }
  
  public static IRowMeta createRowMeta(List<Argument> arguments) {
    
    // Convert arguments to row meta
    IRowMeta rowMeta = new RowMeta();
    for (Argument argument:arguments ) {
      IValueMeta vm = createValueMeta(argument.getType(), argument.getName());
      rowMeta.addValueMeta(vm);
    }
    
    return rowMeta;
  }

  private static IValueMeta createValueMeta(DataTypeName type, String name) {
    if (type != null && name!=null) {
      switch (type) {
        case BOOLEAN:
          return new ValueMetaBoolean(name);
        case INTEGER:
          return new ValueMetaInteger(name);
        case NUMBER:
          return new ValueMetaNumber(name);
        case BIGNUMBER:
          return new ValueMetaBigNumber(name);
        case DATE:
          return new ValueMetaDate(name);
        case BINARY:
          return new ValueMetaBinary(name);
        case JSON:
          return new ValueMetaJson(name);
       // case UNKNOWN:
        case STRING:
          return new ValueMetaString(name);
      }
    }
    return null;
  }
}
