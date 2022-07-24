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
package org.apache.hop.expression.util;

import org.apache.commons.io.IOUtils;
import org.apache.hop.core.logging.ILogChannel;
import org.apache.hop.core.logging.LogChannel;
import org.apache.hop.core.row.IValueMeta;
import org.apache.hop.core.row.value.ValueMetaBigNumber;
import org.apache.hop.core.row.value.ValueMetaBinary;
import org.apache.hop.core.row.value.ValueMetaBoolean;
import org.apache.hop.core.row.value.ValueMetaDate;
import org.apache.hop.core.row.value.ValueMetaInteger;
import org.apache.hop.core.row.value.ValueMetaJson;
import org.apache.hop.core.row.value.ValueMetaNone;
import org.apache.hop.core.row.value.ValueMetaNumber;
import org.apache.hop.core.row.value.ValueMetaString;
import org.apache.hop.expression.DataType;
import org.apache.hop.expression.DataTypeName;
import org.apache.hop.expression.IExpression;
import java.io.InputStreamReader;
import java.io.StringWriter;

public class ExpressionUtil {
  private static final ILogChannel LOG = new LogChannel("Expression");

  private ExpressionUtil() {
    super();
  }

  public static String loadDocumention(String id, String url) {
    StringWriter writer = new StringWriter();
    try (InputStreamReader is = new InputStreamReader(IExpression.class.getResourceAsStream(url))) {
      IOUtils.copy(is, writer);
    } catch (Exception e) {
      LOG.logError("Missing operator documentation: {0}", id);
      return null;
    }

    return writer.toString();
  }

  public static String findDocumentionDescription(String doc) {
    if (doc == null) {
      return "";
    }

    int beginIndex = doc.indexOf("id=\"preamble\"");
    beginIndex = doc.indexOf("<p>", beginIndex);

    if (beginIndex > 0) {
      int endIndex = doc.indexOf("</p>", beginIndex);

      return doc.substring(beginIndex + 3, endIndex);
    }

    return "";
  }

  public static DataTypeName createDataType(IValueMeta valueMeta) {
    switch (valueMeta.getType()) {
      case IValueMeta.TYPE_BOOLEAN:
        return DataTypeName.BOOLEAN;
      case IValueMeta.TYPE_DATE:
      case IValueMeta.TYPE_TIMESTAMP:
        return DataTypeName.DATE;
      case IValueMeta.TYPE_STRING:
        return DataTypeName.STRING;
      case IValueMeta.TYPE_INTEGER:
        return DataTypeName.INTEGER;
      case IValueMeta.TYPE_NUMBER:
        return DataTypeName.NUMBER;
      case IValueMeta.TYPE_BIGNUMBER:
        return DataTypeName.BIGNUMBER;
      case ValueMetaJson.TYPE_JSON:
        return DataTypeName.JSON;
      case IValueMeta.TYPE_BINARY:
        return DataTypeName.BINARY;
      default:
        return DataTypeName.UNKNOWN;
    }
  }

  public static IValueMeta createValueMeta(final String name, final DataTypeName type) {
    switch (type) {
      case BOOLEAN:
        return new ValueMetaBoolean(name);
      case INTEGER: // Max 2.147.483.647
        return new ValueMetaInteger(name, 9, 0);
      case NUMBER:
        return new ValueMetaNumber(name, -1, -1);
      case BIGNUMBER:
        return new ValueMetaBigNumber(name, -1, -1);
      case STRING:
        return new ValueMetaString(name, -1, -1);
      case DATE:
        return new ValueMetaDate(name, -1, -1);
      case BINARY:
        return new ValueMetaBinary(name, -1, -1);
      default:
        return new ValueMetaNone(name);
    }
  }
  
  public static IValueMeta createValueMeta(final String name, final DataType type) {
    switch (type.getName()) {
      case BOOLEAN:
        return new ValueMetaBoolean(name);
      case INTEGER:
        return new ValueMetaInteger(name);
      case NUMBER:
        return new ValueMetaNumber(name, type.getPrecision(), type.getScale());
      case BIGNUMBER:
        return new ValueMetaBigNumber(name, type.getPrecision(), type.getScale());
      case STRING:
        return new ValueMetaString(name, type.getPrecision(), type.getScale());
      case DATE:
        return new ValueMetaDate(name, -1, -1);
      default:
        return new ValueMetaNone(name);
    }
  }
}
