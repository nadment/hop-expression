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
 *
 */

package org.apache.hop.expression.xp;

import org.apache.hop.core.exception.HopException;
import org.apache.hop.core.extension.ExtensionPoint;
import org.apache.hop.core.extension.IExtensionPoint;
import org.apache.hop.core.logging.ILogChannel;
import org.apache.hop.core.variables.IVariables;
import org.apache.hop.expression.FunctionRegistry;
import org.apache.hop.expression.UserDefinedFunction;
import org.apache.hop.expression.UserDefinedFunctionMeta;
import org.apache.hop.metadata.api.IHopMetadata;

@ExtensionPoint(id = "UdfCreatedExtensionPoint", extensionPointId = "HopGuiMetadataObjectCreated",
    description = "User Defined Function created")
public class UdfCreatedExtensionPoint implements IExtensionPoint<IHopMetadata> {
  @Override
  public void callExtensionPoint(final ILogChannel log, IVariables variables, IHopMetadata object)
      throws HopException {

    if (object instanceof UserDefinedFunctionMeta) {
      UserDefinedFunctionMeta meta = (UserDefinedFunctionMeta) object;
      log.logBasic("User Defined Function created " + meta.getName());
      FunctionRegistry.register(meta.getName(), new UserDefinedFunction(meta));
    }
  }
}
