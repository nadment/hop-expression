/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.hop.expression;

import org.apache.hop.core.exception.HopPluginException;
import org.apache.hop.core.plugins.BasePluginType;
import org.apache.hop.core.plugins.PluginAnnotationType;
import org.apache.hop.core.plugins.PluginMainClassType;
import org.apache.hop.core.util.Utils;
import java.lang.reflect.Constructor;
import java.net.URL;
import java.util.List;

/** This class represents the expression function plugin type. */
@PluginMainClassType(Function.class)
@PluginAnnotationType(FunctionPlugin.class)
public class FunctionPluginType extends BasePluginType<FunctionPlugin> {

  private static FunctionPluginType pluginType;

  protected FunctionPluginType() {
    super(FunctionPlugin.class, "FUNCTION", "Function");
  }

  public static FunctionPluginType getInstance() {
    if (pluginType == null) {
      pluginType = new FunctionPluginType();
    }
    return pluginType;
  }

  @Override
  protected String extractID(FunctionPlugin annotation) {
    String id = annotation.id();
    if ( id==null) id=annotation.toString();
    
    return id;
  }

  @Override
  protected String extractName(FunctionPlugin annotation) {
    String name = annotation.id();
    if ( name==null) name=annotation.toString();
    
    return name;
  }

  @Override
  protected String extractDesc(FunctionPlugin annotation) {
    return annotation.description();
  }

  @Override
  protected String extractDocumentationUrl(FunctionPlugin annotation) {
    return annotation.documentationUrl();
  }

  @Override
  public void handlePluginAnnotation(Class<?> clazz, FunctionPlugin annotation,
      List<String> libraries, boolean nativePluginType, URL pluginFolder)
      throws HopPluginException {

    PluginAnnotationType type = this.getClass().getAnnotation(PluginAnnotationType.class);
    
    System.out.println("FX Plugin "+annotation);
    if ( annotation!=null && !Utils.isEmpty(annotation.id())) {
      super.handlePluginAnnotation(clazz, annotation, libraries, nativePluginType, pluginFolder);
      try {
        Constructor<?> constructor = clazz.getDeclaredConstructor();
        Function function = (Function) constructor.newInstance();   
        //FunctionRegistry.register(annotation.id(), function);
      } catch (Exception e) {
        // If class doesn't have constructor, method should be static
      }
      
    
    } 
    
  }

  @Override
  public void searchPlugins() throws HopPluginException {
    System.out.println("Search FX Plugin");
    super.searchPlugins();
  }
}
