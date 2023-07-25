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
package org.apache.hop.expression.operator;

import org.apache.hop.expression.Call;
import org.apache.hop.expression.ExpressionException;
import org.apache.hop.expression.Function;
import org.apache.hop.expression.FunctionPlugin;
import org.apache.hop.expression.IExpression;
import org.apache.hop.expression.IExpressionContext;
import org.apache.hop.expression.OperatorCategory;
import org.apache.hop.expression.type.BinaryType;
import org.apache.hop.expression.type.OperandTypes;
import org.apache.hop.expression.type.ReturnTypes;
import org.apache.hop.expression.type.StringType;
import org.apache.hop.expression.type.Type;

/**
 * The function reverses the order of characters in a string value, or of bytes in a binary value.
 */
@FunctionPlugin
public class ReverseFunction extends Function {

  private final static Function REVERSE_BINARY = new ReverseBinaryFunction();
  private final static Function REVERSE_STRING = new ReverseStringFunction();
  
  public ReverseFunction() {
    super("REVERSE", ReturnTypes.ARG0, OperandTypes.STRING.or(OperandTypes.BINARY),
        OperatorCategory.STRING, "/docs/reverse.html");
  }
 
  @Override
  public IExpression compile(IExpressionContext context, Call call) throws ExpressionException {

    Type type = call.getOperand(0).getType();

    if (type.isSameFamily(StringType.STRING)) {      
      return new Call(REVERSE_STRING, call.getOperand(0));
    }
    
    if (type.isSameFamily(BinaryType.BINARY)) {      
      return new Call(REVERSE_BINARY, call.getOperand(0));
    }
    
    return call;
  }
}
