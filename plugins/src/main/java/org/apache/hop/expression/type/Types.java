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

package org.apache.hop.expression.type;

import org.apache.hop.expression.IExpression;

public class Types {

   private Types() {
    // Utility class
  }

   
   /**
    * Determines common type for a comparison operator whose operands are STRING
    * type and the other (non STRING) type.
    */
   Type commonTypeForBinaryComparison(Type type1, Type type2) {
     return null;
   }
   
   /**
    * Coerces CASE WHEN statement branches to one common type.
    *
    * <p>Rules: Find common type for all the then operands and else operands,
    * then try to coerce the then/else operands to the type if needed.
    */
//   boolean caseWhenCoercion(SqlCallBinding binding);


   /**
    * Returns whether a IExpression should be cast to a target type.
    */
   protected boolean needToCast(IExpression expression, Type toType) {
     return false;
   }
   

   /**
    * Returns whether the conversion from {@code source} to {@code target} type
    * is a 'loss-less' cast, that is, a cast from which
    * the original value of the field can be certainly recovered.
    *
    * @param source source type
    * @param target target type
    * @return true if the conversion is a loss-less cast
    */   
   public static boolean isLosslessCast(Type source, Type target) {
     
//     // Both BOOLEAN type
//     if (source.is(TypeId.BOOLEAN) && target.is(TypeId.BOOLEAN)) {
//       return true;
//     }
//     if (source.is(TypeId.DATE) && target.is(TypeId.DATE)) {
//       return true;
//     }
//     if (source.is(TypeId.JSON) && target.is(TypeId.JSON)) {
//       return true;
//     }
//     
//     // Both INTEGER type
//     if (source.is(TypeId.INTEGER) && target.is(TypeId.INTEGER)) {
//       return source.getPrecision() <= target.getPrecision();
//     }
//     
//     // Both STRING type: it depends on the precision (length)
//     if (source.is(TypeId.STRING) && target.is(TypeId.STRING)) {
//       return source.getPrecision() <= target.getPrecision();
//     }
//
//     // Both BINARY type: it depends on the precision (length)
//     if (source.is(TypeId.BINARY) && target.is(TypeId.BINARY)) {
//       return source.getPrecision() <= target.getPrecision();
//     }
//     
//     // Both NUMBER type: it depends on the precision/scale
//     if (source.is(TypeId.NUMBER) && target.is(TypeId.NUMBER)) {
//       return source.getPrecision() <= target.getPrecision() && source.getScale() <= target.getScale();
//     }
          
     if (source.is(target.getId())) {
       if (source.getPrecision() <= target.getPrecision()
           && source.getScale() <= target.getScale()) {
         return true;
       }
     }
     
     // Return FALSE by default
     return false;
   }
   
   /**
    * Returns whether two types are comparable. They need to be scalar types of the same family
    *
    * @param type1 First type
    * @param type2 Second type
    * @return Whether types are comparable
    */
   public static boolean isComparable(Type type1, Type type2) {

     final TypeFamily family1 = type1.getFamily();
     final TypeFamily family2 = type2.getFamily();
     if (family1 == family2) {
       return true;
     }

     // If one of the arguments is of type 'ANY', return true.
     if (family1 == TypeFamily.ANY
         || family2 == TypeFamily.ANY) {
       return true;
     }

     // If one of the arguments is of type 'NULL', return true.
//     if (family1 == TypeFamily.NULL
//         || family2 == TypeFamily.NULL) {
//       return true;
//     }

     
     return false;
   }
   
   public static boolean isString(Type type) {
     if ( type==null )return false;
     return type.is(TypeId.STRING);
   }

   
   /** Return the default {@link Type} that belongs to this {@link TypeId}. */
//   public Type getDefaultType(TypeId id) {
//     switch (id) {
//       case BOOLEAN:
//         return BooleanType.BOOLEAN;
//       case BINARY:
//         return BinaryType.BINARY;
//       case STRING:
//         return StringType.STRING;
//       case TEMPORAL:
//         return DateType.DATE;
//       case INTERVAL:
//         return IntervalType.INTERVAL;
//       case NUMERIC:
//         return NumberType.NUMBER;
//       case JSON:
//         return JsonType.JSON;
//       default:
//         return null;
//     }
//   }
}
