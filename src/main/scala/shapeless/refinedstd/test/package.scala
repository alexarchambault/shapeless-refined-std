/*
 * Copyright (c) 2014 Alexandre Archambault
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless.refinedstd

package object test {

  def equalInferredTypeAndValue[A, B](a: A, b: B) (implicit 
    ev: A =:= B
  ): Boolean =
    ev(a) == b

  def equalInferredType[A, B](a: => A, b: => B) (implicit ev: A =:= B): Unit = {}

}
