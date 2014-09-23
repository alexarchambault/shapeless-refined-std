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

package shapeless.refinedstd.ops
package iterator

import language.higherKinds
import scala.reflect.ClassTag
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.refinedstd.util._


trait TakeRight[N <: Nat, A, Sig] extends DepFn1[Iterator[A]] with BuiltOutOf[A] {
  implicit def classTag: ClassTag[A]  
  def n: Int

  def apply(it: Iterator[A]): Out = {
    val b = builder()    
    for (x <- it.takeRight(n))
      b += x    
    b.result()
  }
}

object TakeRight {
  type Aux[N <: Nat, A, Sig, Out0] = TakeRight[N, A, Sig] { type Out = Out0 }

  def apply[N <: Nat, A, Sig](implicit 
    takeRight: TakeRight[N, A, Sig]
  ): Aux[N, A, Sig, takeRight.Out] = takeRight

  implicit def takeRight[N <: Nat, A, Sig] (implicit 
    classTag0: ClassTag[A]
  , builder0: typelevel.Builder[Sig, N, A]
  , n0: ToInt[N]
  ): Aux[N, A, Sig, builder0.Out] =
    new TakeRight[N, A, Sig] {
      type Out = builder0.Out
      def builder() = builder0()
      val classTag = classTag0
      val n = n0()
    }
}
