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

package shapeless.pimpedstd.ops
package traversablelike

import language.higherKinds
import scala.collection.TraversableLike
import shapeless._
import shapeless.ops.nat.ToInt

trait Take[N <: Nat, A, Repr, Sig] extends DepFn2[Repr, Repr => TraversableLike[A, Repr]] with BuiltOutOf[A] {
  def n: Int
  
  def apply(coll: Repr, toCollection: Repr => TraversableLike[A, Repr]): Out = {
    val b = builder()
    for (x <- toCollection(toCollection(coll).take(n)))
      b += x
    b.result()
  }
}

object Take {
  type Aux[N <: Nat, A, Repr, Sig, Out0] = Take[N, A, Repr, Sig] { type Out = Out0 }
  
  def apply[N <: Nat, A, Repr, Sig] (implicit 
    take: Take[N, A, Repr, Sig]
  ): Aux[N, A, Repr, Sig, take.Out] = take
  
  implicit def take[N <: Nat, A, Repr, Sig] (implicit 
    n0: ToInt[N]
  , builder0: typelevel.Builder[Sig, N, A]
  ): Aux[N, A, Repr, Sig, builder0.Out] =
      new Take[N, A, Repr, Sig] {
        type Out = builder0.Out
        def builder() = builder0()
        val n = n0()
      }  
}
