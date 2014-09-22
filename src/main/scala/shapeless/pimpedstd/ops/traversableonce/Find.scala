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

package shapeless.pimpedstd
package ops
package traversableonce

import language.higherKinds
import shapeless._
import shapeless.ops.nat.ToInt

trait Find[N <: Nat, A, Sig] extends DepFn2[TraversableOnce[A], A => Boolean] with BuiltOutOf[A] {
  def n: Int

  def apply(coll: TraversableOnce[A], p: A => Boolean): Out = {
    val b = builder()
    for (x <- coll.toIterator.filter(p).take(n))
      b += x
    b.result()
  }
}

object Find {
  type Aux[N <: Nat, A, Sig, Out0] = Find[N, A, Sig] { type Out = Out0 }

  def apply[N <: Nat, A, Sig] (implicit 
    find: Find[N, A, Sig]
  ): Aux[N, A, Sig, find.Out] = find

  implicit def find[N <: Nat, A, Sig] (implicit 
    n0: ToInt[N]
  , builder0: typelevel.Builder[Sig, N, A]
  ): Aux[N, A, Sig, builder0.Out] =
      new Find[N, A, Sig] {
        type Out = builder0.Out
        def builder() = builder0()
        val n = n0()
      }
}
