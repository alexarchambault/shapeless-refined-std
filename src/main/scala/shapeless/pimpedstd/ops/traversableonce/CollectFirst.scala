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
package traversableonce

import language.higherKinds
import shapeless._
import shapeless.ops.nat.ToInt

trait CollectFirst[N <: Nat, A, B, Sig] extends DepFn2[TraversableOnce[A], PartialFunction[A, B]] with BuiltOutOf[B] {
  def n: Int
  
  def apply(coll: TraversableOnce[A], pf: PartialFunction[A, B]): Out = {
    val l = builder()
    val it = coll.toIterator
    val _n = n
    var count = 0
    
    while (it.hasNext && count < _n) {
      val x = it.next()
      if (pf.isDefinedAt(x)) {
        l += pf(x)
        count += 1
      }
    }
    
    l.result()
  }
}

object CollectFirst {
  type Aux[N <: Nat, A, B, Sig, Out0] = CollectFirst[N, A, B, Sig] { type Out = Out0 }

  def apply[N <: Nat, A, B, Sig] (implicit 
    collectFirst: CollectFirst[N, A, B, Sig]
  ): Aux[N, A, B, Sig, collectFirst.Out] = collectFirst

  implicit def collectFirst[N <: Nat, A, B, Sig] (implicit 
    n0: ToInt[N]
  , builder0: typelevel.Builder[Sig, N, B]
  ): Aux[N, A, B, Sig, builder0.Out] =
      new CollectFirst[N, A, B, Sig] {
        type Out = builder0.Out
        def builder() = builder0()
        val n = n0()
      }
}
