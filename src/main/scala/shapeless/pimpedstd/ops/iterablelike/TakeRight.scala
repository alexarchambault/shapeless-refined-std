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
package iterablelike

import language.higherKinds
import scala.collection.IterableLike
import shapeless._
import shapeless.ops.nat.ToInt

trait TakeRight[N <: Nat, A, Repr, Sig] extends DepFn1[IterableLike[A, Repr]] with BuiltOutOf[A] {
  def n: Int

  def apply(coll: IterableLike[A, Repr]): Out = {
    val b = builder()
    b.sizeHintBounded(n, coll)
    
    val lead = coll.iterator drop n
    var go = false
    for (x <- coll) {
      if (lead.hasNext) lead.next()
      else go = true
      if (go) b += x
    }
    
    b.result()
  }
}

object TakeRight {
  type Aux[N <: Nat, A, Repr, Sig, Out0] = TakeRight[N, A, Repr, Sig] { type Out = Out0 }

  def apply[N <: Nat, A, Repr, Sig] (implicit 
    takeRight: TakeRight[N, A, Repr, Sig]
  ): Aux[N, A, Repr, Sig, takeRight.Out] = takeRight

  implicit def takeRight[N <: Nat, A, Repr, Sig] (implicit
    builder0: typelevel.Builder[Sig, N, A]
  , n0: ToInt[N]
  ): Aux[N, A, Repr, Sig, builder0.Out] =
    new TakeRight[N, A, Repr, Sig] {
      type Out = builder0.Out
      def builder() = builder0()
      val n = n0()
    }
}
