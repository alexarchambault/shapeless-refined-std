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
package traversablelike

import language.higherKinds
import scala.collection.{ TraversableLike, mutable }
import shapeless._
import shapeless.ops.nat.ToInt

trait SplitAt[N <: Nat, A, Repr, Sig] extends DepFn2[Repr, Repr => TraversableLike[A, Repr]] {
  type Group
  def builder(): mutable.Builder[A, Option[Group]]

  def n: Int

  type Out = Option[(Group, Repr)]
  def apply(coll: Repr, toCollection: Repr => TraversableLike[A, Repr]): Out = {
    val (first, second) = toCollection(coll).splitAt(n)
    
    val b = builder()
    for (x <- toCollection(first))
      b += x
    
    b.result().map((_, second))
  }
}

object SplitAt {
  type Aux[N <: Nat, A, Repr, Sig, Group0] = SplitAt[N, A, Repr, Sig] { type Group = Group0 }

  def apply[N <: Nat, A, Repr, Sig] (implicit 
    splitAt: SplitAt[N, A, Repr, Sig]
  ): Aux[N, A, Repr, Sig, splitAt.Group] = splitAt

  implicit def splitAt[N <: Nat, A, Repr, Sig] (implicit 
    n0: ToInt[N]
  , builder0: typelevel.Builder[Sig, N, A]
  ): Aux[N, A, Repr, Sig, builder0.Group] =
    new SplitAt[N, A, Repr, Sig] {
      type Group = builder0.Group
      def builder() = builder0()
      val n = n0()
    }
}
