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
import scala.collection.generic.CanBuildFrom
import scala.collection.{ mutable, TraversableLike }
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.pimpedstd.util.DepFn3

trait ScanLeft[N <: Nat, A, Repr, B, Sig] extends DepFn3[TraversableLike[A, Repr], B, (B, A) => B] {
  type Group
  def builder(): mutable.Builder[A, Repr]
  def groupBuilder(): mutable.Builder[B, Option[Group]]

  def n: Int
  
  type Out = Option[(Group, Repr)]

  def apply(coll: TraversableLike[A, Repr], z: B, op: (B, A) => B): Out = {
    val b = groupBuilder()
    val extra = builder()
    
    var remaining = n

    var acc = z
    b += acc

    for (x <- coll)
      if (remaining > 0) {
        acc = op(acc, x)
        b += acc
        remaining -= 1
      } else
        extra += x

    b.result().map((_, extra.result()))
  }
}

object ScanLeft {
  type Aux[N <: Nat, A, Repr, B, Sig, Group0] = ScanLeft[N, A, Repr, B, Sig] { type Group = Group0 }

  def apply[N <: Nat, A, Repr, B, Sig] (implicit 
    scanLeft: ScanLeft[N, A, Repr, B, Sig]
  ): Aux[N, A, Repr, B, Sig, scanLeft.Group] = scanLeft

  implicit def scanLeft[N <: Nat, A, Repr, B, Sig] (implicit 
    n0: ToInt[N]
  , builder0: CanBuildFrom[Repr, A, Repr]
  , groupBuilder0: typelevel.Builder[Sig, Succ[N], B]
  ): Aux[N, A, Repr, B, Sig, groupBuilder0.Group] =
    new ScanLeft[N, A, Repr, B, Sig] {
      type Group = groupBuilder0.Group
      def builder() = builder0()
      def groupBuilder() = groupBuilder0()
      val n = n0()
    }
}
