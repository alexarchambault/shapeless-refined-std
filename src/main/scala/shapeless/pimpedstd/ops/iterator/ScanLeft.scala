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
package iterator

import language.higherKinds
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.pimpedstd.util.DepFn3

trait ScanLeft[N <: Nat, A, B, Sig] extends DepFn3[Iterator[A], B, (B, A) => B] with BuiltOutOf[B] {
  def n: Int

  def apply(it: Iterator[A], z: B, op: (B, A) => B): Out = {
    val b = builder()

    var remaining = n
    var acc = z
    b += acc

    while (remaining > 0 && it.hasNext) {
      acc = op(acc, it.next())
      b += acc
      remaining -= 1
    }

    b.result()
  }
}

object ScanLeft {
  type Aux[N <: Nat, A, B, Sig, Out0] = ScanLeft[N, A, B, Sig] { type Out = Out0 }

  def apply[N <: Nat, A, B, Sig](implicit scanLeft: ScanLeft[N, A, B, Sig]): Aux[N, A, B, Sig, scanLeft.Out] = scanLeft

  implicit def scanLeft[N <: Nat, A, B, Sig] (implicit
    builder0: typelevel.Builder[Sig, Succ[N], B]
  , n0: ToInt[N]
  ): Aux[N, A, B, Sig, builder0.Out] =
    new ScanLeft[N, A, B, Sig] {
      type Out = builder0.Out
      def builder() = builder0()
      val n = n0()
    }
}
