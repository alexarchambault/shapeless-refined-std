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
import shapeless._
import shapeless.ops.nat.{ Diff, ToInt }

trait Slice[From <: Nat, Until <: Nat, A, Sig] extends DepFn1[Iterator[A]] with BuiltOutOf[A] {
  def from: Int
  def until: Int

  def apply(it: Iterator[A]): Out = {
    val b = builder()
    for (x <- it.slice(from, until))
      b += x
    b.result()
  }
}

object Slice {
  type Aux[From <: Nat, Until <: Nat, A, Sig, Out0] = Slice[From, Until, A, Sig] { type Out = Out0 }

  def apply[From <: Nat, Until <: Nat, A, Sig] (implicit 
    slice: Slice[From, Until, A, Sig]
  ): Aux[From, Until, A, Sig, slice.Out] = slice

  implicit def slice[From <: Nat, Until <: Nat, Length <: Nat, A, Sig] (implicit
    from0: ToInt[From]
  , until0: ToInt[Until]
  , diff: Diff.Aux[Until, From, Length]
  , builder0: typelevel.Builder[Sig, Length, A]
  ): Aux[From, Until, A, Sig, builder0.Out] =
    new Slice[From, Until, A, Sig] {
      type Out = builder0.Out
      def builder() = builder0()
      val from = from0()
      val until = until0()
    }
}

