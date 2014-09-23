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

package shapeless.refinedstd.ops.iterator

import shapeless._
import shapeless.refinedstd.ops.{ typelevel, GroupBuiltOutOf }
import shapeless.ops.nat.ToInt

trait Sliding[Size <: Nat, Step <: Nat, A, Sig] extends DepFn1[Iterator[A]] with GroupBuiltOutOf[A] {
  def size: Int
  def step: Int

  type Out = Iterator[Group]
  
  def apply(it: Iterator[A]): Out = {
    val b = groupBuilder()
    it.sliding(size, step).withPartial(false).map { s =>
      b.clear()
      for (x <- s)
        b += x
      b.result()
    }
  }
}

object Sliding {
  type Aux[Size <: Nat, Step <: Nat, A, Sig, Group0] = Sliding[Size, Step, A, Sig] { type Group = Group0 }

  def apply[Size <: Nat, Step <: Nat, A, Sig] (implicit 
    sliding: Sliding[Size, Step, A, Sig]
  ): Aux[Size, Step, A, Sig, sliding.Group] = sliding

  implicit def sliding[Size <: Nat, Step <: Nat, A, Sig] (implicit
    builder0: typelevel.Builder[Sig, Size, A]
  , size0: ToInt[Size]
  , step0: ToInt[Step]
  , sizeEv: =:!=[Size, _0]
  , stepEv: =:!=[Step, _0]
  ): Aux[Size, Step, A, Sig, builder0.Group] =
    new Sliding[Size, Step, A, Sig] {
      type Group = builder0.Group
      def groupBuilder() = builder0().mapResult(_.get)
      def size = size0()
      def step = step0()
    }
}
