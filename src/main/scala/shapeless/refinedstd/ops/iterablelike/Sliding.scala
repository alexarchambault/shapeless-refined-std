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

package shapeless.refinedstd.ops.iterablelike

import language.higherKinds
import collection.{ mutable, IterableLike }
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.refinedstd.ops.{typelevel, GroupBuiltOutOf}
import shapeless.refinedstd.util.BuildFrom

trait Sliding[Size <: Nat, Step <: Nat, A, Repr, Sig] extends DepFn1[IterableLike[A, Repr]] with GroupBuiltOutOf[A] {
  def builder(): mutable.Builder[Group, Out]

  def size: Int
  def step: Int

  def apply(coll: IterableLike[A, Repr]): Out = {
    val b = builder()
    for (s <- coll.iterator.sliding(size, step).withPartial(false)) {
      val _b = groupBuilder()
      for (x <- s)
        _b += x 
      b += _b.result()
    }
    b.result()
  }
}

object Sliding {
  type Aux[Size <: Nat, Step <: Nat, A, Repr, Sig, Group0, Out0] =
    Sliding[Size, Step, A, Repr, Sig] { type Group = Group0; type Out = Out0 }

  def apply[Size <: Nat, Step <: Nat, A, Repr, Sig] (implicit
    sliding: Sliding[Size, Step, A, Repr, Sig]
  ): Aux[Size, Step, A, Repr, Sig, sliding.Group, sliding.Out] = sliding

  // BuildFrom (instead of CanBuildFrom) makes scalac infer better types for Out
  implicit def sliding[Size <: Nat, Step <: Nat, Group0, A, Repr, Sig] (implicit
    builder0: typelevel.Builder.Aux[Sig, Size, A, Group0]
  , size0: ToInt[Size]
  , step0: ToInt[Step]
  , sizeEv: =:!=[Size, _0]
  , stepEv: =:!=[Step, _0]
  , bf: BuildFrom[Repr, Group0]
  ): Aux[Size, Step, A, Repr, Sig, Group0, bf.To] =
      new Sliding[Size, Step, A, Repr, Sig] {
        type Group = Group0
        type Out = bf.To
        def groupBuilder() = builder0().mapResult(_.get)
        def builder() = bf()
        val size = size0()
        val step = step0()
      }
}
