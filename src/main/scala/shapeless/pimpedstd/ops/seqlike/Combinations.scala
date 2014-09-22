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

package shapeless.pimpedstd.ops.seqlike

import shapeless.pimpedstd.ops.typelevel
import shapeless.pimpedstd.util.BuildFrom

import language.higherKinds
import scala.collection.{ mutable, SeqLike }
import shapeless._
import shapeless.pimpedstd.ops.GroupBuiltOutOf
import shapeless.ops.nat.ToInt

trait Combinations[Size <: Nat, A, Repr, Sig] extends DepFn2[Repr, Repr => SeqLike[A, Repr]] with GroupBuiltOutOf[A] {
  def size: Int

  def builder(): mutable.Builder[Group, Out]

  
  def iterator(coll: Repr, toCollection: Repr => SeqLike[A, Repr]): Iterator[Group] = 
    toCollection(coll).combinations(size).map{ s =>
      val b = groupBuilder()
      for (x <- toCollection(s))
        b += x
      b.result()
    }
  
  def apply(coll: Repr, toCollection: Repr => SeqLike[A, Repr]): Out = {
    val b = builder()
    for (x <- iterator(coll, toCollection)) {
      b += x
    }
    b.result()
  }
}

object Combinations {
  type Aux[Size <: Nat, A, Repr, Sig, Group0, Out0] = Combinations[Size, A, Repr, Sig] { type Group = Group0; type Out = Out0 }

  def apply[Size <: Nat, A, Repr, Sig] (implicit 
    combinations: Combinations[Size, A, Repr, Sig]
  ): Aux[Size, A, Repr, Sig, combinations.Group, combinations.Out] = combinations

  implicit def combinations[Size <: Nat, Group0, A, Repr, Sig] (implicit
    builder0: typelevel.Builder.Aux[Sig, Size, A, Group0]
  , size0: ToInt[Size]
  , ev: =:!=[Size, _0]
  , bf: BuildFrom[Repr, Group0]
  ): Aux[Size, A, Repr, Sig, Group0, bf.To] =
    new Combinations[Size, A, Repr, Sig] {
      type Group = Group0
      type Out = bf.To
      def groupBuilder() = builder0().mapResult(_.get)
      def builder() = bf()
      val size = size0()
    }
}
