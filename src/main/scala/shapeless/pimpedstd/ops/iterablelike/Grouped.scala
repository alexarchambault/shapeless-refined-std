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

package shapeless.pimpedstd.ops.iterablelike

import language.higherKinds
import scala.collection.IterableLike
import shapeless._
import shapeless.pimpedstd.ops.{typelevel, BuiltOutOf}
import shapeless.ops.nat.ToInt
import shapeless.pimpedstd.util._

trait Grouped[Size <: Nat, A, Repr, Sig] extends DepFn1[IterableLike[A, Repr]] with BuiltOutOf[Seq[A]] {
  def size: Int

  def apply(coll: IterableLike[A, Repr]): Out = {
    val b = builder()    
    for (x <- coll.iterator.grouped(size).withPartial(false)) 
      b += x    
    b.result()
  }
}

object Grouped {
  type Aux[Size <: Nat, A, Repr, Sig, Out0] = Grouped[Size, A, Repr, Sig] { type Out = Out0 }

  def apply[Size <: Nat, A, Repr, Sig] 
  (implicit grouped: Grouped[Size, A, Repr, Sig]): Aux[Size, A, Repr, Sig, grouped.Out] = grouped

  implicit def grouped[Size <: Nat, Group, A, Repr, Sig] (implicit
    builder0: typelevel.Builder.Aux[Sig, Size, A, Group]
  , size0: ToInt[Size]
  , ev: =:!=[Size, _0]
  , bf: BuildFrom[Repr, Group]
  ): Aux[Size, A, Repr, Sig, bf.To] =
      new Grouped[Size, A, Repr, Sig] {
        type Out = bf.To
        def builder() = bf().on[Seq[A]]{ s =>
          val b = builder0()
          b ++= s
          b.result().get
        }
        val size = size0()
      }
}
