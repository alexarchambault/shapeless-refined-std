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

package shapeless.pimpedstd.ops.iterator

import shapeless._
import shapeless.pimpedstd.ops.{typelevel, GroupBuiltOutOf}
import shapeless.ops.nat.ToInt

trait Grouped[Size <: Nat, A, Sig] extends DepFn1[Iterator[A]] with GroupBuiltOutOf[A] {
  def size: Int

  type Out = Iterator[Group]
  
  def apply(it: Iterator[A]): Out = {
    val b = groupBuilder()
    
    it.grouped(size).withPartial(false).map{ s =>
      b.clear()
      for (x <- s)
        b += x
      b.result()
    }
  }
}

object Grouped {
  type Aux[Size <: Nat, A, Sig, Group0] = Grouped[Size, A, Sig] { type Group = Group0 }

  def apply[Size <: Nat, A, Sig] (implicit
    grouped: Grouped[Size, A, Sig]
  ): Aux[Size, A, Sig, grouped.Group] = grouped
  
  implicit def grouped[Size <: Nat, A, Sig] (implicit 
    groupBuilder0: typelevel.Builder[Sig, Size, A]
  , size0: ToInt[Size]
  , ev: =:!=[Size, _0]
  ): Aux[Size, A, Sig, groupBuilder0.Group] =
    new Grouped[Size, A, Sig] {
      type Group = groupBuilder0.Group
      def groupBuilder() = groupBuilder0().mapResult(_.get)
      val size = size0()
    }  
}
