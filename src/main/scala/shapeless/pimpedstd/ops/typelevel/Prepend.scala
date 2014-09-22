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
package ops.typelevel

import shapeless.Sized._
import shapeless._
import shapeless.ops.hlist.Tupler
import scala.collection.generic.CanBuildFrom
import util.IsTraversableLikeAux

trait Prepend[-L, Elem] {
  type Out
  def apply(l: L, elem: Elem): Out
}

trait PrependHelpers {
  type Aux[L, Elem, Out0] = Prepend[L, Elem] { type Out = Out0 }
  
  def apply[L, Elem] (implicit 
    prepend: Prepend[L, Elem]
  ): Aux[L, Elem, prepend.Out] = prepend
}

trait PrependLowPriorityImplicits extends PrependHelpers {
  
  implicit def tuplePrepend[H, T, L <: HList] (implicit
    gen: Generic.Aux[T, L]
  , tupler: Tupler[H :: L]
  ): Aux[T, H, tupler.Out] =
    new Prepend[T, H] {
      type Out = tupler.Out
      def apply(t: T, elem: H) = tupler(elem :: gen.to(t))
    }
  
}

object Prepend extends PrependLowPriorityImplicits {
  
  implicit def hlistPrepend[H, T <: HList]: Aux[T, H, H :: T] =
    new Prepend[T, H] {
      type Out = H :: T
      def apply(l: T, elem: H) = elem :: l
    }
  
  implicit def sizedPrepend[H, A, UB, Repr, That, N <: Nat] (implicit 
    itl: IsTraversableLikeAux[Repr, A]
  , lub: Lub[H, A, UB] 
  , cbf: CanBuildFrom[Repr, UB, That]
  , ev: AdditiveCollection[That]
  ): Aux[Sized[Repr, N], H, Sized[That, Succ[N]]] = 
    new Prepend[Sized[Repr, N], H] {
      type Out = Sized[That, Succ[N]]
      def apply(s: Sized[Repr, N], elem: H) = {
        val r = itl.conversion(s.unsized)
        val builder = cbf.apply(r.repr)
        builder += lub.left(elem)
        builder ++= r.toIterator.map(lub.right)
        wrap[That, Succ[N]](builder.result())
      }
    }
  
}
