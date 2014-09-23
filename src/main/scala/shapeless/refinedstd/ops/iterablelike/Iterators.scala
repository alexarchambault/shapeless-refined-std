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

package shapeless.refinedstd
package ops.iterablelike

import scala.collection.generic.CanBuildFrom
import shapeless._
import shapeless.ops.hlist.Tupler
import util.IsTraversableLikeAux

trait Iterators[L] extends DepFn1[L]

trait IteratorsDefinitions {
  type Aux[L, Out0] = Iterators[L] { type Out = Out0 }
  
  def apply[L] (implicit
    iterators: Iterators[L]
  ): Aux[L, iterators.Out] = iterators
  
  
  trait IteratorsHList[L <: HList] extends Iterators[L] { type Out <: HList }
  
  object IteratorsHList {
    type Aux[L <: HList, Out0 <: HList] = IteratorsHList[L] { type Out = Out0 }
    
    def apply[L <: HList] (implicit
      iteratorsHList: IteratorsHList[L]
    ): Aux[L, iteratorsHList.Out] = iteratorsHList
  }

}

trait IteratorsLowPriorityImplicits extends IteratorsDefinitions {
  
  implicit def productIterators[P, L <: HList, ItL <: HList] (implicit 
    gen: Generic.Aux[P, L]
  , hlistIterators: IteratorsHList.Aux[L, ItL]
  , tupler: Tupler[ItL]
  ): Aux[P, tupler.Out] =
      new Iterators[P] {
        type Out = tupler.Out
        def apply(p: P) = tupler(hlistIterators(gen.to(p)))
      }
  
}

object Iterators extends IteratorsLowPriorityImplicits {

  implicit def hnilIterators[L <: HNil]: IteratorsHList.Aux[L, HNil] =
    new IteratorsHList[L] {
      type Out = HNil
      def apply(l: L) = HNil
    }

  implicit def hconsIterators[H, T <: HList] (implicit 
    isIterableLike: IsIterableLike[H]
  , tailIterators: IteratorsHList[T]
  ): IteratorsHList.Aux[H :: T, Iterator[isIterableLike.A] :: tailIterators.Out] =
      new IteratorsHList[H :: T] {
        type Out = Iterator[isIterableLike.A] :: tailIterators.Out
        def apply(l: H :: T) = isIterableLike.conversion(l.head).iterator :: tailIterators(l.tail)
      }
 
  
  implicit def sizedIterators[SizedRepr, Repr, N <: Nat, A, That] (implicit
    ev: AdditiveCollection[SizedRepr]
  , isTraversableLike: IsTraversableLikeAux[SizedRepr, Repr]
  , isIterableLike: IsIterableLike.Aux[Repr, A]
  , cbf: CanBuildFrom[SizedRepr, Iterator[A], That]
  , tev: AdditiveCollection[That]
  ): Aux[Sized[SizedRepr, N], Sized[That, N]] =
      new Iterators[Sized[SizedRepr, N]] {
        type Out = Sized[That, N]
        def apply(s: Sized[SizedRepr, N]) = s.map(r => isIterableLike.conversion(r).iterator)
      }
  
}
