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

package shapeless.pimpedstd.ops.typelevel

import shapeless._
import shapeless.ops.hlist.IsHCons
import scala.collection.generic.IsTraversableLike
import shapeless.ops.nat.LT

trait Head[L] extends DepFn1[L]

trait HeadDefinitions {
  type Aux[L, Out0] = Head[L] { type Out = Out0 }
  
  def apply[L] (implicit 
    head: Head[L]
  ): Aux[L, head.Out] = head
}

trait HeadLowPriorityImplicits extends HeadDefinitions {
  
  implicit def productHead[P, L <: HList] (implicit 
    gen: Generic.Aux[P, L]
  , isHCons: IsHCons[L]
  ): Aux[P, isHCons.H] =
    new Head[P] {
      type Out = isHCons.H
      def apply(p: P) = isHCons.head(gen.to(p))
    }
  
}

object Head extends HeadLowPriorityImplicits {
  
  implicit def hlistHead[H, T <: HList]: Aux[H :: T, H] =
    new Head[H :: T] {
      type Out = H
      def apply(l: H :: T) = l.head
    }
  
  implicit def sizedHead[Repr, N <: Nat] (implicit 
    ev: AdditiveCollection[Repr]
  , isTraversableLike: IsTraversableLike[Repr]
  , lt: LT[_0, N]
  ): Aux[Sized[Repr, N], isTraversableLike.A] =
    new Head[Sized[Repr, N]] {
      type Out = isTraversableLike.A
      def apply(s: Sized[Repr, N]) = s.head
    }  
  
}
