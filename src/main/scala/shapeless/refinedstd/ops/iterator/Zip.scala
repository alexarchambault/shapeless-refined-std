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

import language.higherKinds
import shapeless._
import shapeless.refinedstd.ops.typelevel.Prepend
import shapeless.ops.hlist.Tupler

trait Zip[L] extends DepFn1[L] {
  type Elem
  type Out = Iterator[Elem]
}

trait ZipHelpers {  
  type Aux[L, Elem0] = Zip[L] { type Elem = Elem0 }
  
  def apply[L](implicit 
    zip: Zip[L]
  ): Aux[L, zip.Elem] = zip

  
  trait ZipHList[L <: HList] extends Zip[L] { type Elem <: HList }

  object ZipHList {
    type Aux[L <: HList, Elem0 <: HList] = ZipHList[L] { type Elem = Elem0 }
    
    def apply[L <: HList] (implicit 
      zipHList: ZipHList[L]
    ): Aux[L, zipHList.Elem] = zipHList
  }
  
  
  trait ZipProduct[L] extends Zip[L]

  object ZipProduct {
    type Aux[L, Elem0] = ZipProduct[L] { type Elem = Elem0 }
    
    def apply[L](implicit 
      zipProduct: ZipProduct[L]
    ): Aux[L, zipProduct.Elem] = zipProduct
  }  
}

trait ZipLowPriorityImplicits extends ZipHelpers {

  implicit def zipProduct[P, L <: HList, Elem <: HList] (implicit
    gen: Generic.Aux[P, L]
  , zipHList: ZipHList.Aux[L, Elem]
  , tupler: Tupler[Elem]
  ): ZipProduct.Aux[P, tupler.Out] =
    new ZipProduct[P] {
      type Elem = tupler.Out
      def apply(p: P) = zipHList(gen.to(p)).map(tupler(_))
    }
  
}

object Zip extends ZipLowPriorityImplicits {
  
  implicit def zipHListSingle[A, L <: HNil]: ZipHList.Aux[Iterator[A] :: L, A :: HNil] =
    new ZipHList[Iterator[A] :: L] {
      type Elem = A :: HNil
      def apply(l: Iterator[A] :: L) = l.head.map(_ :: HNil)
    }
  
  implicit def zipHListSucc[ElemH, T <: HList] (implicit 
    zipTail: ZipHList[T]
  ): ZipHList.Aux[Iterator[ElemH] :: T, ElemH :: zipTail.Elem] =
    new ZipHList[Iterator[ElemH] :: T] {
      type Elem = ElemH :: zipTail.Elem
      def apply(l: Iterator[ElemH] :: T) = l.head.zip(zipTail(l.tail)).map{case (h, t) => h :: t}
    }
  
  
  implicit def zipSized[CC[_], A, N <: Nat, L <: HList, ElemHL <: HList] (implicit 
    toHList: shapeless.ops.sized.ToHList.Aux[CC[Iterator[A]], N, L]
  , zipHList: ZipHList.Aux[L, ElemHL]
  , hlToSized: shapeless.ops.hlist.ToSized[ElemHL, CC]
  ): Aux[Sized[CC[Iterator[A]], N], hlToSized.Out] =
    new Zip[Sized[CC[Iterator[A]], N]] {
      type Elem = hlToSized.Out
      def apply(l: Sized[CC[Iterator[A]], N]) = zipHList(toHList(l)).map(hlToSized(_))
    } 
  
}


trait ZipPrepend[H, T] extends DepFn2[H, T] {
  type Elem
  type Out = Iterator[Elem]
}

object ZipPrepend {
  type Aux[H, T, Elem0] = ZipPrepend[H, T] { type Elem = Elem0 }
  
  def apply[H, T] (implicit 
    zipPrepend: ZipPrepend[H, T]
  ): Aux[H, T, zipPrepend.Elem] = zipPrepend
  
  implicit def zipPrepend[H, T, L] (implicit 
    prepend: Prepend.Aux[T, H, L]
  , zip: Zip[L]
  ): Aux[H, T, zip.Elem] = 
    new ZipPrepend[H, T] {
      type Elem = zip.Elem
      def apply(h: H, t: T) = zip(prepend(t, h))
    }
}