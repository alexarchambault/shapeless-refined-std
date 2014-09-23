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

package shapeless.refinedstd.syntax

import language.higherKinds
import shapeless._
import shapeless.refinedstd.ops.typelevel.SizedOf
import shapeless.refinedstd.ops.iterator._


case class IteratorTupleExtensions[A](it: Iterator[A]) {

  def takeT(n: Nat) (implicit take: Take[n.N, A, Product]): take.Out =
    take(it)

  def sliceT(from: Nat, until: Nat) (implicit slice: Slice[from.N, until.N, A, Product]): slice.Out =
    slice(it)

  def takeRightT(n: Nat) (implicit takeRight: TakeRight[n.N, A, Product]): takeRight.Out =
    takeRight(it)

  def scanLeftT[B](n: Nat, z: B)(op: (B, A) => B) (implicit scanLeft: ScanLeft[n.N, A, B, Product]): scanLeft.Out =
    scanLeft(it, z, op)
  def scanRightT[B](n: Nat, z: B)(op: (A, B) => B) (implicit scanRight: ScanRight[n.N, A, B, Product]): scanRight.Out =
    scanRight(it, z, op)

  def indicesWhereT(n: Nat)(p: A => Boolean) (implicit indicesWhere: IndicesWhere[n.N, A, Product]): indicesWhere.Out = 
    indicesWhere(it, p)

  def indicesOfT[B >: A](n: Nat, elem: B) (implicit indicesOf: IndicesOf[n.N, B, Product]): indicesOf.Out = 
    indicesOf(it, elem, 0)

  def groupedT(n: Nat) (implicit grouped: Grouped[n.N, A, Product]): grouped.Out =
    grouped(it)

  def slidingT(size: Nat, step: Nat) (implicit sliding: Sliding[size.N, step.N, A, Product]): sliding.Out =
    sliding(it)
  def slidingT(size: Nat) (implicit sliding: Sliding[size.N, nat._1, A, Product]): sliding.Out =
    sliding(it)

  def duplicatesT(n: Nat) (implicit duplicates: Duplicates[n.N, A, Product]): duplicates.Out = 
    duplicates(it)
  
}

case class IteratorHListExtensions[A](it: Iterator[A]) {

  def takeH(n: Nat) (implicit take: Take[n.N, A, HList]): take.Out =
    take(it)

  def sliceH(from: Nat, until: Nat) (implicit slice: Slice[from.N, until.N, A, HList]): slice.Out =
    slice(it)

  def takeRightH(n: Nat) (implicit takeRight: TakeRight[n.N, A, HList]): takeRight.Out = 
    takeRight(it)

  def scanLeftH[B](z: B, n: Nat)(op: (B, A) => B) (implicit scanLeft: ScanLeft[n.N, A, B, HList]): scanLeft.Out =
    scanLeft(it, z, op)
  def scanRightH[B](z: B, n: Nat)(op: (A, B) => B) (implicit scanRight: ScanRight[n.N, A, B, HList]): scanRight.Out =
    scanRight(it, z, op)

  def indicesWhereH(n: Nat)(p: A => Boolean) (implicit indicesWhere: IndicesWhere[n.N, A, HList]): indicesWhere.Out = 
    indicesWhere(it, p)

  def indicesOfH[B >: A](n: Nat, elem: B) (implicit indicesOf: IndicesOf[n.N, B, HList]): indicesOf.Out = 
    indicesOf(it, elem, 0)

  def groupedH(n: Nat) (implicit grouped: Grouped[n.N, A, HList]): grouped.Out =
    grouped(it)

  def slidingH(size: Nat, step: Nat) (implicit sliding: Sliding[size.N, step.N, A, HList]): sliding.Out =
    sliding(it)
  def slidingH(size: Nat) (implicit sliding: Sliding[size.N, nat._1, A, HList]): sliding.Out =
    sliding(it)

  def duplicatesH(n: Nat) (implicit duplicateN: Duplicates[n.N, A, HList]): duplicateN.Out = 
    duplicateN(it)

}


case class IteratorSizedExtensions[A](it: Iterator[A]) {

  def takeS[CC[_]](n: Nat) (implicit take: Take[n.N, A, Sized[CC[A], n.N]]): take.Out =
    take(it)

  def sliceS[CC[_]](from: Nat, until: Nat) (implicit slice: Slice[from.N, until.N, A, SizedOf[CC[A]]]): slice.Out =
    slice(it)

  def takeRightS[CC[_]](n: Nat) (implicit takeRight: TakeRight[n.N, A, Sized[CC[A], n.N]]): takeRight.Out =
    takeRight(it)

  case class ScanLeftSizedOf[CC[_]]() {
    def apply[B](z: B, n: Nat)(op: (B, A) => B) (implicit scanLeft: ScanLeft[n.N, A, B, Sized[CC[B], Succ[n.N]]]): scanLeft.Out =
      scanLeft(it, z, op)
  }
  
  def scanLeftS[CC[_]] = ScanLeftSizedOf[CC]()

  case class ScanRightSizedOf[CC[_]]() {
    def apply[B](z: B, n: Nat)(op: (A, B) => B) (implicit scanRight: ScanRight[n.N, A, B, Sized[CC[B], Succ[n.N]]]): scanRight.Out =
      scanRight(it, z, op)
  }

  def scanRightS[CC[_]] = ScanRightSizedOf[CC]()

  def indicesWhereS[CC[_]](n: Nat)(p: A => Boolean) (implicit indicesWhere: IndicesWhere[n.N, A, Sized[CC[Int], n.N]]): indicesWhere.Out =
    indicesWhere(it, p)

  case class IndicesOfSizedOf[CC[_]]() {
    def apply[B >: A](n: Nat, elem: B) (implicit indicesOf: IndicesOf[n.N, B, Sized[CC[A], n.N]]): indicesOf.Out =
      indicesOf(it, elem, 0)
  }
  
  def indicesOfS[CC[_]] = IndicesOfSizedOf[CC]()

  def groupedS[CC[_]](n: Nat) (implicit grouped: Grouped[n.N, A, Sized[CC[A], n.N]]): grouped.Out =
    grouped(it)

  def slidingS[CC[_]](size: Nat, step: Nat) (implicit sliding: Sliding[size.N, step.N, A, Sized[CC[A], size.N]]): sliding.Out =
    sliding(it)
  def slidingS[CC[_]](size: Nat) (implicit sliding: Sliding[size.N, nat._1, A, Sized[CC[A], size.N]]): sliding.Out =
    sliding(it)

  def duplicatesS[CC[_]](n: Nat) (implicit duplicateN: Duplicates[n.N, A, Sized[CC[Iterator[A]], n.N]]): duplicateN.Out =
    duplicateN(it)

}

case class IteratorExtraExtensions[A](it: Iterator[A]) {

  def zipN[L](others: L) (implicit zipPrepend: ZipPrepend[Iterator[A], L]): zipPrepend.Out =
    zipPrepend(it, others)
  
}