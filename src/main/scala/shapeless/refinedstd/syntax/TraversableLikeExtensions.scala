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

import scala.collection.TraversableLike
import shapeless._
import shapeless.refinedstd.ops.traversablelike._
import shapeless.refinedstd.ops.typelevel.SizedOf

case class TraversableLikeTupleExtensions[A, Repr](repr: Repr, toCollection: Repr => TraversableLike[A, Repr]) {

  def takeT(n: Nat) (implicit take: Take[n.N, A, Repr, Product]): take.Out =
    take(repr, toCollection)

  def sliceT(from: Nat, until: Nat) (implicit slice: Slice[from.N, until.N, A, Repr, Product]): slice.Out =
    slice(repr, toCollection)

  def splitAtT(n: Nat) (implicit splitAt: SplitAt[n.N, A, Repr, Product]): splitAt.Out =
    splitAt(repr, toCollection)

  def scanT[B >: A, That](n: Nat, z: B)(op: (B, B) => B)
  (implicit scanLeft: ScanLeft[n.N, A, Repr, B, Product]): scanLeft.Out =
    scanLeftT(n, z)(op)

  def scanLeftT[B, That](n: Nat, z: B)(op: (B, A) => B)
  (implicit scanLeft: ScanLeft[n.N, A, Repr, B, Product]): scanLeft.Out =
    scanLeft(toCollection(repr), z, op)

  def scanRightT[B, That](n: Nat, z: B)(op: (A, B) => B)
  (implicit scanRight: ScanRight[n.N, A, Repr, B, Product]): scanRight.Out =
    scanRight(toCollection(repr), z, op)

}

case class TraversableLikeHListExtensions[A, Repr](repr: Repr, toCollection: Repr => TraversableLike[A, Repr]) {

  def takeH(n: Nat) (implicit take: Take[n.N, A, Repr, HList]): take.Out =
    take(repr, toCollection)

  def sliceH(from: Nat, until: Nat) (implicit slice: Slice[from.N, until.N, A, Repr, HList]): slice.Out =
    slice(repr, toCollection)

  def splitAtH(n: Nat) (implicit splitAt: SplitAt[n.N, A, Repr, HList]): splitAt.Out =
    splitAt(repr, toCollection)

  def scanH[B >: A, That](n: Nat, z: B)(op: (B, B) => B)
  (implicit scanLeft: ScanLeft[n.N, A, Repr, B, HList]): scanLeft.Out =
    scanLeftH(n, z)(op)

  def scanLeftH[B, That](n: Nat, z: B)(op: (B, A) => B)
  (implicit scanLeft: ScanLeft[n.N, A, Repr, B, HList]): scanLeft.Out =
    scanLeft(toCollection(repr), z, op)

  def scanRightH[B, That](n: Nat, z: B)(op: (A, B) => B)
  (implicit scanRight: ScanRight[n.N, A, Repr, B, HList]): scanRight.Out =
    scanRight(toCollection(repr), z, op)

}

case class TraversableLikeSizedExtensions[A, Repr](repr: Repr, toCollection: Repr => TraversableLike[A, Repr]) {

  def takeS(n: Nat) (implicit take: Take[n.N, A, Repr, Sized[Repr, n.N]]): take.Out =
    take(repr, toCollection)

  def sliceS(from: Nat, until: Nat) (implicit slice: Slice[from.N, until.N, A, Repr, SizedOf[Repr]]): slice.Out =
    slice(repr, toCollection)

  def splitAtS(n: Nat) (implicit splitAt: SplitAt[n.N, A, Repr, Sized[Repr, n.N]]): splitAt.Out =
    splitAt(repr, toCollection)

  def scanS[B >: A, That](n: Nat, z: B)(op: (B, B) => B)
  (implicit scanLeft: ScanLeft[n.N, A, Repr, B, Sized[Repr, Succ[n.N]]]): scanLeft.Out =
    scanLeftS(n, z)(op)

  def scanLeftS[B, That](n: Nat, z: B)(op: (B, A) => B)
  (implicit scanLeft: ScanLeft[n.N, A, Repr, B, Sized[Repr, Succ[n.N]]]): scanLeft.Out =
    scanLeft(toCollection(repr), z, op)

  def scanRightS[B, That](n: Nat, z: B)(op: (A, B) => B)
  (implicit scanRight: ScanRight[n.N, A, Repr, B, Sized[Repr, Succ[n.N]]]): scanRight.Out =
    scanRight(toCollection(repr), z, op)

}

case class TraversableLikeExtraExtensions[A, Repr](repr: Repr, toCollection: Repr => TraversableLike[A, Repr]) {

  /* The type of the items of the collection has to be carefully chosen, so that a LabelledGeneric can be found
   * for it. E.g. List[Fruit](Apple(...), Banana(...))
   *                   ^^^^^
   * Default inferred type here (Product with Serializable with Fruit) will make the implicit lookup fail.
   * Usual tricks (auxiliary type B >: A, or with evidence A <:< B) don't seem to work.
   */
  
  def typePartition[C <: Coproduct] (implicit gen: LabelledGeneric.Aux[A, C], partition: PartitionAs[A, Repr, C]) =
    partition(toCollection(repr), gen.to)

  def typePartitionAs[B, C <: Coproduct](f: A => B) 
  (implicit gen: LabelledGeneric.Aux[B, C], partition: PartitionAs[A, Repr, C]) =
    partition(toCollection(repr), a => gen.to(f(a)))

  def typePartitionWith[B, C <: Coproduct](f: A => B) 
  (implicit gen: LabelledGeneric.Aux[B, C], partition: PartitionWith[A, Repr, C]) =
    partition(toCollection(repr), a => gen.to(f(a)))

}