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

package shapeless.pimpedstd.syntax

import shapeless.{Sized, HList, Nat}
import shapeless.pimpedstd.ops.seqlike.{IndicesOfSlice, IndicesOf, Combinations}

import scala.collection.{GenSeq, SeqLike}


case class SeqLikeTupleExtensions[A, Repr](repr: Repr, toCollection: Repr => SeqLike[A, Repr]) {

  def combinationsT(n: Nat) (implicit combinations: Combinations[n.N, A, Repr, Product]): combinations.Out =
    combinations(repr, toCollection)
  
  def combinationsIteratorT(n: Nat) 
  (implicit combinations: Combinations[n.N, A, Repr, Product]): Iterator[combinations.Group] =
    combinations.iterator(repr, toCollection)

  def indicesOfT[B >: A](count: Nat, elem: B) 
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, Product]): indicesOf.Out =
    indicesOf(toCollection(repr), elem, 0)
  
  def indicesOfT[B >: A](count: Nat, elem: B, fromIndex: Int)
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, Product]): indicesOf.Out = 
    indicesOf(toCollection(repr), elem, fromIndex)

  def lastIndicesOfT[B >: A](count: Nat, elem: B)
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, Product]): indicesOf.Out = {
    val c = toCollection(repr)
    indicesOf.last(c, elem, c.length - 1)
  }
  
  def lastIndicesOfT[B >: A](count: Nat, elem: B, fromIndex: Int)
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, Product]): indicesOf.Out =
    indicesOf.last(toCollection(repr), elem, fromIndex)

  
  def indicesOfSliceT[B >: A](count: Nat, slice: GenSeq[B])
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, Product]): indicesOfSlice.Out =
    indicesOfSlice(toCollection(repr), slice, 0)
  
  def indicesOfSliceT[B >: A](count: Nat, slice: GenSeq[B], fromIndex: Int)
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, Product]): indicesOfSlice.Out =
    indicesOfSlice(toCollection(repr), slice, fromIndex)

  def lastIndicesOfSliceT[B >: A](count: Nat, slice: GenSeq[B])
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, Product]): indicesOfSlice.Out = {
    val seqLike = toCollection(repr)
    indicesOfSlice.last(seqLike, slice, seqLike.length - 1)
  }
  
  def lastIndicesOfSliceT[B >: A](count: Nat, slice: GenSeq[B], fromIndex: Int)
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, Product]): indicesOfSlice.Out = 
    indicesOfSlice.last(toCollection(repr), slice, fromIndex)
  
}

case class SeqLikeHListExtensions[A, Repr](repr: Repr, toCollection: Repr => SeqLike[A, Repr]) {

  def combinationsH(n: Nat) (implicit combinations: Combinations[n.N, A, Repr, HList]): combinations.Out =
    combinations(repr, toCollection)

  def combinationsIteratorH(n: Nat)
  (implicit combinations: Combinations[n.N, A, Repr, HList]): Iterator[combinations.Group] =
    combinations.iterator(repr, toCollection)

  def indicesOfH[B >: A](count: Nat, elem: B)
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, HList]): indicesOf.Out =
    indicesOf(toCollection(repr), elem, 0)

  def indicesOfH[B >: A](count: Nat, elem: B, fromIndex: Int)
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, HList]): indicesOf.Out =
    indicesOf(toCollection(repr), elem, fromIndex)

  def lastIndicesOfH[B >: A](count: Nat, elem: B)
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, HList]): indicesOf.Out = {
    val c = toCollection(repr)
    indicesOf.last(c, elem, c.length - 1)
  }

  def lastIndicesOfH[B >: A](count: Nat, elem: B, fromIndex: Int)
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, HList]): indicesOf.Out =
    indicesOf.last(toCollection(repr), elem, fromIndex)


  def indicesOfSliceH[B >: A](count: Nat, slice: GenSeq[B])
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, HList]): indicesOfSlice.Out =
    indicesOfSlice(toCollection(repr), slice, 0)

  def indicesOfSliceH[B >: A](count: Nat, slice: GenSeq[B], fromIndex: Int)
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, HList]): indicesOfSlice.Out =
    indicesOfSlice(toCollection(repr), slice, fromIndex)

  def lastIndicesOfSliceH[B >: A](count: Nat, slice: GenSeq[B])
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, HList]): indicesOfSlice.Out = {
    val seqLike = toCollection(repr)
    indicesOfSlice.last(seqLike, slice, seqLike.length - 1)
  }

  def lastIndicesOfSliceH[B >: A](count: Nat, slice: GenSeq[B], fromIndex: Int)
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, HList]): indicesOfSlice.Out =
    indicesOfSlice.last(toCollection(repr), slice, fromIndex)

}

case class SeqLikeSizedExtensions[A, Repr](repr: Repr, toCollection: Repr => SeqLike[A, Repr]) {

  def combinationsS(n: Nat) (implicit combinations: Combinations[n.N, A, Repr, Sized[Repr, n.N]]): combinations.Out =
    combinations(repr, toCollection)

  def combinationsIteratorS(n: Nat)
  (implicit combinations: Combinations[n.N, A, Repr, Sized[Repr, n.N]]): Iterator[combinations.Group] =
    combinations.iterator(repr, toCollection)

  def indicesOfS[B >: A](count: Nat, elem: B)
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, Sized[Repr, count.N]]): indicesOf.Out =
    indicesOf(toCollection(repr), elem, 0)

  def indicesOfS[B >: A](count: Nat, elem: B, fromIndex: Int)
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, Sized[Repr, count.N]]): indicesOf.Out =
    indicesOf(toCollection(repr), elem, fromIndex)

  def lastIndicesOfS[B >: A](count: Nat, elem: B)
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, Sized[Repr, count.N]]): indicesOf.Out = {
    val c = toCollection(repr)
    indicesOf.last(c, elem, c.length - 1)
  }

  def lastIndicesOfS[B >: A](count: Nat, elem: B, fromIndex: Int)
  (implicit indicesOf: IndicesOf[count.N, A, Repr, B, Sized[Repr, count.N]]): indicesOf.Out =
    indicesOf.last(toCollection(repr), elem, fromIndex)


  def indicesOfSliceS[B >: A](count: Nat, slice: GenSeq[B])
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, Sized[Repr, count.N]]): indicesOfSlice.Out =
    indicesOfSlice(toCollection(repr), slice, 0)

  def indicesOfSliceS[B >: A](count: Nat, slice: GenSeq[B], fromIndex: Int)
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, Sized[Repr, count.N]]): indicesOfSlice.Out =
    indicesOfSlice(toCollection(repr), slice, fromIndex)

  def lastIndicesOfSliceS[B >: A](count: Nat, slice: GenSeq[B])
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, Sized[Repr, count.N]]): indicesOfSlice.Out = {
    val seqLike = toCollection(repr)
    indicesOfSlice.last(seqLike, slice, seqLike.length - 1)
  }

  def lastIndicesOfSliceS[B >: A](count: Nat, slice: GenSeq[B], fromIndex: Int)
  (implicit indicesOfSlice: IndicesOfSlice[count.N, A, Repr, B, Sized[Repr, count.N]]): indicesOfSlice.Out =
    indicesOfSlice.last(toCollection(repr), slice, fromIndex)

}