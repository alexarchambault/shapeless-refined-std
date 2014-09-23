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

import scala.collection.IterableLike
import shapeless._
import shapeless.refinedstd.ops.iterablelike._

case class IterableLikeTupleExtensions[A, Repr](repr: Repr, toCollection: Repr => IterableLike[A, Repr]) {

  def groupedT(n: Nat) (implicit grouped: Grouped[n.N, A, Repr, Product]): grouped.Out =
    grouped(toCollection(repr))

  def slidingT(size: Nat, step: Nat) (implicit sliding: Sliding[size.N, step.N, A, Repr, Product]): sliding.Out =
    sliding(toCollection(repr))

  def slidingT(size: Nat) (implicit sliding: Sliding[size.N, nat._1, A, Repr, Product]): sliding.Out =
    sliding(toCollection(repr))

  def takeRightT(n: Nat) (implicit takeRight: TakeRight[n.N, A, Repr, Product]): takeRight.Out =
    takeRight(toCollection(repr))

}

case class IterableLikeHListExtensions[A, Repr](repr: Repr, toCollection: Repr => IterableLike[A, Repr]) {

  def groupedH(n: Nat) (implicit grouped: Grouped[n.N, A, Repr, HList]): grouped.Out =
    grouped(toCollection(repr))

  def slidingH(size: Nat, step: Nat) (implicit sliding: Sliding[size.N, step.N, A, Repr, HList]): sliding.Out =
    sliding(toCollection(repr))

  def slidingH(size: Nat) (implicit sliding: Sliding[size.N, nat._1, A, Repr, HList]): sliding.Out =
    sliding(toCollection(repr))

  def takeRightH(n: Nat) (implicit takeRight: TakeRight[n.N, A, Repr, HList]): takeRight.Out =
    takeRight(toCollection(repr))

}

case class IterableLikeSizedExtensions[A, Repr](repr: Repr, toCollection: Repr => IterableLike[A, Repr]) {

  def groupedS(n: Nat) (implicit grouped: Grouped[n.N, A, Repr, Sized[Repr, n.N]]): grouped.Out =
    grouped(toCollection(repr))

  def slidingS(size: Nat, step: Nat) (implicit sliding: Sliding[size.N, step.N, A, Repr, Sized[Repr, size.N]]): sliding.Out =
    sliding(toCollection(repr))

  def slidingS(size: Nat) (implicit sliding: Sliding[size.N, nat._1, A, Repr, Sized[Repr, size.N]]): sliding.Out =
    sliding(toCollection(repr))

  def takeRightS(n: Nat) (implicit takeRight: TakeRight[n.N, A, Repr, Sized[Repr, n.N]]): takeRight.Out =
    takeRight(toCollection(repr))

}

case class IterableLikeExtraExtensions[A, Repr](repr: Repr, toCollection: Repr => IterableLike[A, Repr]) {

  def zipN[L](others: L) (implicit zipPrepend: ZipPrepend[Repr, L]): zipPrepend.Out =
    zipPrepend(repr, others)

}
