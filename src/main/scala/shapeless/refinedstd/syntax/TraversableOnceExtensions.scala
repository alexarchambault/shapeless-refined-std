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

import shapeless.{ Nat, HList, Sized }
import shapeless.refinedstd.ops.traversableonce._

case class TraversableOnceTupleExtensions[A](coll: TraversableOnce[A]) {

  def collectFirstT[B](n: Nat)(pf: PartialFunction[A, B])
  (implicit collectFirstN: CollectFirst[n.N, A, B, Product]): collectFirstN.Out =
    collectFirstN(coll, pf)

  def findT(n: Nat)(p: A => Boolean) (implicit findN: Find[n.N, A, Product]): findN.Out =
    findN(coll, p)


  def minT[B >: A](n: Nat) (implicit minN: Min[n.N, A, B, Product]): minN.Out =
    minN(coll, (x: A) => x: B)

  def minTBy[B](n: Nat)(f: A => B) (implicit minN: Min[n.N, A, B, Product]): minN.Out =
    minN(coll, f)

  def maxT[B >: A](n: Nat) (implicit minN: Min[n.N, A, B, Product]): minN.Out =
    minN.reverse(coll, (x: A) => x: B)

  def maxTBy[B](n: Nat)(f: A => B) (implicit minN: Min[n.N, A, B, Product]): minN.Out =
    minN.reverse(coll, f)
  
}

case class TraversableOnceHListExtensions[A](coll: TraversableOnce[A]) {

  def collectFirstH[B](n: Nat)(pf: PartialFunction[A, B])
  (implicit collectFirstN: CollectFirst[n.N, A, B, HList]): collectFirstN.Out =
    collectFirstN(coll, pf)

  def findH(n: Nat)(p: A => Boolean) (implicit findN: Find[n.N, A, HList]): findN.Out =
    findN(coll, p)


  def minH[B >: A](n: Nat) (implicit minN: Min[n.N, A, B, HList]): minN.Out =
    minN(coll, (x: A) => x: B)

  def minHBy[B](n: Nat)(f: A => B) (implicit minN: Min[n.N, A, B, HList]): minN.Out =
    minN(coll, f)

  def maxH[B >: A](n: Nat) (implicit minN: Min[n.N, A, B, HList]): minN.Out =
    minN.reverse(coll, (x: A) => x: B)

  def maxHBy[B](n: Nat)(f: A => B) (implicit minN: Min[n.N, A, B, HList]): minN.Out =
    minN.reverse(coll, f)
  
}

case class TraversableOnceSizedExtensions[A, Repr](coll: TraversableOnce[A]) {

  def collectFirstS[B](n: Nat)(pf: PartialFunction[A, B])
  (implicit collectFirstN: CollectFirst[n.N, A, B, Sized[Repr, n.N]]): collectFirstN.Out =
    collectFirstN(coll, pf)

  def findS(n: Nat)(p: A => Boolean) (implicit findN: Find[n.N, A, Sized[Repr, n.N]]): findN.Out =
    findN(coll, p)


  def minS[B >: A](n: Nat) (implicit minN: Min[n.N, A, B, Sized[Repr, n.N]]): minN.Out =
    minN(coll, (x: A) => x: B)

  def minSBy[B](n: Nat)(f: A => B) (implicit minN: Min[n.N, A, B, Sized[Repr, n.N]]): minN.Out =
    minN(coll, f)

  def maxS[B >: A](n: Nat) (implicit minN: Min[n.N, A, B, Sized[Repr, n.N]]): minN.Out =
    minN.reverse(coll, (x: A) => x: B)

  def maxSBy[B](n: Nat)(f: A => B) (implicit minN: Min[n.N, A, B, Sized[Repr, n.N]]): minN.Out =
    minN.reverse(coll, f)
  
}

case class TraversableOnceExtraExtensions[A](coll: TraversableOnce[A]) {

  def exist(n: Int)(p: A => Boolean): Boolean =
    coll.toIterator.filter(p).take(n).length == n

  def minOption[B >: A : Ordering]: Option[A] = {
    var res = Option.empty[A]

    for (x <- coll)
      if (res.isEmpty || implicitly[Ordering[B]].compare(x, res.get) < 0)
        res = Some(x)

    res
  }

  def minByOption[B: Ordering](f: A => B): Option[A] = {
    var res = Option.empty[(A, B)]

    for (x <- coll) {
      val fx = f(x)
      if (res.isEmpty || implicitly[Ordering[B]].compare(fx, res.get._2) < 0)
        res = Some(x -> fx)
    }

    res.map(_._1)
  }

  def maxOption[B >: A : Ordering]: Option[A] = {
    var res = Option.empty[A]

    for (x <- coll)
      if (res.isEmpty || implicitly[Ordering[B]].compare(x, res.get) > 0)
        res = Some(x)

    res
  }

  def maxByOption[B: Ordering](f: A => B): Option[A] = {
    var res = Option.empty[(A, B)]

    for (x <- coll) {
      val fx = f(x)
      if (res.isEmpty || implicitly[Ordering[B]].compare(fx, res.get._2) > 0)
        res = Some(x -> fx)
    }

    res.map(_._1)
  }

}

