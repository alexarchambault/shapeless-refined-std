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
package ops
package traversableonce

import language.higherKinds
import scala.reflect.ClassTag
import shapeless._
import shapeless.ops.nat.ToInt


trait Min[N <: Nat, A, B, Sig] extends DepFn2[TraversableOnce[A], A => B] with BuiltOutOf[A] { self =>
  implicit def classTag: ClassTag[A]
  implicit def ordering: Ordering[B]
  def n: Int
  
  def reverse: Min[N, A, B, Sig] { type Out = self.Out } =
    new Min[N, A, B, Sig] {
      type Out = self.Out
      def classTag = self.classTag
      def ordering = self.ordering.reverse
      def n = self.n
      def builder() = self.builder()
    }

  def apply(coll: TraversableOnce[A], f: A => B): Out = {
    val buffer = Array.ofDim[A](n)
    var filled = false
    var next = 0
    
    def insert(x: A, startIdx: Int): Unit = {
      var idx = startIdx
      buffer(idx) = x
      while (idx > 0 && ordering.compare(f(buffer(idx-1)), f(x)) > 0) {
        buffer(idx) = buffer(idx-1)
        idx -= 1
        buffer(idx) = x
      }
    }
    
    for (x <- coll)
      if (next < n) {
        insert(x, next)
        next += 1
        if (next == n)
          filled = true
      } else if (n > 0 && ordering.compare(f(buffer(n-1)), f(x)) > 0) 
        insert(x, n-1)
    
    val b = builder()
    for (idx <- 0 until next)
      b += buffer(idx)
    b.result()
  }
}

object Min {
  type Aux[N <: Nat, A, B, Sig, Out0] = Min[N, A, B, Sig] { type Out = Out0 }

  def apply[N <: Nat, A, B, Sig] (implicit 
    min: Min[N, A, B, Sig]
  ): Aux[N, A, B, Sig, min.Out] = min

  implicit def min[N <: Nat, A, B, Sig] (implicit
    n0: ToInt[N]
  , classTag0: ClassTag[A]
  , ordering0: Ordering[B]
  , builder0: typelevel.Builder[Sig, N, A]
  ): Aux[N, A, B, Sig, builder0.Out] = 
    new Min[N, A, B, Sig] {
      type Out = builder0.Out
      def builder() = builder0()
      val classTag = classTag0
      val ordering = ordering0
      val n = n0()
    }
}