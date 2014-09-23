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
package ops
package traversablelike

import language.higherKinds
import scala.collection.generic.CanBuildFrom
import scala.collection.{ mutable, TraversableLike }
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.refinedstd.util.DepFn3

import scala.reflect.ClassTag

trait ScanRight[N <: Nat, A, Repr, B, Sig] extends DepFn3[TraversableLike[A, Repr], B, (A, B) => B] {
  type Group
  def builder(): mutable.Builder[A, Repr]
  def groupBuilder(): mutable.Builder[B, Group]
  
  implicit def classTag: ClassTag[A]

  def n: Int

  
  type Out = Option[(Group, Repr)]

  def apply(coll: TraversableLike[A, Repr], z: B, op: (A, B) => B): Out = {
    val buffer = Array.ofDim[A](n)
    var filled = false
    var idx = 0
    
    val extra = builder()
    
    for (x <- coll) {
      if (filled) {
        extra += buffer(idx)
        buffer(idx) = x
      } else {
        buffer(idx) = x
      }
      
      idx += 1
      if (idx == n) {
        if (!filled)
          filled = true
        idx = 0
      }
    }
    
    if (filled) {
      def helper(acc: B, _idx: Int, looped: Boolean): List[B] =
        if (_idx < 0)
          helper(acc, n - 1, looped = true)
        else if (_idx >= idx || !looped) {
          val _acc = op(buffer(_idx), acc)
          _acc :: helper(_acc, _idx - 1, looped)
        } else
          Nil

      val b = groupBuilder()
      
      for (x <- (z :: helper(z, idx - 1, looped = false)).reverse)
        b += x
      
      Some((b.result(), extra.result()))
    } else
      None
  }
}

object ScanRight {
  type Aux[N <: Nat, A, Repr, B, Sig, Group0] = ScanRight[N, A, Repr, B, Sig] { type Group = Group0 }

  def apply[N <: Nat, A, Repr, B, Sig] (implicit 
    scanRight: ScanRight[N, A, Repr, B, Sig]
  ): Aux[N, A, Repr, B, Sig, scanRight.Group] = scanRight

  implicit def scanRight[N <: Nat, A, Repr, B, Sig] (implicit 
    n0: ToInt[N] 
  , builder0: CanBuildFrom[Repr, A, Repr]
  , classTag0: ClassTag[A]
  , groupBuilder0: typelevel.Builder[Sig, Succ[N], B]
  ): Aux[N, A, Repr, B, Sig, groupBuilder0.Group] =
    new ScanRight[N, A, Repr, B, Sig] {
      type Group = groupBuilder0.Group        
      def groupBuilder() = groupBuilder0().mapResult(_.get)
      def builder() = builder0()
      val classTag = classTag0
      val n = n0()
    }
}
