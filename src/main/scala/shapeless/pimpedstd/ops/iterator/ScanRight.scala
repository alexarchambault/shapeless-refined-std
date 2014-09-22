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

package shapeless.pimpedstd.ops
package iterator

import language.higherKinds
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.pimpedstd.util._

import scala.reflect.ClassTag

trait ScanRight[N <: Nat, A, B, Sig] extends DepFn3[Iterator[A], B, (A, B) => B] with BuiltOutOf[B] {
  implicit def classTag: ClassTag[A]  
  def n: Int

  def apply(it: Iterator[A], z: B, op: (A, B) => B): Out = {
    val b = builder()
    for (x <- it.takeRight(n).scanRight(z)(op))
      b += x
    b.result()
  }
}

object ScanRight {
  type Aux[N <: Nat, A, B, Sig, Out0] = ScanRight[N, A, B, Sig] { type Out = Out0 }

  def apply[N <: Nat, A, B, Sig] (implicit 
    scanRight: ScanRight[N, A, B, Sig]
  ): Aux[N, A, B, Sig, scanRight.Out] = scanRight

  implicit def scanRight[N <: Nat, A, B, Sig] (implicit
    builder0: typelevel.Builder[Sig, Succ[N], B] 
  , n0: ToInt[N] 
  , classTag0: ClassTag[A]
  ): Aux[N, A, B, Sig, builder0.Out] =
    new ScanRight[N, A, B, Sig] {
      type Out = builder0.Out
      def builder() = builder0()
      val classTag = classTag0
      val n = n0()
    }
}
