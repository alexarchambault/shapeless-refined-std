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

package shapeless.refinedstd.ops
package iterablelike

import shapeless.refinedstd.util.BuildFrom
import shapeless.{DepFn2, DepFn1}
import shapeless.refinedstd.ops.typelevel.{Prepend, Head}

trait Zip[L] extends DepFn1[L] {
  type Elem
}

object Zip {
  type Aux[L, Elem0, Out0] = Zip[L] { type Elem = Elem0; type Out = Out0 }
  
  def apply[L] (implicit 
    zip: Zip[L]
  ): Aux[L, zip.Elem, zip.Out] = zip
  
  implicit def zip[L, ItL, Repr, Elem0, That] (implicit 
    iterators: Iterators.Aux[L, ItL]
  , iteratorZip: iterator.Zip.Aux[ItL, Elem0]
  , head: Head.Aux[L, Repr]
  , cbf: BuildFrom.Aux[Repr, Elem0, That]
  ): Aux[L, Elem0, That] = 
    new Zip[L] {
      type Elem = Elem0
      type Out = That
      def apply(l: L) = {
        val b = cbf()          
        for (x <- iteratorZip(iterators(l)))
          b += x          
        b.result()
      }
    }
  
}


trait ZipPrepend[H, T] extends DepFn2[H, T] {
  type Elem
}

object ZipPrepend {
  type Aux[H, T, Elem0, Out0] = ZipPrepend[H, T] { type Elem = Elem0; type Out = Out0 }

  def apply[H, T] (implicit 
    zipPrepend: ZipPrepend[H, T]
  ): Aux[H, T, zipPrepend.Elem, zipPrepend.Out] = zipPrepend

  implicit def zipPrepend[H, T, L] (implicit
    prepend: Prepend.Aux[T, H, L]
  , zip: Zip[L]
  ): Aux[H, T, zip.Elem, zip.Out] =
    new ZipPrepend[H, T] {
      type Elem = zip.Elem
      type Out = zip.Out
      def apply(h: H, t: T) = zip(prepend(t, h))
    }
}