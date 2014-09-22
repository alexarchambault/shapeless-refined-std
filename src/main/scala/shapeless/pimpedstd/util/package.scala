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

import language.implicitConversions
import language.higherKinds
import shapeless._

import scala.collection.mutable
import scala.reflect.ClassTag

package object util {
  
  type IsTraversableLikeAux[Repr, A0] = scala.collection.generic.IsTraversableLike[Repr] { type A = A0 }
  type IsSeqLikeAux[Repr, A0] = scala.collection.generic.IsSeqLike[Repr] { type A = A0 }

  trait HListRepeat[N <: Nat] {
    type Out[A] <: HList
  }
  
  object HListRepeat {
    type Aux[N <: Nat, Out0[A] <: HList] = HListRepeat[N] { type Out[A] = Out0[A] }
    
    def apply[N <: Nat] (implicit 
      repeat: HListRepeat[N]
    ): Aux[N, repeat.Out] = repeat
    
    def apply(n: Nat) (implicit
      repeat: HListRepeat[n.N]
    ): Aux[n.N, repeat.Out] = repeat
    
    implicit val repeatZero: Aux[_0, Const[HNil]#λ] =
      new HListRepeat[_0] {
        type Out[A] = HNil
      }
    
    implicit def repeatSucc[N <: Nat] (implicit 
      current: HListRepeat[N]
    ): Aux[Succ[N], ({ type Out[A] = A :: current.Out[A] })#Out] =
      new HListRepeat[Succ[N]] {
        type Out[A] = A :: current.Out[A]
      }
  }
  
  trait TupleRepeat[N <: Nat] {
    type Out[A]
  }
  
  trait TupleRepeatDefinitions {
    type Aux[N <: Nat, Out0[A]] = TupleRepeat[N] { type Out[A] = Out0[A] }

    def apply[N <: Nat] (implicit 
      repeat: TupleRepeat[N]
    ): Aux[N, repeat.Out] = repeat
    
    def apply(n: Nat) (implicit 
      repeat: TupleRepeat[n.N]
    ): Aux[n.N, repeat.Out] = repeat
  }
  
  object TupleRepeat extends TupleRepeatBoilerplate {
    implicit val repeatZero: Aux[_0, Const[Unit]#λ] =
      new TupleRepeat[_0] {
        type Out[A] = Unit
      }
  }
  
  implicit class IteratorExtensions[A](val it: Iterator[A]) extends AnyVal {
    def takeRight(n: Int) (implicit 
      classTag: ClassTag[A]
    ): Iterator[A] = {
      val a = Array.ofDim[A](n)
      var filled = false
      var idx = 0
      
      while (it.hasNext) {
        a(idx) = it.next()
        
        idx += 1
        if (idx == n) {
          if (!filled) 
            filled = true
          idx = 0
        }
      }
      
      if (filled)
        a.iterator.drop(idx) ++ a.iterator.take(idx)
      else
        a.iterator.take(idx)
    }
  }

  
  implicit class BuilderOn[Elem, To](val self: mutable.Builder[Elem, To]) extends AnyVal {
    def on[NewElem](f: NewElem => Elem): mutable.Builder[NewElem, To] =
      new mutable.Builder[NewElem, To] {
        def +=(elem: NewElem): this.type = {
          self += f(elem)
          this
        }
        def result(): To = self.result()
        def clear(): Unit = self.clear()
      }
  }


  trait DepFn3[T, U, V] {
    type Out
    def apply(t: T, u: U, v: V): Out
  }

}
