package u04lab
import u03.Sequences.*, u03.Optionals.*
import Sequence.*, Optional.*

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    def logAll[A](t: T[A])(f: A => Unit ): Unit //T[A]

  object TraversableOptional extends Traversable[Optional]:
    def logAll[A](t: Optional[A])(f: A => Unit): Unit = t match
      case Just(a) =>  f(a)
      case _ => println("Empty")

  object TraversableSequence extends Traversable[Sequence]:
    def logAll[A](t: Sequence[A])(f: A => Unit): Unit = t match
      case Cons(h,t) => f(h);logAll(t)(f)
      case _ => ;//println("Nill")

  def log[A](a: A): Unit = println("The next element is: "+a)

 /* def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()
*/
@main def tryTraversable =
  import Ex5Traversable.*
  
  TraversableOptional.logAll(Just(5))(log)
  TraversableSequence.logAll(Cons(10,Cons(20,Cons(30,Nil()))))(log)
