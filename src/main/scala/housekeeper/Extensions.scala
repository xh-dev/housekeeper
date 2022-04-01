package housekeeper

import scala.util.{Failure, Success, Try}


object Extensions {
  extension[A] (a: A) {
    def nothingHappen(operation: A => Unit): A = {
      operation(a)
      a
    }

    def asTry(): Try[A] = Success(a)

    def asTryWithMap[B](operation: A => B): Try[B] = asTry().map(operation)
  }

  extension[V] (option: Option[V]) {
    def ifUnexpected(msg: String): Try[V] = {
      option match {
        case Some(v) => Success(v)
        case None => Failure(new RuntimeException(msg))
      }
    }

    def allAsExpected[A](indexedSeq: Seq[Try[A]], msg: String): Try[Seq[A]] = {
      if (indexedSeq.exists(_.isFailure)) Failure(new RuntimeException(msg)) else Success(indexedSeq.map(_.get))
    }

    def tryTo[A](operation: V => A): Try[A] = ifUnexpected("Passing empty to operation").map(v => operation(v))
  }

  extension[V, A <: Seq[Try[V]]] (tries: A) {
    def testAllSuccess(msg: String | Exception): Try[Seq[V]] = if (tries.exists(_.isFailure)) Failure(msg match {
      case msg: String => new RuntimeException(msg)
      case ex: Exception => ex
    }) else Success(tries.map(_.get))
  }

  extension[V] (t: Try[V]) {
    def throwIfFail(): Try[V] = {
      if (t.isFailure) {
        throw t.failed.get
      } else {
        t
      }
    }

    def doSideEffect(op:V=>Unit): Try[V] ={
      if(t.isFailure){
        t
      } else {
        t.map{item=>
          op(item)
          item
        }
      }
    }
  }
}
