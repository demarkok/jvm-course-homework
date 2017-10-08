package ru.spbau.kaysin.set

import scala.collection.GenTraversableOnce

/**
  * A trait representing multiset - the set which can contain one or more instances of one value.
  * @tparam T type of contained values.
  */
sealed trait Multiset[+T] {
  /**
    * The multiset size. All repeated instances are included.
    */
  val size: Int

  /**
    * A filter function which delete from the multiset all the elements except satisfying the condition.
    * If value doesn't satisfy the condition -- the method remove all it's instances.
    * @param predicate The condition.
    * @return Resulting Multiset.
    */
  def filter(predicate: T => Boolean): Multiset[T] // can't use Predicate due to it's invariance

  /**
    * A map function which transforms each element of the multiset into the result of applying the function to it.
    * @param function The transforming function.
    * @tparam That Resulting type.
    * @return Transformed multiset.
    */
  def map[That](function: T => That): Multiset[That]

  /**
    * A flatMap function which transforms each element of the multiset into some container and then join all these
    * containers into one multiset.
    * @param function The transforming function.
    * @tparam That The type of the resulting multiset.
    * @return Transformed multiset.
    */
  def flatMap[That](function: T => GenTraversableOnce[That]): Multiset[That]

  /**
    * A method which is looking for given element in the set.
    * @param elem The element which it's looking for.
    * @tparam A The method will cast comparing elements to this type.
    * @return None if the element isn't present in the multiset and the element wrapped into Some otherwise.
    */
  def find[A >: T](elem: A): Option[T]

  /**
    * A method which counts the number instances of the element in the multiset.
    * It enables to use '()'.
    * @param elem The element which it's counting.
    * @tparam A The method will cast comparing elements to this type.
    * @return The number of entrances.
    */
  def apply[A >: T](elem: A): Int

  /**
    * Add a new element into the multiset.
    * @param elem The added element
    * @tparam R type of the added element.
    * @return The resulting multiset.
    */
  def :+[R >: T](elem: R): Multiset[R]

  /**
    * Unite two sets into one. If a value is presented in both sets,
    * the number of it's instances in the result is maximum of numbers of instances in joined ones.
    * @param right The second multiset.
    * @tparam R Type of elements in the second multiset.
    * @return The union of this multiset and the second.
    */
  def ++[R >: T](right: Multiset[R]): Multiset[R]

  /**
    * Intersect two sets. If a value is presented in both sets,
    * the number of it's instances in the result is minimum of numbers of instances in joined ones.
    * @param right The second multiset.
    * @tparam R Type of elements in the second multiset.
    * @return The intersection of this multiset and the second.
    */
  def intersect[R >: T](right: Multiset[R]): Multiset[R]

  /**
    * Get the underlying sequence of the set.
    * The sequence contains each elements (including replications) in undefined order.
    * @return The sequence of elements in the set in undefined order.
    */
  def toSeq: Seq[T]

  /**
    * Perform action for each element of the set.
    * @param action The performed action.
    * @tparam R Type of the action. Actually, it doesn't matter.
    */
  def foreach[R](action: T => R): Unit = toSeq.foreach(action)
}

object Multiset {

  /**
    * The constructor of multiset which takes underlying sequence.
    * @param seq The sequence which will be transformed into Multiset.
    * @tparam T The type of elements in the sequence and in the resulting Multiset.
    * @return A multiset which contains all the elements from the sequence and only them.
    */
  def apply[T](seq: GenTraversableOnce[T]): Multiset[T] = new MultisetImpl[T](seq)

  def empty: Multiset[Any] = new MultisetImpl[Any](List.empty)

  private class MultisetImpl[T](seq: GenTraversableOnce[T]) extends Multiset[T] {

    private val map: Map[Int, (T, Int)] = seq.toList // Hash code -> (value, number)
      .groupBy(_.hashCode)
      .map {case (hash, list) => (hash, (list.head, list.length))}
    override val size: Int = seq.size

    override def filter(predicate: (T) => Boolean): Multiset[T] = new MultisetImpl[T](toSeq.filter(predicate))

    override def map[That](function: (T) => That): Multiset[That] = new MultisetImpl[That](toSeq.map(function))

    override def flatMap[That](function: (T) => GenTraversableOnce[That]): Multiset[That] =
      new MultisetImpl[That](toSeq.flatMap(function))

    override def find[That >: T](elem: That): Option[T] = toSeq.find(_ == elem)

    override def apply[A >: T](elem: A): Int = toSeq.count(_ == elem)

    override def toSeq: Seq[T] = map.flatMap{case (_, (value, number)) => List.fill(number)(value)}.toSeq

    override def :+[R >: T](elem: R): Multiset[R] = new MultisetImpl[R](toSeq :+ elem)

    override def ++[R >: T](right: Multiset[R]): Multiset[R] =
      new MultisetImpl[R]((toSeq.toSet ++ right.toSeq.toSet)
        .toList
        .flatMap(x => List.fill(Math.max(right(x), apply(x)))(x)))

    override def intersect[R >: T](right: Multiset[R]): Multiset[R] = new MultisetImpl[R](toSeq.intersect(right.toSeq))

    override def equals(obj: scala.Any): Boolean = obj match {
      case right: Multiset[T] =>
        right.toSeq.map(_.hashCode()).sorted ==
          toSeq.map(_.hashCode()).sorted
    }
  }


  /**
    * THe extractor of the constructor argument (The underlying sequence).
    * @param set The set from which the argument is extracted.
    * @tparam T The type of the set elements.
    * @return Extracted underlying seq.
    */
  def unapply[T](set: Multiset[T]): Option[Seq[T]] = Some(set.toSeq)

  // unapplySeq would be implemented in the same way
  // (because Multiset has a constructor which takes Seq)
  // therefore unapply and unapplySeq pattern matching would be clashed.
}