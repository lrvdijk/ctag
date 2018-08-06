package nl.tudelft.pl2.data

import scala.collection.mutable

/**
  * Namespace wrapping auxiliary type definitions for
  * in-memory representation of graphs.
  */
object Graph {

  /**
    * A type representing a list/map of options
    * that can be (optionally) provided to each
    * line of a GFA1.0 file.
    */
  type Options = mutable.Map[String, (Char, String)]

  type Coordinates = mutable.Map[Int, Long]
}
