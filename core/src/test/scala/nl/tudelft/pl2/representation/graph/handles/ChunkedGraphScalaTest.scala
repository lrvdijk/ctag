package nl.tudelft.pl2.representation.graph.handles

import java.util
import java.nio.file.{Path, Paths}

import nl.tudelft.pl2.representation.external.Node
import nl.tudelft.pl2.representation.graph.loaders.ChunkedGraphLoader
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.mutable
import scala.collection.mutable.Map

@RunWith(classOf[JUnitRunner])
class ChunkedGraphScalaTest extends FunSuite {
  val TB10S_PATH: Path = Paths.get(Thread
    .currentThread
    .getContextClassLoader
    .getResource("TB10_small.gfa").toURI)

  private var vMap: util.Map[Integer, Integer] = _
  private var lMap: util.HashMap[Integer, util.HashSet[Node]] = _
  private var loader: ChunkedGraphLoader = _
  private var chunkedGraph: ChunkedGraph = _

  def before(): Unit = {
    vMap = new util.HashMap[Integer, Integer]()
    lMap = new util.HashMap[Integer, util.HashSet[Node]]()
    loader = new ChunkedGraphLoader()
    chunkedGraph = new ChunkedGraph(loader, vMap, lMap, TB10S_PATH)
  }

  test("Test sample compare string") {
    before()
    val key = "ORI"
    val value = "sample"

    val options: mutable.Map[String, (Char, String)] =
      mutable.HashMap()
    options.put(key, ('z', value))
    //scalastyle:off null
    val node = new Node(0, "1", 0, "ACT", null, null, options, null)
    //scalastyle:on null
    val nodeSet = new java.util.HashSet[Node]()
    nodeSet.add(node)
    lMap.put(0, nodeSet)
    assertResult(node) {
      chunkedGraph.getNodesByGenome(value).iterator().next()
    }
  }

  test("Test sample compare number") {
    before()
    val key = "ORI"
    val valueString = "sample"
    val valueNumber = "0"

    val options: mutable.Map[String, (Char, String)] =
      mutable.HashMap()
    options.put(key, ('z', valueNumber))
    //scalastyle:off null
    val node = new Node(0, "1", 0, "ACT", null, null, options, null)
    //scalastyle:on null
    val nodeSet = new java.util.HashSet[Node]()
    nodeSet.add(node)
    lMap.put(0, nodeSet)
    val headers = new java.util.HashMap[String, String]()
    headers.put(key, valueString)
    assertResult(node) {
      chunkedGraph.updateHeaders(headers)
      chunkedGraph.getNodesByGenome(valueString).iterator().next()
    }
  }

  test("Test sample compare number false") {
    before()
    val key = "ORI"
    val valueString = "sample;sample2"
    val valueNumber = "0"
    val valueNumber2 = "1"

    val options: mutable.Map[String, (Char, String)] =
      mutable.HashMap()
    options.put(key, ('z', valueNumber2))
    //scalastyle:off null
    val node = new Node(0, "1", 0, "ACT", null, null, options, null)
    //scalastyle:on null
    val nodeSet = new java.util.HashSet[Node]()
    nodeSet.add(node)
    lMap.put(0, nodeSet)
    val headers = new java.util.HashMap[String, String]()
    headers.put(key, valueString)
    assertResult(true) {
      chunkedGraph.updateHeaders(headers)
      chunkedGraph.getNodesByGenome(valueString).isEmpty
    }
  }
}
