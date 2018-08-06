package nl.tudelft.pl2.data.builders

import java.io.{BufferedReader, File, FileReader}
import java.util.Observer
import java.util.logging.Logger

import nl.tudelft.pl2.data.Gfa1Parser
import nl.tudelft.pl2.data.Graph.Options
import nl.tudelft.pl2.data.caches.SubCache
import nl.tudelft.pl2.data.indexing.Indexer
import nl.tudelft.pl2.data.loaders.GraphPathCollection
import nl.tudelft.pl2.data.storage.writers.{CtagWriter, HeaderWriter, HeatMapWriter}
import nl.tudelft.pl2.representation.exceptions.CTagException
import nl.tudelft.pl2.representation.external.{Edge, Node}
import org.apache.logging.log4j.LogManager

import scala.collection.mutable


case class EmptyBuildNodeException(reason: String, loc: String)
  extends CTagException(reason, loc)


case class EmptyGraphException(reason: String, loc: String)
  extends CTagException(reason, loc)


case class InvalidGraphException(reason: String, loc: String)
  extends CTagException(reason, loc)


/**
  * Builds the files needed for 0th semantic zoom level.
  *
  * @param paths The collection of paths to different
  *              files used and written during building.
  */
class ZeroZoomBuilder(paths: GraphPathCollection) {
  /**
    * Log4J [[Logger]] used to log debug information
    * and other significant events.
    */
  private val LOGGER = LogManager.getLogger("ZeroZoomBuilder")

  /**
    * Writer of the header file.
    */
  private val headerWriter = new HeaderWriter(paths.headerPath)

  /**
    * Writer of the zoom file.
    */
  private val zoomWriter = new CtagWriter(paths.zeroFilePath)

  /**
    * Writer and builder of the index.
    */
  private val indexer = new Indexer(paths.zeroIndexPath)

  /**
    * Writer of the heat map.
    */
  private val heatMapWriter = new HeatMapWriter(paths.heatMapPath)

  /**
    * Points from to to from.
    */
  private val incomingMap = new mutable.HashMap[Int, mutable.Set[Int]]
    with mutable.MultiMap[Int, Int]


  /**
    * A map which converts a node name (as string) to the corresponding
    * [[BuilderNode]] object.
    */
  private val nodeMap = new mutable.HashMap[String, BuilderNode]

  /**
    * A map that converts a node ID (int) to the corresponding BuilderNode
    */
  private val nodeIdMap = new mutable.HashMap[Int, BuilderNode]

  /**
    * Store each path registered, such that we can calculate genome
    * positions when all nodes have been parsed.
    */
  private val graphPaths = new mutable.HashMap[String, Array[String]]

  /**
    * The mapping of genomes to their indices, or identifiers.
    */
  private val genomes: mutable.Map[String, Int] = mutable.HashMap[String, Int]()

  /**
    * Passes a header from to the [[HeaderWriter]] to be
    * stored to disk.
    *
    * @param options The [[Options]] representing the header.
    */
  def registerHeader(options: Options): Unit = {
    headerWriter.storeHeader(options)
  }

  /**
    * Builds a [[Node]] with [[Edge]] references to be
    * stored to disk.
    *
    * @param name    The [[Node]] name.
    * @param content The [[Node]] content.
    * @param options The [[Options]] that apply to this [[Node]].
    */
  def registerNode(name: String, content: String, options: Options): Unit = {
    if (nodeMap.contains(name)) {
      val node = nodeMap(name)

      node.content = content
      node.options ++= options
    } else {
      val id = nodeMap.size

      // Actual layer will be determined when all edges are parsed.
      // Genome coordinates will be determined when registering a Path
      val layer = 0
      nodeMap.put(name, new BuilderNode(
        id, name, layer, content, mutable.Buffer[Int](),
        mutable.Buffer[Int](), options, new mutable.HashMap[Int, Long]
      ))
      nodeIdMap.put(nodeMap(name).id, nodeMap(name))
    }
  }

  /**
    * Adds a reference to the [[Edge]] to the [[Node]] currently
    * being built for later storage.
    *
    * @param from         The origin [[Node]] of this [[Edge]].
    * @param reversedFrom Whether the origin [[Node]] is reversed.
    * @param to           The destination [[Node]] of this [[Edge]].
    * @param reversedTo   Whether the destination [[Node]] is reversed.
    * @param options      The [[Options]] that apply to this [[Edge]].
    */
  def registerEdge(from: String,
                   reversedFrom: Boolean,
                   to: String,
                   reversedTo: Boolean,
                   options: Options): Unit = {
    val nodeFrom: BuilderNode = if(nodeMap.contains(from)) {
      nodeMap(from)
    } else {
      val id = nodeMap.size
      val newNode = new BuilderNode(
        id, from, 0, "", mutable.Buffer[Int](), mutable.Buffer[Int](),
        new mutable.HashMap[String, (Char, String)], new mutable.HashMap[Int, Long]
      )
      nodeMap(from) = newNode
      nodeIdMap(newNode.id) = newNode

      newNode
    }

    val nodeTo: BuilderNode = if(nodeMap.contains(to)) {
      nodeMap(to)
    } else {
      val id = nodeMap.size
      val newNode = new BuilderNode(
        id, from, 0, "", mutable.Buffer[Int](), mutable.Buffer[Int](),
        new mutable.HashMap[String, (Char, String)], new mutable.HashMap[Int, Long]
      )
      nodeMap(to) = newNode
      nodeIdMap(newNode.id) = newNode

      newNode
    }

    nodeFrom.outgoing += nodeTo.id
    nodeTo.incoming += nodeFrom.id
  }

  def registerPath(name: String, nodes: Array[String]): Unit = {
    graphPaths(name) = nodes
  }

  /**
    * Perform topological sorting to assign a layer to each node.
    */
  private def assignLayers(): Unit = {
    // First find nodes without incoming edges, these nodes
    // will start in layer 0.
    var queue = Array[BuilderNode]()
    val indegreeMap = mutable.Map[Int, Int](
      nodeMap.values.toSeq.map(n => (n.id, n.incoming.size)).filter(e => e._2 > 0): _*)

    var i = 0
    nodeMap foreach { case (nodeName, node) =>
      if(node.incoming.isEmpty) {
        node.layer = 0
        queue :+= node

        LOGGER.debug(".. node {} (id: {}), layer 0", i, node.id)
        i += 1
      }
    }

    LOGGER.debug("Start topological sort... (queue size: {})", queue.size)
    while(queue.nonEmpty) {
      val node = queue.head
      queue = queue.tail
      LOGGER.debug("Current node: {}, queue size: {}", node, queue.size)

      for(target <- nodeIdMap(node.id).outgoing) {
        val targetNode: BuilderNode = nodeIdMap(target)
        if(!indegreeMap.contains(targetNode.id)) {
          throw InvalidGraphException("Graph is not acyclic, can't determine layers.",
            "ZeroZoomBuilder::assignLayers")
        }
        indegreeMap(targetNode.id) -= 1

        targetNode.layer = Math.max(targetNode.layer, node.layer + 1)
        LOGGER.debug(".. node {} (id: {}), layer {}", i, targetNode.id, targetNode.layer)
        i += 1

        if(indegreeMap(targetNode.id) == 0) {
          queue :+= targetNode
          indegreeMap.remove(targetNode.id)
        }
      }
    }

    if(indegreeMap.nonEmpty) {
      LOGGER.warn("Still elements in `indegreeMap`, graph is not acyclic.")
    }
  }

  private def assignGenomes(): Unit = {
    graphPaths foreach { case (path_name, nodes) =>
      val genomeId = genomes.size
      genomes(path_name) = genomeId
      var coord: Long = 0

      for(nodeName <- nodes) {
        val node = nodeMap(nodeName)

        node.genomes(genomeId) = coord
        coord += node.content.length
      }
    }

    // Create a fake header line which indicates the genomes present
    // Required for subsequent builders and other parts of the program
    if(graphPaths.nonEmpty) {
      val genomeHeader = mutable.Map("H" -> ('Z', graphPaths.keys.mkString(";")))
      registerHeader(genomeHeader)
    }
  }

  /**
    * Index all nodes in topological order such that nodes that are
    * placed closely together are in the same chunk.
    */
  private def writeIndex(): Unit = {
    var queue = Array[BuilderNode]()
    val indegreeMap = mutable.Map[Int, Int](
      nodeMap.values.toSeq.map(n => (n.id, n.incoming.size)).filter(e => e._2 > 0): _*)

    nodeMap foreach { case (nodeName, node) =>
      if(node.incoming.isEmpty) {
        writeNode(node)
        queue :+= node
      }
    }

    LOGGER.debug("Start topological sort for indexing...")
    while(queue.nonEmpty) {
      val node = queue.head
      queue = queue.tail

      for(target <- nodeIdMap(node.id).outgoing) {
        val targetNode: BuilderNode = nodeIdMap(target)

        if(!indegreeMap.contains(targetNode.id)) {
          throw InvalidGraphException("Graph is not acyclic, can't determine layers.",
            "ZeroZoomBuilder::assignLayers")
        }
        indegreeMap(targetNode.id) -= 1

        writeNode(targetNode)

        if(indegreeMap(targetNode.id) == 0) {
          queue :+= targetNode
          indegreeMap.remove(targetNode.id)
        }
      }
    }

    if(indegreeMap.nonEmpty) {
      LOGGER.warn("Still elements in `indegreeMap`, graph is not acyclic.")
    }
  }

  private def writeNode(node: BuilderNode): Unit = {
    val nodeLen = zoomWriter.storeNode(
      node.id,
      node.name,
      node.layer,
      node.content,
      node.incoming,
      node.outgoing,
      node.options,
      node.genomes)

    indexer.indexNode(node.id, nodeLen, node.layer)
    heatMapWriter.incrementLayerAt(node.layer)
  }

  /**
    * Flushes the [[ZeroZoomBuilder]] by storing and indexing
    * the [[Node]] that is currently being built, if there is
    * such a [[Node]].
    */
  def flush(): Unit = {
    if (zoomWriter.getFileLength == 0) {
      close()
      throw EmptyGraphException(paths.zeroFilePath + " contains no segments.",
        "In ZeroZoomBuilder.")
    }
    indexer.flush()
    heatMapWriter.flush()
  }

  /**
    * Closes the [[HeaderWriter]], [[CtagWriter]], and
    * [[Indexer]] associated with this [[ZeroZoomBuilder]].
    */
  def close(): Unit = {
    zoomWriter.close()
    headerWriter.close()
    indexer.close()
    heatMapWriter.close()
    LOGGER.debug("Closed all files.")
  }

  /**
    * Counts the number of times a substring is in a full string.
    *
    * @param str    The full string.
    * @param substr The substring.
    * @return The number of times the substring is in a full string.
    */
  def countSubstring(str: String, substr: String): Int
  = substr.r.findAllMatchIn(str).length
}

/**
  * Companion object to the [[ZeroZoomBuilder]].
  */
object ZeroZoomBuilder {
  /**
    * Builds the files needed for a [[SubCache]]
    *
    * @param paths The paths used for building additional files.
    */
  def buildFiles(paths: GraphPathCollection, observer: Observer): Unit = {
    val builder = new ZeroZoomBuilder(paths)
    val size = new File(paths.gfaPath.toUri).length()
    val reader = new BufferedReader(new FileReader(paths.gfaPath.toString))
    try {
      Gfa1Parser.parse(reader, builder, observer, size)
      builder.assignLayers()
      builder.assignGenomes()
      builder.writeIndex()
    } catch {
      case any: Any => any.printStackTrace()
    } finally {
      reader.close()
      try {
        builder.flush()
      } finally {
        builder.close()
      }
    }
  }
}
