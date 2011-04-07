/*
 * Stardict.scala
 */
package jstardict

import java.io.{BufferedInputStream,
                FileInputStream,
                RandomAccessFile,
                File, FilenameFilter}
import java.lang.System._

/**
 * Stardict implemented by Scala running on JVM. 
 * Scala: 2.8.1
 */
object Stardict {

  /**
   * This method takes only one argument, the folder in which
   * the dictionaries are stored. One dictionary consists of two
   * files: one is the dictionary itself, the file name extension
   * of which is 'dict'; the other is the index file, with extension 'idx'.
   * For example: oald.dict and oald.idx. The file name without
   * extensions is used to initialize dictionaries.
   */
  def main(args:Array[String]) {
    if (0 >= args.length) {
      println("Please specify the directory where dictionaries are stored.")
      exit(1)
    }

    val dicts = load(args(0))

    while(true) {
      val word = Console.readLine()

      if (word == "\\") bye

      print(word + " | ")
      for (exp <- lookup(dicts, word)) 
        print(exp + " <<<")
    }
  }

  /**
   * Print bye-bye and exit the application.
   */
  def bye {print("\nBye-bye!\n"); exit(0)}

  /**
   * Load dictionaries found in the directory indicated
   * by the given path.
   */
  def load(path:String):Array[Dictionary] = {
    val paths = new File(path).list(new FilenameFilter() {
      def accept(dir:File, name:String):Boolean = {
        name.endsWith(".dict")
      }
    })

    if (null == paths || 0 >= paths.length) {
      println("No dictionaries found under [" + path + "]. Exit.")
      exit(2)
    }

    for {p <- paths
         dict = new SimpleDictionary(path + (if (path.endsWith(File.separator)) ""
                                             else File.separator) +
                                     p.substring(0, p.length - 5))
       } yield dict
  }

  /**
   * Look up the given word in the dictionaries, and return
   * an array of explanations.
   */
  def lookup(dicts:Array[Dictionary], word:String):Array[String] = {
    for {dict <- dicts
         exp = try {dict.lookup(word)} catch {case _=> "[Not found]"}
       } yield exp
  }
}


/**
 * Base class for all dictionary implementations.
 */
abstract class Dictionary {

  def getWordIdx(word:String, min:Long, max:Long):WordIdx = {

    Util.info("word => " + word + ", min => "
                + min + ", max => " + max)
    if (max == (min + 1)) return NotFound

    val pos = min + (max - min) / 2
    val wi = getWordIdx(pos)
    Util.info("wi => " + wi)

    val found = word.compareTo(wi.word)

    if (found > 0)
      getWordIdx(word, pos, max)
    else if (found < 0)
      getWordIdx(word, min, pos)
    else
      wi
  }

  def lookup(word:String):String

  def getWordIdx(pos:Long):WordIdx = NotFound
}


object TestDictionary extends Dictionary {

  override def getWordIdx(pos:Long):WordIdx = {
    new WordIdx(words(pos.asInstanceOf[Int]), 0, 0)
  }

  override def lookup(word:String):String = {
    getWordIdx(word, 0, words.length).toString()
  }
  
  val words = Array("brute", "cushy", "disown", "dovetail",
                    "halve", "muck", "sack", "seek", "seize")
}


/**
 * A simple dictionary is represented by two files:
 * one is <dictionary-name>.DICT, the other is
 * <dictionary-name>.IDX. These two files should
 * reside in the same directory for a successful
 * loading.
 */
class SimpleDictionary(name:String) extends Dictionary {

  val dictFile = new RandomAccessFile(name + ".dict", "r")
  val dictIdx = new DictIdxMap(name)

  override def lookup(word:String):String = {
    val wordIdx = dictIdx.get(word)
    if (null == wordIdx)
      "Sorry, can\'t find word: " + word
    else
      lookup(wordIdx)
  }
  
  def lookup(wordIdx:WordIdx):String = {
    val cont = new Array[Byte](wordIdx.size)
    dictFile.seek(wordIdx.offset)
    dictFile.read(cont)
    new String(cont, "utf-8")
  }
}


abstract class DictIdx(name:String) {
  def add(widx:WordIdx):Unit

  def get(word:String):WordIdx

  final def initialize():Unit = {
    val idxFile =
      new BufferedInputStream(new FileInputStream(name + ".idx"))
    try {
      val cont = new Array[Byte](1024)
      var n = idxFile.read(cont)
      while(n != -1) {
        var j = -1; var k = 0
        for (i <- 0 until n) {
          if (-1 == j) {
            if (0 == cont(i))
              j = 0
            else
              k += 1
          }
          else if (8 > j) j += 1
          
          if (8 == j) {
            val word = new String(cont, i - 8 - k, k, "UTF-8")
            val offset = Util.intValue(cont, i - 8 + 1)
            val size = Util.intValue(cont, i - 4 + 1)
            val wi = new WordIdx(word, offset, size)
            add(wi)
            k = 0; j = -1
          }
        }

        var m = 0
        if (8 > j) {
          m = k + j + 1
          if (m > 0) System.arraycopy(cont, n - m, cont, 0, m)
        }

        n = idxFile.read(cont, m, cont.length - m) + m
      }
    }
    finally {
      idxFile.close()
    }
  }
}


/**
 * A dictionary index implementation on Map structure.
 */
class DictIdxMap(name:String) extends DictIdx(name) {
  import scala.collection.mutable._
  private val idx = new HashMap[String, WordIdx]()

  override def add(widx:WordIdx):Unit = {
    idx.put(widx.word, widx)
  }

  override def get(word:String):WordIdx = {
    idx.get(word).get
  }

  initialize()
}


/**
 * A simple structure for word index information,
 * which includes the word, the offset and the data size
 * in the 'dict' file.
 */
class WordIdx(val word:String, val offset:Int, val size:Int) { 
  override def toString() = {
    "[word: " + word + ", offset: " + offset + ", size: " + size + "]"
  }
}

object NotFound extends WordIdx("", -1, -1)


object Util {
  def info(str:String) = println("[INFO] " + str)
  def debug(str:String) = if (_debug) println("[DEBUG] " + str)
  val _debug = false

  /**
   * Convert a four-byte array to an integer.
   * &0xff is necessary to remove the minus sign as
   * a minus Byte value is longer than 8 bits.
   */
  final def intValue(cont:Array[Byte], start:Int):Int = {
    var n = 0
    for (i <- 0 to 3) n = (n << 8)|(cont(start + i)&0xff)
    n
  }

  final def testIntValue():Unit = {
    val b = new Array[Byte](4)
    b(0) = 18; b(1) = 120; b(2) = 0; b(3) = 90
    var bs = ""
    for(n <- b) {
      bs += Integer.toBinaryString(n)
    }
    info(bs)
      
    val i = Util.intValue(b, 0)
    info(Integer.toBinaryString(i))
  }
}

