import java.net.URLClassLoader

package object forcomp {
  val dictionaryPath = List("forcomp", "linuxwords.txt")

  def loadDictionary = {
    val wordstream = Option {
      getClass.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      common.resourceAsStreamFromSrc(dictionaryPath)
    } orElse {
      val resourceFile = new java.io.FileInputStream("""C:\Users\I066469\func-prog-course\fpcourse.week5\src\main\resources\forcomp\linuxwords.txt""")
      Some(resourceFile)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }

    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }

}
