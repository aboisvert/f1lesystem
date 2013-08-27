package f1lesystem

import java.io.{InputStream, Reader}
import java.nio.ByteBuffer
import java.nio.charset.Charset

object FileSystem {
  val UTF8 = Charset.forName("UTF-8")
}

trait FileSystem {
  type FILE <: File
  type DIR  <: Dir

  def root: DIR

  def parseFile(path: String): FILE
  def parseDirectory(path: String): DIR

  trait Path {
    def filename: String

    def fullpath: String

    /** Return file extension, e.g. "foo.bar" => "bar" or "package.tar.gz" => "tar.gz" */
    def extension: String = {
      val dot = filename.indexOf(".")
      if (dot == -1) ""
      else filename.substring(dot + 1, filename.length)
    }

    /** Return filename without extension, e.g. "foo.bar" => "foo" or "package.tar.gz" => "package" */
    def basename: String = {
      val dot = filename.indexOf(".")
      if (dot == -1) filename
      else filename.substring(0, dot)
    }

    /** Returns true if file or directory exists */
    def exists: Boolean

    /** Delete the file or directory. */
    def delete(): Unit

    override def equals(x: Any) = x match {
      case other: Path => other.fullpath == this.fullpath
      case _ => false
    }

    override def hashCode = fullpath.hashCode

    override def toString = fullpath
  }

  trait Dir extends Path {
    def parent: Option[DIR]

    def listFiles: Seq[FILE]
    def listDirectories: Seq[DIR]

    def /(filename: String): FILE
    def /+(filename: String): DIR

    def mkdir(): Unit

    def deleteRecursively(): Unit

    override def fullpath = (parent map (_.fullpath) getOrElse "") + "/" + filename
  }

  trait File extends Path {
    def parent: DIR

    def touch(): Unit

    def readAsByteBuffer(): ByteBuffer
    def readAsInputStream(): InputStream
    def readAsString(charSet: Charset = FileSystem.UTF8): String
    def readAsReader(): Reader

    def write(data: ByteBuffer): Unit
    def write(str: String, charSet: Charset = FileSystem.UTF8): Unit
    def write(stream: InputStream): Unit
    def write(reader: Reader): Unit

    override def fullpath = parent.fullpath + "/" + filename
  }
}

