package f1lesystem

import java.io.{InputStream, Reader}
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.io.InputStreamReader

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

    def touch(): Unit = { write("", FileSystem.UTF8) }

    def readAsInputStream[T](f: InputStream => T): T
    
    def readAsByteBuffer[T](f: ByteBuffer => T): T
    
    def readAsString(charSet: Charset = FileSystem.UTF8): String = {
      readAsByteBuffer { buf => charSet.newDecoder().decode(buf).toString }
    }
    
    def readAsReader[T](f: Reader => T): T = {
      readAsInputStream { is => f(new InputStreamReader(is)) }
    }

    def write(data: ByteBuffer, size: Int): Unit = {
      write(new ByteBufferInputStream(data), size)
    }
    
    def write(str: String, charSet: Charset = FileSystem.UTF8): Unit = {
      val encoded = charSet.encode(str)
      write(encoded, encoded.limit)
    }
    
    def write(stream: InputStream, size: Long): Unit = {
      write(new InputStreamReader(stream), size)
    }
    
    def write(reader: Reader, size: Long): Unit
    
    def copyFile(file: String): Unit
    def copyFile(file: LocalFileSystem#LocalFile): Unit = { copyFile(file.fullpath) }
    
    def size: Long

    override def fullpath = parent.fullpath + "/" + filename
  }
}

