package f1lesystem

import java.io.{InputStream, Reader}
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.io.InputStreamReader

object FileSystem {
  val UTF8 = Charset.forName("UTF-8")
}

trait FileSystem {
  type PATH <: Path
  type FILE <: PATH with File
  type DIR  <: PATH with Dir

  def root: DIR

  def parseFile(path: String): FILE
  def parseDirectory(path: String): DIR

  trait Path {
    // The 'asInstanceOf' is theoretically wrong (of course) but is used here to prevent boilerplate in subclasses
    final protected def path: PATH = this.asInstanceOf[PATH]

    final def filesystem: FileSystem.this.type = FileSystem.this

    def filename: String

    def fullpath: String

    def parentOrRootDir: DIR

    def sibling(name: String): PATH

    /** Return file extension.
     *
     *  e.g.
     *         "foo.bar"        => "bar"
     *         "package.tar.gz" => "gz"
     *         "foo"            => ""
     *         "foo."           => ""
     */
    final def extension: String = {
      val dot = filename.lastIndexOf(".")
      if (dot == -1) ""
      else filename.substring(dot + 1, filename.length)
    }

    /** Return all file extensions as a single string
     *
     *  e.g.
     *         "foo.bar"        => "bar"
     *         "package.tar.gz" => "tar.gz"
     *         "foo"            => ""
     *         "foo."           => ""
     */
    final def extensions: String = {
      val dot = filename.indexOf(".")
      if (dot == -1) ""
      else filename.substring(dot + 1, filename.length)
    }

    /** Return filename without its (last) extension.
     *
     *   e.g.
     *           "foo.bar"        => "foo"
     *           "package.tar.gz" => "package.tar"
     *           "foo"            => "foo"
     *           "foo."           => "foo"
     */
    final def basename: String = {
      val dot = filename.lastIndexOf(".")
      if (dot == -1) filename
      else filename.substring(0, dot)
    }

    /** Return a path with this path's extension (if any) changed to `extension`.
     *
     *  e.g.
     *        "foo.bar".withExtension("baz")     => "foo.baz"
     *        "foo".withExtension("baz")         => "foo.baz"
     *        "foo.tar.gz".withExtension("baz")  => "foo.tar.baz"
     *        "foo.".withExtension("baz")        => "foo.tar.baz"
     */
    final def withExtension(extension: String): PATH = {
      sibling(basename + "." + extension)
    }

    /** Return a path with this path's extension (if any) changed to `extension`.
     *
     *  e.g.
     *        "foo.bar".trimExtensions   => "foo"
     *        "foo".trimExtensions       => "foo"
     *        "foo.tar.gz".trimExtension => "foo"
     *        "foo.".trimExtensions      => "foo"
     */
    final def trimExtensions: PATH = {
      var s = path
      while (s.basename != s.filename) {
        s = s.sibling(s.basename)
      }
      s
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

    override final def sibling(filename: String): DIR = parentOrRootDir /+ filename

    override final def parentOrRootDir: DIR = parent getOrElse FileSystem.this.root

    final def isRootdir = parent.isDefined

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

    override final def sibling(filename: String): FILE = parentOrRootDir / filename

    override final def parentOrRootDir: DIR = parent

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
    def copyFile(file: LocalFileSystem.LocalFile): Unit = { copyFile(file.fullpath) }

    def size: Long

    override def fullpath = parent.fullpath + "/" + filename
  }
}

