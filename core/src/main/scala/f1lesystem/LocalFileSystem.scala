package f1lesystem

import java.io.{InputStream, Reader}
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.channels.FileChannel

object LocalFileSystem {

  /** Utility trait for local testing */
  trait TempRoot {
    val rootName: String

    val fs = new LocalFileSystem()

    // create and return empty test directory under java.io.tmpdir
    lazy val root: fs.Dir = {
      val tmp = fs.parseDirectory(System.getProperty("java.io.tmpdir")) /+ rootName
      if (tmp.exists) {
        tmp.deleteRecursively()
      }
      tmp.mkdir()
      tmp
    }
  }
}

class LocalFileSystem extends FileSystem {
  type PATH = LocalPath
  type FILE = LocalFile
  type DIR = LocalDir

  override def root = new LocalDir(None, "")

  def parseFile(path: String): FILE = {
    val file = new java.io.File(path)
    val parent = if (file.getParentFile == null) root else parseDirectory(file.getParentFile.getCanonicalPath)
    new LocalFile(parent, file.getName)
  }

  def parseDirectory(path: String): DIR = {
    val file = new java.io.File(path)
    new LocalDir(Option(file.getParentFile) map (_.getCanonicalPath) map parseDirectory, file.getName)
  }

  trait LocalPath extends Path {
    override def exists = file.exists

    override def delete() { file.delete() }

    private[f1lesystem] def file = new java.io.File(fullpath)
  }

  class LocalFile(override val parent: DIR, override val filename: String ) extends File with LocalPath {
    import java.nio.charset._

    override def readAsByteBuffer[T](f: ByteBuffer => T): T = {
      val raf = new java.io.RandomAccessFile(file, "r")
      try {
        // over 1MB, memory-map the file; otherwise wrap an FileInputStream
        if (raf.length > 1024 * 1024) {
          val buf = raf.getChannel.map(FileChannel.MapMode.READ_ONLY, 0, raf.length)
          f(buf)
        } else {
          val size = raf.length.toInt // WARNING: Does not support > 4GB files
          val buf = ByteBuffer.allocate(size.toInt)
          val c = raf.getChannel
          var pos = 0
          while (pos < size) {
            val n = c.read(buf)
            if (n == -1) pos = size
            else pos += n
          }
          buf.position(0) // ready to be read
          f(buf)
        }
      } finally raf.close()
    }

    override def readAsInputStream[T](f: InputStream => T): T = {
      val fis = new java.io.FileInputStream(file)
      try f(fis)
      finally fis.close()
    }

    override def readAsReader[T](f: Reader => T): T = {
      val reader = new java.io.BufferedReader(new java.io.FileReader(file))
      try f(reader)
      finally reader.close()
    }

    override def touch() {
      new java.io.FileOutputStream(file).close()
    }

    override def write(reader: java.io.Reader, size: Long): Unit = {
      val writer = new java.io.FileWriter(file)
      try StreamUtils.copyReader(reader, writer, size)
      finally writer.close()
    }

    override def write(stream: java.io.InputStream, size: Long): Unit = {
      val out = new java.io.FileOutputStream(file)
      try StreamUtils.copyStream(stream, out, size)
      finally out.close()
    }

    override def write(data: java.nio.ByteBuffer, size: Int): Unit = {
      val buf = data.slice().limit(size).asInstanceOf[ByteBuffer]
      val f = new java.io.RandomAccessFile(file, "rw")
      try {
        f.setLength(0) // truncate if needed
        val c = f.getChannel
        while (buf.hasRemaining) {
          c.write(buf)
        }
      } finally {
        f.close()
        data.position(data.position + size)
      }

    }

    override def copyFile(file: String) {
      val in = new java.io.FileInputStream(file)
      val out = new java.io.FileOutputStream(this.file)
      try StreamUtils.copyStream(in, out)
      finally {
        in.close()
        out.close()
      }
    }

    override def size: Long = file.length
  }

  class LocalDir(
    override val parent: Option[DIR],
    override val filename: String
  ) extends Dir with LocalPath {

    override def listFiles: Seq[FILE] = {
      val files = file.listFiles
      if (files == null) return Seq.empty
      files filter (_.isFile) map { f => new LocalFile(this, f.getName) }
    }

    override def listDirectories: Seq[DIR] = {
      val files = file.listFiles
      if (files == null) return Seq.empty
      files filter (_.isDirectory) map { d => new LocalDir(Some(this), d.getName) }
    }

    override def /(filename: String): FILE = new LocalFile(this, filename)

    override def /+(filename: String): DIR = new LocalDir(Some(this), filename)

    override def mkdir() { file.mkdirs() }

    override def deleteRecursively() {
      if (!exists) return
      for (d <- listDirectories) { d.deleteRecursively() }
      for (f <- listFiles) { f.delete() }
      delete()
    }
  }
}
