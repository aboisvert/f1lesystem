package f1lesystem

import java.io._
import java.io.IOException
import java.nio.ByteBuffer

/** Various Stream-related utility functions */
object StreamUtils {

  /** Copy from an input stream to the given output stream up to 'n' bytes. */
  @throws(classOf[java.io.IOException])
  def copyStream(in: InputStream, out: OutputStream, n: Long = Long.MaxValue) {
    val buffer = new Array[Byte](16*4096)
    var remaining = n

    def read = if (remaining < buffer.length) {
      in.read(buffer, 0, remaining.toInt)
    } else {
      in.read(buffer)
    }

    var bytesRead = read
    while (bytesRead != -1 && remaining > 0) {
      out.write(buffer, 0, bytesRead)
      remaining -= bytesRead
      bytesRead = read
    }
  }

  /** Copy from an input stream to the given output stream up to 'n' bytes. */
  @throws(classOf[java.io.IOException])
  def copyReader(in: Reader, out: Writer, n: Long = Long.MaxValue) {
    val buffer = new Array[Char](16*4096)
    var remaining = n

    def read = if (remaining < buffer.length) {
      in.read(buffer, 0, remaining.toInt)
    } else {
      in.read(buffer)
    }

    var bytesRead = read
    while (bytesRead != -1 && remaining > 0) {
      out.write(buffer, 0, bytesRead)
      remaining -= bytesRead
      bytesRead = read
    }
  }

  def using[C <: Closeable, T](closable: C)(f: C => T): T = {
    try {
      f(closable)
    } finally {
      closable.close()
    }
  }

  def streamToString(in: InputStream) = {
    try {
      val reader = new InputStreamReader(in)
      val writer = new StringWriter()
      StreamUtils.copyReader(reader, writer)
      writer.getBuffer.toString
    } finally {
      in.close()
    }
  }

  def readerToString(reader: Reader) = {
    try {
      val writer = new StringWriter()
      StreamUtils.copyReader(reader, writer)
      writer.getBuffer.toString
    } finally {
      reader.close()
    }
  }
}

class ByteBufferInputStream(val buf: ByteBuffer) extends InputStream {
  override def read(): Int = synchronized {
    if (!buf.hasRemaining) return -1
    buf.get
  }
  override def read(bytes: Array[Byte], off: Int, len: Int) = synchronized {
    val read = math.min(len, buf.remaining)
    buf.get(bytes, off, len)
    read
  }
}

class ByteBufferOutputStream(val buf: ByteBuffer) extends OutputStream {
  override def write(b: Int) = synchronized {
    buf.put(b.asInstanceOf[Byte])
  }
  override def write(bytes: Array[Byte], off: Int, len: Int) = synchronized {
    buf.put(bytes, off, len)
  }
}
