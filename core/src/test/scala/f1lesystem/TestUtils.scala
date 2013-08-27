package f1lesystem

object TestUtils {
  implicit def stringToByteBuffer(s: String): java.nio.ByteBuffer = FileSystem.UTF8.encode(s)
}
