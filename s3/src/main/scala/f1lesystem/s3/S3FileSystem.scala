package f1lesystem.s3

import com.amazonaws.auth.{AWSCredentials, BasicAWSCredentials}
import com.amazonaws.services.s3.{AmazonS3, AmazonS3Client}
import com.amazonaws.services.s3.model.{GetObjectRequest, S3Object, S3ObjectSummary, ObjectMetadata}
import java.nio.charset.Charset
import f1lesystem._
import java.io.InputStream
import java.nio.ByteBuffer
import java.io.Reader
import java.io.InputStreamReader
import java.io.ByteArrayOutputStream
import java.io.FileInputStream
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import com.amazonaws.services.s3.model.ListObjectsRequest
import com.amazonaws.services.s3.model.ObjectListing
import com.amazonaws.services.s3.model.AmazonS3Exception

object S3FileSystem {
  def defaultS3Client = {
    val awsAccessKeyId = System.getProperty("AWS_ACCESS_KEY_ID", System.getenv("AWS_ACCESS_KEY_ID"))
    val awsSecretAccessKey = System.getProperty("AWS_SECRET_ACCESS_KEY", System.getenv("AWS_SECRET_ACCESS_KEY"))
    val creds = new BasicAWSCredentials(awsAccessKeyId, awsSecretAccessKey)
    new AmazonS3Client(creds)
  }
}

trait S3FileSystem extends FileSystem {
  import S3FileSystem._

  type PATH = S3Path
  type FILE = S3File
  type DIR = S3Dir

  def s3: AmazonS3 = defaultS3Client

  class S3FileNotFoundException(bucket: String, key: String) extends java.io.FileNotFoundException(s"bucket $bucket key $key")

  sealed trait S3Path extends Path {
    def bucket: String
    def key: String

    override def filename: String = {
      val pos = key.lastIndexOf("/")
      if (pos == -1) key
      else key.substring(pos + 1)
    }

    override def exists = {
      try {
        possibleFileNotFound {
          val meta = s3.getObjectMetadata(bucket, key)
          meta != null
        }
      } catch { case e: S3FileNotFoundException => false }
    }

    override def delete() { s3.deleteObject(bucket, key) }

    override def fullpath = s"s3://$bucket/$key"

    def possibleFileNotFound[T](f: => T) = {
      try f
      catch { case e: AmazonS3Exception if e.getStatusCode == 404 =>
        throw new S3FileNotFoundException(bucket, key)
      }
    }
  }

  case class S3File(bucket: String, key: String) extends File with S3Path {

    override def readAsByteBuffer[T](f: ByteBuffer => T): T = readAsInputStreamAndSize { (is, size) =>
      val sizeInt = size.toInt
      val buf = ByteBuffer.allocate(sizeInt)
      var pos = 0
      while (pos < sizeInt) {
        val n = is.read(buf.array, pos, sizeInt - pos)
        if (n == -1) pos = sizeInt
        else pos += n
      }
      buf.position(0) // ready to be read
      f(buf)
    }

    override def readAsInputStream[T](f: InputStream => T): T = {
      val obj = s3.getObject(bucket, key)
      val is = obj.getObjectContent
      try f(is)
      finally is.close()
    }

    override def write(reader: java.io.Reader, size: Long): Unit = ???

    override def write(stream: java.io.InputStream, size: Long): Unit = {
      val meta = new ObjectMetadata()
      meta.setContentLength(size)
      s3.putObject(bucket, key, stream, meta)
    }

    def readAsInputStreamAndSize[T](f: (InputStream, Long) => T): T = {
      val obj = s3.getObject(bucket, key)
      val is = obj.getObjectContent
      try f(is, obj.getObjectMetadata.getContentLength)
      finally is.close()
    }

    override def copyFile(file: String) {
      val fis = new FileInputStream(file)
      try {
        write(fis, new java.io.File(file).length)
      } finally fis.close()
    }

    override def size = {
      val meta = s3.getObjectMetadata(bucket, key)
      if (meta == null) throw new S3FileNotFoundException(bucket, key)
      meta.getContentLength
    }

    override def parent: S3Dir = {
      val pos = key.lastIndexOf("/")
      if (pos == -1) S3Dir(bucket, "")
      else S3Dir(bucket, key.substring(0, pos + 1))
    }

  }

  case class S3Dir(bucket: String, key: String) extends Dir with S3Path {
    override def /(filename: String) = S3File(bucket, key + filename)
    override def /+(filename: String) = S3Dir(bucket, key + filename + "/")
    override def deleteRecursively() {
      listDirectories foreach { _.deleteRecursively() }
      listFiles foreach { _.delete() }
      (this / "").delete()
    }

    override def listDirectories: Seq[S3Dir] = {
      var result = ArrayBuffer[S3Dir]()

      def append(listing: ObjectListing) {
        listing.getCommonPrefixes.asScala foreach { prefix =>
          if (prefix.endsWith("/")) {
            result += S3Dir(bucket, prefix)
          }
        }
      }

      val req = new ListObjectsRequest()
        .withBucketName(bucket)
        .withPrefix(key)
        .withDelimiter("/")

      var listing = s3.listObjects(req)
      append(listing)
      while (listing.isTruncated) {
        listing = s3.listNextBatchOfObjects(listing)
        append(listing)
      }
      result
    }

    override def listFiles: Seq[S3File] = {
      var result = ArrayBuffer[S3File]()

      def append(listing: ObjectListing) {
        listing.getObjectSummaries.asScala foreach { obj =>
          if (!obj.getKey.endsWith("/")) {
            result += S3File(bucket, obj.getKey)
          }
        }
      }

      val req = new ListObjectsRequest()
        .withBucketName(bucket)
        .withPrefix(key)
        .withDelimiter("/")

      var listing = s3.listObjects(req)
      append(listing)
      while (listing.isTruncated) {
        listing = s3.listNextBatchOfObjects(listing)
        append(listing)
      }
      result
    }

    override def mkdir(): Unit = { (this / "").touch() }

    override def parent: Option[S3Dir] = {
      val pos = key.dropRight(1).lastIndexOf("/")
      if (pos == -1) None
      else Some(S3Dir(bucket, key.substring(0, pos)))
    }

  }

  val URLRegex = """s3://([^/]+)/(.*)""".r
  val LegacyRegex = """([^:]+):(.*)""".r

  override def parseDirectory(path: String): S3Dir = {
    if (!path.endsWith("/")) throw new IllegalArgumentException("S3 directory must end with '/': " + path)
    val (bucket, key) = path match {
      case URLRegex(bucket, key)    => (bucket, key)
      case LegacyRegex(bucket, key) => (bucket, key)
      case _ => throw new IllegalArgumentException("Invalid S3 URL: " + path)
    }
    S3Dir(bucket, key)
  }
  override def parseFile(path: String): S3File = {
    if (path.endsWith("/")) throw new IllegalArgumentException("S3 file should not end with '/': " + path)
    val (bucket, key) = path match {
      case URLRegex(bucket, key)    => (bucket, key)
      case LegacyRegex(bucket, key) => (bucket, key)
      case _ => throw new IllegalArgumentException("Invalid S3 URL: " + path)
    }
    S3File(bucket, key)
  }

}