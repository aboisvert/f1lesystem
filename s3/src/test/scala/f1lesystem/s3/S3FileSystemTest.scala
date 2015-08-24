package f1lesystem.s3

import java.io.File
import org.scalatest.{OneInstancePerTest, WordSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.BeforeAndAfterEach

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class S3FileSystemTest extends WordSpec with ShouldMatchers with BeforeAndAfterEach {
  val fs = new S3FileSystem {
    def root: S3Dir = parseDirectory("bizo-dev:alex/f1lesystem/")
  }

  val tmp = fs.root

  override def beforeEach() {
    tmp.deleteRecursively()
  }

  "S3FileSystem" should {

    "return fullpath" in {
      tmp should be === new fs.S3Dir("bizo-dev", "alex/f1lesystem/")
      tmp.fullpath should be === "s3://bizo-dev/alex/f1lesystem/"
    }

    "parse directory" in {
      fs.parseDirectory(tmp.fullpath) should be === tmp
    }

    "parse file" in {
      val foo = tmp / "foo"
      fs.parseFile(foo.fullpath) should be === foo
    }

    "return file basename and extesion" in {
      val foo = tmp / "foo"
      foo.basename should be === "foo"
      foo.parent should be === tmp
      foo.extension should be === ""

      val bar = tmp / "bar.ext"
      bar.basename should be === "bar"
      bar.parent should be === tmp
      bar.extension should be === "ext"

      val tgz = tmp / "archive.tar.gz"
      tgz.basename should be === "archive.tar"
      tgz.extension should be === "gz"
    }

    "return empty directories and files for empty directory" in {
      tmp.listDirectories should be === Seq.empty
      tmp.listFiles should be === Seq.empty
    }

    "create directory using mkdir()" in {
      val subDir = tmp /+ "subDir"
      subDir.exists should be === false

      subDir.mkdir()
      subDir.exists should be === true
    }

    "create file using touch()" in {
      val subFile = tmp / "subFile"
      subFile.exists should be === false

      subFile.touch()
      subFile.exists should be === true

      tmp.listFiles should be === Seq(tmp / "subFile")
    }

    "list files and dirs" in {
      tmp.listDirectories should be === Seq.empty
      tmp.listFiles should be === Seq.empty

      val subFile = tmp / "subFile"
      subFile.touch()

      tmp.listFiles should be === Seq(tmp / "subFile")
      tmp.listDirectories should be === Seq.empty

      val subDir = tmp /+ "subDir"
      subDir.mkdir()

      tmp.listFiles should be === Seq(tmp / "subFile")
      tmp.listDirectories should be === Seq(tmp /+ "subDir")


      (subDir / "subFile2").touch()
      (subDir / "subDir2").touch()

      tmp.listFiles should be === Seq(tmp / "subFile")
      tmp.listDirectories should be === Seq(tmp /+ "subDir")

      subFile.delete()
      tmp.listFiles should be === Seq.empty
      tmp.listDirectories should be === Seq(tmp /+ "subDir")

      subDir.deleteRecursively()
      tmp.listFiles should be === Seq.empty
      tmp.listDirectories should be === Seq.empty
    }

    "delete directory" in {
      val subDir = tmp /+ "subDir"
      subDir.exists should be === false

      subDir.mkdir()
      subDir.exists should be === true

      subDir.delete()
      subDir.exists should be === false
    }

    "delete file" in {
      val subFile = tmp / "subFile"
      subFile.exists should be === false

      subFile.touch()
      subFile.exists should be === true

      subFile.delete()
      subFile.exists should be === false
    }

    "list files and directories" in {
      val subDir1 = tmp /+ "subDir1"
      subDir1.mkdir()

      val subDir2 = subDir1 /+ "subDir2"
      subDir2.mkdir()

      val subDir3 = subDir1 /+ "subDir3"
      subDir3.mkdir()

      val subDir4 = subDir3 /+ "subDir4"
      subDir4.mkdir()

      val subFile1 = subDir2 / "subFile1"
      subFile1.touch()

      val subFile2 = subDir2 / "subFile2"
      subFile2.touch()

      val subFile3 = subDir3 / "subFile3"
      subFile3.touch()

      subDir1.listFiles.toSet should be === Set()
      subDir1.listDirectories.toSet should be === Set(subDir2, subDir3)

      subDir2.listFiles.toSet should be === Set(subFile1, subFile2)
      subDir2.listDirectories.toSet should be === Set()

      subDir3.listFiles.toSet should be === Set(subFile3)
      subDir3.listDirectories.toSet should be === Set(subDir4)
    }

    "delete files and directories recursively" in {
      val subDir1 = tmp /+ "subDir1"
      subDir1.mkdir()

      val subDir2 = subDir1 /+ "subDir2"
      subDir2.mkdir()

      val subDir3 = subDir1 /+ "subDir3"
      subDir3.mkdir()

      val subFile1 = subDir2 / "subFile1"
      subFile1.touch()

      val subFile2 = subDir2 / "subFile2"
      subFile2.touch()

      subDir1.deleteRecursively()

      subDir1.exists should be === false
      subDir2.exists should be === false
      subDir3.exists should be === false

      subFile1.exists should be === false
      subFile2.exists should be === false
    }

    "generate paths" in {
      val foo = tmp /+ "foo" / "bar"
      (foo.fullpath drop tmp.fullpath.length) should be === "foo/bar"
    }

    "read and write to a file using strings" in {
      val foo = tmp / "foo"
      foo.write("foo")
      foo.readAsString() should be === "foo"
    }

    "read and write to a file using byte array" in {
      val foo = tmp / "foo2"
      foo.write(Array[Byte](1, 2, 3, 4, 5, 42))
      foo.readAsByteArray().toSeq should be === Seq[Byte](1, 2, 3, 4, 5, 42)
    }

  }
}