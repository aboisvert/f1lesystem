require 'buildr/scala'

VERSION_NUMBER = "0.0.1-SNAPSHOT"

repositories.remote << "http://repo1.maven.org/maven2"

desc "Scala file-system abstractions"
define "f1lesystem" do
  project.version = VERSION_NUMBER
  project.group = "org.alexboisvert"

  define "core" do
    package(:jar)
  end

  define "s3" do
    package(:jar)
  end
end
