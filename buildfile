require 'buildr/scala'

VERSION_NUMBER = "0.0.2"

Buildr.settings.build['scala.test'] = "org.scalatest:scalatest_#{Buildr::Scala.version_without_build}:jar:2.0.M5"

repositories.remote << "http://repo1.maven.org/maven2"

HTTPCLIENT = 'org.apache.httpcomponents:httpclient:jar:4.2.1'
HTTPCORE = 'org.apache.httpcomponents:httpcore:jar:4.2.1'
COMMONSLOG = 'commons-logging:commons-logging:jar:1.1.1'

CODEC = 'commons-codec:commons-codec:jar:1.5'

SLF4J_VERSION = "1.5.6"
SLF4J = [
  "org.slf4j:slf4j-api:jar:#{SLF4J_VERSION}",
  "org.slf4j:slf4j-log4j12:jar:#{SLF4J_VERSION}",
  "org.slf4j:jcl-over-slf4j:jar:#{SLF4J_VERSION}"
]
LOG4J = "log4j:log4j:jar:1.2.15"

AWS_SDK = 'com.amazonaws:aws-java-sdk:jar:1.5.5'

desc "Scala file-system abstractions"
define "f1lesystem" do
  project.version = VERSION_NUMBER
  project.group = "org.alexboisvert"

  scala_suffix = "_2.10"
  
  define "core" do
    package(:jar, :id => id + scala_suffix)
  end

  define "s3" do
    compile.with project("core"), AWS_SDK, HTTPCORE, HTTPCLIENT, COMMONSLOG, CODEC

    compile.using :deprecation => true,
                  :other => ['-unchecked', '-Xprint-types']

    test.using :scalatest
    test.with LOG4J, SLF4J

    package(:jar, :id => id + scala_suffix)
  end
end
