package v3

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.io.StdIn

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

object Globals {
  // For blocking file IO or blocking crypto.
  val cachedThreadPool = Executors.newCachedThreadPool()
  val blockingExecutionContext = ExecutionContext.fromExecutor(cachedThreadPool)
  val stdin = StdIn

  val client = {
    import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration
    import com.amazonaws.services.dynamodbv2.AmazonDynamoDBAsyncClientBuilder
    val conf = new EndpointConfiguration("http://localhost:8000", "us-east-1")
    AmazonDynamoDBAsyncClientBuilder.standard().withEndpointConfiguration(conf).build()
  }

  object Implicits {
    implicit val system = ActorSystem()
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher
  }
}
