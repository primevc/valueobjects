package prime.utils.msgpack
 import prime.vo.source.ValueSource
 import java.io.{ByteArrayInputStream,ByteArrayOutputStream}
 import kafka.utils._
 import kafka.producer._
 import kafka.producer.async.DefaultEventHandler
 import kafka.serializer.Encoder
 import kafka.serializer.Decoder
 import org.msgpack.{MessagePackObject,Unpacker}
 import org.msgpack.`object`._

object KafkaVOSerializer extends Encoder[Any] with Decoder[Any] {

  def newProducer(config : ProducerConfig) = new kafka.producer.Producer(
    config,
    new DefaultEventHandler[Any,Any](config,
                                     Utils.createObject[Partitioner[Any]](config.partitionerClass, config.props),
                                     KafkaVOSerializer,
                                     KafkaVOSerializer,
                                     new ProducerPool(config)));

  def toBytes(vo: Any): Array[Byte] = {
    val out : ByteArrayOutputStream = new ByteArrayOutputStream()
    val msgpack : VOPacker = new VOPacker(out)
    msgpack.pack(vo)
    out.toByteArray()
  }

  def fromBytes(bytes: Array[Byte]): Any = {
    val msgUnpacker = new Unpacker(new ByteArrayInputStream(bytes))
    msgUnpacker.setVOHelper(VOUnpacker)
    msgUnpacker.next.getData match {
      case r : MessagePackValueSource => r
      case r : ArrayType              => r.asArray
      case r : BooleanType            => if (r.asBoolean) java.lang.Boolean.TRUE else java.lang.Boolean.FALSE
      case r : IntegerType            => r.asLong
      case r : FloatType              => r.asDouble
      case r : MapType                => r.asMap
      case r : NilType                => null
      case r : RawType                => r.asString
    }
  }
}
