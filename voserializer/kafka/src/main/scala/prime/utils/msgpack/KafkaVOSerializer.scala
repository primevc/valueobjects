package prime.utils.msgpack
 import prime.vo.ValueObject
 import prime.vo.source.ValueSource
 import java.io.{ByteArrayInputStream,ByteArrayOutputStream}
 import kafka.message.Message
 import kafka.serializer.Encoder
 import kafka.serializer.Decoder
 import org.msgpack.Unpacker

class KafkaVOSerializer extends Encoder[ValueObject] with Decoder[ValueSource] {

	def toMessage(event: ValueObject): Message = {
		val out : ByteArrayOutputStream = new ByteArrayOutputStream()
		val msgpack : VOPacker = new VOPacker(out)
		msgpack.pack(event)
		new Message(out.toByteArray())
	}

	def toEvent(message: Message): ValueSource = {
		val buf = message.payload
		val arr = new Array[Byte] (buf.remaining)
		buf.get(arr)
		val msgUnpacker = new Unpacker(new ByteArrayInputStream (arr))
		msgUnpacker.setVOHelper(VOUnpacker)
		msgUnpacker.next.asInstanceOf[MessagePackValueSource]
	}
}
