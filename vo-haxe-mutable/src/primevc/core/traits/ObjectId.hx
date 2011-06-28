package primevc.core.traits;
 import haxe.io.BytesOutput;
 import primevc.utils.msgpack.Reader;
 import primevc.core.traits.IObjectId;
  using primevc.utils.IfUtil;
  using primevc.utils.msgpack.Format;

class ObjectId
{
	static inline public var TYPE_ID = 0x01D;
	
	@:keep static public function msgpack_packVO(o : BytesOutput, obj : IObjectId, propertyBits : Int, prependMsgpackType : Bool = false) : Int
	{
		Assert.notNull(o);
		Assert.notNull(obj);
		
		var b /* bytes written */ : Int;
		if (prependMsgpackType) {
			if (propertyBits.not0()) b = o.packValueObject();
			else return o.packNil();
		}
		else b = 0;
		
		b += o.packValueObjectHeader(TYPE_ID, 0, propertyBits);
		
		if (propertyBits.not0()) {
			o.writeByte(0x01); ++b;
			b += o.packObjectId(obj.id);
		}
		
		return b;
	}
	
	@:keep static public function msgpack_unpackVO(reader : Reader, obj : IObjectId, propertyBytes : Int) : Void
	{
		Assert.that(reader != null && obj != null);
		var bits:Int;
		
		if (!(propertyBytes).not0()) return;

		--propertyBytes;
		
		bits = reader.readByte();
		if ((bits & 0x01).not0()) (untyped obj).setId(reader.readMsgPackValue(0, primevc.types.ObjectId));
		
		if ((propertyBytes).not0()) reader.discardRemainingVOProperties(propertyBytes);
	}
}
