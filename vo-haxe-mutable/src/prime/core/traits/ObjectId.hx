package prime.core.traits;
 import haxe.io.BytesOutput;
 import prime.utils.msgpack.Reader;
 import prime.core.traits.IObjectId;
  using prime.utils.IfUtil;
  using prime.utils.msgpack.Format;

class ObjectId
{
	static inline public var TYPE_ID = 0x01D;
	
	@:keep static public function msgpack_packVO(o : BytesOutput, obj : IObjectId, propertyBits : Int, prependMsgpackType : Bool = false) : Int
	{
		Assert.isNotNull(o);
		Assert.isNotNull(obj);
		
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
		Assert.isNotNull(reader);
		Assert.isNotNull(obj);
		Assert.isEqual(propertyBytes, 1);

		var fieldOffset:Int = (untyped obj)._fieldOffset(TYPE_ID);
#if debug
		var bits:Int = reader.readByte();
		Assert.that((bits & 0x01).not0());
		(untyped obj)._propertiesSet |= (bits              << fieldOffset);
#else
		(untyped obj)._propertiesSet |= (reader.readByte() << fieldOffset);
#end
		(untyped obj).id = reader.readMsgPackValue(0, prime.types.ObjectId);
	}
}
