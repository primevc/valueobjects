package primevc.types;
 import haxe.io.BytesOutput;
  using primevc.utils.IfUtil;
  using primevc.utils.msgpack.Format;

class UniqueID
{
	static inline public var TYPE_ID = 1;
	
	static public function msgpack_packVO(o : BytesOutput, obj : IUniqueID, prependMsgpackType : Bool = false, propertyBits : Int) : Int
	{
		Assert.that(o != null && obj != null);
		
		var b /* bytes written */ : Int;
		if (prependMsgpackType) {
			if (propertyBits.not0()) b = o.packValueObject();
			else return o.packNil();
		}
		else b = 0;
		
		b += o.packValueObjectHeader(TYPE_ID, 0, propertyBits);
		
		if (propertyBits.not0()) {
			o.writeByte(0x01); ++b;
			b += o.packUniqueID(obj.id);
		}
		
		return b;
	}
	
	var value : String;
	
	private function new() {
		
	}
	
	public function toString() {
		return value;
	}
}
