package primevc.utils.msgpack;
 import haxe.io.Output;
 import haxe.Int32;
 import primevc.utils.FastArray;

/**
 * MessagePack values to bytes formatter
 * @author Danny Wilson
 * @creation-date nov 22, 2010
 */
class Format
{
	/**
	 * Writes 'value' in the most compact MsgPack form possible, to output 'o'.
	 * Returns: bytes written.
	 */
	static public function packInt(o : Output, value : Int) : Int
	{
		if (value >= 0 && value < 0x10000)
		{
			if (value <= 127) {
			 	o.writeByte(value);
				return 1;
			}
			else if (value <= 255) {
				o.writeByte(0xcc);
				o.writeByte(value);
				return 2;
			}
			else {
				o.writeByte(0xcd);
				o.writeUInt16(value);
				return 3;
			}
		}
		else if (value < 0 && value >= -0x8000)
		{
			if (value >= -31) {
				o.writeByte(0xE0 | -value);
				return 1;
			}
			else if (value >= -0x80) {
				o.writeByte(0xd0);
				o.writeInt8(value);
				return 2;
			}
			else {
				o.writeByte(0xd1);
				o.writeInt16(value);
				return 3;
			}
		}
		else { // Int32
			o.writeByte(0xd2);
			o.writeInt32(haxe.Int32.ofInt(value));
			return 5;
		}
	}
	
	/**
	 * Writes a MessagePack 'double'.
	 * Returns: 9 (bytes written).
	 */
	static inline public function writeDouble(o : Output, value : Float)
	{
		o.writeByte(0xcb);
		o.writeDouble(value);
		return 9;
	}
	
	/**
	 * Writes a MessagePack Array header.
	 * After calling this, write the array elements.
	 * Returns: bytes written.
	 */
	static public function writeArrayHeader(o : Output, arrayLength : Int)
	{
		Assert.that(arrayLength > 0);
		if (arrayLength <= 15) {
			o.writeByte(0x90 | arrayLength);
			return 1;
		}
		else if (arrayLength < 0x10000) {
			o.writeByte(0xdc);
			o.writeUInt16(arrayLength);
			return 3;
		}
		else {
			o.writeByte(0xdd);
			o.writeInt32(haxe.Int32.ofInt(arrayLength));
			return 5;
		}
	}
	
	static public function writeInt(o : Output, bytes:Int, value:Int) switch (bytes) {
		case 1: o.writeByte(value);
		case 2: o.writeInt16(value);
		case 3: o.writeInt24(value);
		case 4: o.writeInt32(value);
		
		default: Assert.that(false);
	}
	
	static public function intSize(v : Int)
	 	return if (v &     0xFF == v) 1
		  else if (v &   0xFFFF == v) 2
		  else if (v & 0xFFFFFF == v) 3
		  else 4
	
	
	
	/**
	 *  Builtin VO value type:
	 *  +-byte-+---bits---+-------------------+
	 *  | 0xD7 | 1XXXXXXX | ... type data ... |
	 *  +------+----------+-------------------+
	 *  Type id: 0 - 127, per type different length
	 *  
	 *  Custom VO Type:
	 *  +-byte-+---bits---+-------------------+-----------------------+
	 *  | 0xD7 | 0stttfff | 1..n TypeID bytes | 1..n fields-set bytes |
	 *  +------+----------+-------------------+-----------------------+
	 *    s: 1 == object-start: all (non-starting) custom vo types following the last field are part of this object ('supertypes) until object-end
	 *  ttt: Type id		 				length: 1 - 16 bytes  (all bits 0 == 1 byte)
	 *  fff: Field bitflags					length: 0 - 15 bytes
	*/
	static public function writeValueObjectHeader(o : Output, voType : Int, objectStart : Bool, fieldsSet : Int32, ?fieldsSet2 : Int32 = 0)
	{
		var typeBytes  = intSize(voType);
		var fieldBytes = if (fieldsSet2 != 0) intSize(fieldsSet2) + 4 else intSize(fieldsSet);
		
		o.writeByte(0xd7);
		o.writeByte(objectStart? 0x40 : 0 | ((typeBytes << 3) | fieldBytes);
		
		writeInt(o, typeBytes, voType);
		
		if (fieldBytes <= 4)
			writeInt(o, fieldBytes, fieldsSet);
		if (fieldBytes >  4) {
			o.writeInt32(fieldsSet);
			writeInt(o, intSize(fieldsSet2), fieldsSet2);
		}
		
		// 0xD7 | typeID | supertype-count | fieldsSet | [ supertype 1 .. n: typeID+fieldsSet length | typeID | fieldsSet | supertypes field 1 .. n ] | fields x .. n
		// 0xD7 | typeID byteCount + supertype byteCount + fieldsSet byteCount | typeID [| supertype count] [| fieldsSet] | [| supertype 1..n: typeID | 0 | fieldsSet-bytes | fieldsSet | value 1..n] | fieldsSet-bytes | fieldsSet | value x..n
		/*
		 *  +-byte-+--8-bits---+---------------------+-[--------------]-+-[--------------------+--------------------]-+
		 *  | 0xD7 | tsss ffff | 1..t+1 TypeID bytes | [supertype data] | 0..f fields-set data | per set field values |
		 *  +------+-----------+---------------------+-[--------------]-+-[--------------------+--------------------]-+
		 *  TypeID: 1-2 bytes. (Max 65536 unique types supported - in the TypeID bytes)
		 *  Supertype count: 0-7 - per supertype should be a header in "supertype data"
		 *  Fields-set byte count: 0-15 bytes (Max 120 field flags supported per type)
		 *    When more then 4 bytes are used for fiels-set flags, field data is grouped per 32:  32 flags, 32 values, ...remainder
		 */
		return 2 + typeBytes + fieldBytes;
	}
	
	
	/**
	 * 0xd8
	 */
	static public function writeBuiltin(o : Output)
	{
		o.writeByte(0xD8);
		return 1;
	}
	
	/**
	 * Writes a MessagePack 'nil'.
	 * Returns: 1 (bytes written).
	 */
	static public inline function writeNil(o : Output) : Int {
		o.writeByte(0xc0);
		return 1;
	}
	
	/**
	 * Writes a MessagePack boolean.
	 * Returns: 1 (bytes written).
	 */
	static public inline function writeBool(o : Output, value : Bool) : Int {
		o.writeByte(if (value) 0xc3 else 0xc2);
		return 1;
	}
}
