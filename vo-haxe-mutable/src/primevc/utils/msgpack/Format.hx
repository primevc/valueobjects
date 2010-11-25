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
			if (value >= -32) {
				o.writeByte(0xE0 | (-1 - value));
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
	
	static public function packString(o : Output, value : String) : Int
	{
		var b;
		
		if (value.length <= 31) {
			b = 1;
			o.writeByte(0xA0 | value.length);
		}
		else if (value.length <= 65535) {
			b = 3;
			o.writeByte(0xda);
			o.writeUInt16(value.length);
		}
		else {
			b = 5;
			o.writeByte(0xdb);
			o.writeUInt30(value.length);
		}
		o.writeString(value);
		
		return b + value.length;
	}
	
	/**
	 * Writes a MessagePack 'double'.
	 * Returns: 9 (bytes written).
	 */
	static inline public function packDouble(o : Output, value : Float)
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
	static public function packArrayHeader(o : Output, arrayLength : Int)
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
		case 4: o.writeInt32(haxe.Int32.ofInt(value));
		
		default: Assert.that(false);
	}
	
	static public function intSize(v : Int)
	 	return if (v &     0xFF == v) 1
		  else if (v &   0xFFFF == v) 2
		  else if (v & 0xFFFFFF == v) 3
		  else 4
	
	
	
	/**
	 *  Custom VO Type:
	 *  +-byte-+-----------+
	 *  | 0xD7 | VO Header |
	 *  +------+-----------+
	 *  
	 *  VO Header:
	 *  +---bits---+---------------------+----------------------------------+------------------------------------+
	 *  | tsssffff | 1 or 2 TypeID bytes | 0..s supertype VO Headers + data | 0..f Field bitflags + values group |
	 *  +----------+---------------------+----------------------------------+------------------------------------+
	 *     t: When set, TypeID data is ushort (2 bytes, max 65535) otherwise ubyte (max 255).
	 *   sss: Number of super type "VO Header, Field bitflags + values group" combinations following the TypeID.
	 *  ffff: 'Field bitflags + values group' count: 0 - 15 bytes.
	 *  
	 *  Field bitflags + values group
	 *  +----byte----+-----------------------------+
	 *  | 7654  3210 | VOMsgPack value per set bit |
	 *  +------------+-----------------------------+
	 *  Each bit in the first byte indicates whether a value is available.
	 *  The deserializer keeps track of the group number. 
	 *  
	 *  For example: bits "1000 0100" indicate:
	 *  - there are 2 Values following
	 *  - first value is for field index: $groupnumber + 2
	 *  - second value for field index:   $groupnumber + 7
	 */
	static inline public function packValueObjectHeader(o : Output, voType : Int, superTypes : Int, fieldFlagBytes : Int)
	{
		Assert.that(fieldFlagBytes >= 0 && fieldFlagBytes <= 15);
		Assert.that(superTypes >= 0 && superTypes <= 7);
		
	//	var h = 0xD700 /* 0xd7 << 8 */ | superTypes << 4 | fieldFlagBytes;
		
		if (voType <= 255) {
			o.writeUInt24((0xD700 /* 0xd7 << 8 */ | superTypes << 4 | fieldFlagBytes) << 8 | voType); // hopefully optimized by haXe
			return 3;
		}
		else {
			o.writeUInt16(0xD700 /* 0xd7 << 8 */ | 0x8000 | superTypes << 4 | fieldFlagBytes); // write header in one go
			o.writeUInt16(voType);
			return 4;
		}
	}
	
	
	/**
	 * 0xd8
	 */
	static public function packBuiltin(o : Output)
	{
		o.writeByte(0xD8);
		return 1;
	}
	
	/**
	 * Writes a MessagePack 'nil'.
	 * Returns: 1 (bytes written).
	 */
	static public inline function packNil(o : Output) : Int {
		o.writeByte(0xc0);
		return 1;
	}
	
	/**
	 * Writes a MessagePack boolean.
	 * Returns: 1 (bytes written).
	 */
	static public inline function packBool(o : Output, value : Bool) : Int {
		o.writeByte(if (value) 0xc3 else 0xc2);
		return 1;
	}
}
