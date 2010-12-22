package primevc.utils.msgpack;
 import haxe.io.Output;
 import haxe.io.BytesOutput;
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
		#if MessagePackDebug_Pack trace("packInt: "+value); #end
		
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

#if flash9
	/**
	 * Writes 'value' in the most compact MsgPack unsigned integer form possible, to output 'o'.
	 * Returns: bytes written.
	 */
	static public function packUInt(o : Output, value : UInt) : Int
	{
		#if MessagePackDebug_Pack trace("packUInt: "+value); #end
		
		if (value <= 127) {
		 	o.writeByte(value);
			return 1;
		}
		else if (value <= 255) {
			o.writeByte(0xcc);
			o.writeByte(value);
			return 2;
		}
		else if (value < 0x10000) {
			o.writeByte(0xcd);
			o.writeUInt16(value);
			return 3;
		}
		else {
			o.writeByte(0xce);
			o.writeInt32(cast value);
			return 5;
		}
	}
#end

	static public function packString(o : BytesOutput, value : String) : Int
	{
		#if MessagePackDebug_Pack trace("packString: "+value); #end
		
		var b;
		
	#if flash9
		var byteArr = new flash.utils.ByteArray();
		byteArr.length = value.length;
		byteArr.writeUTFBytes(value);
		var valueLength = byteArr.length;
	#else
		var valueLength = value.length;
	#end
		
		if (valueLength <= 31) {
			b = 1;
			o.writeByte(0xA0 | valueLength);
		}
		else if (valueLength <= 65535) {
			b = 3;
			o.writeByte(0xda);
			o.writeUInt16(valueLength);
		}
		else {
			b = 5;
			o.writeByte(0xdb);
			o.writeUInt30(valueLength);
		}
		
	#if flash9	
		o.write(haxe.io.Bytes.ofData(byteArr));
		byteArr.length = 0; // does this release the memory ?
	#else
		o.writeString(value);
	#end
		
		return b + valueLength;
	}
	
	/**
	 * Writes a MessagePack 'double'.
	 * Returns: 9 (bytes written).
	 */
	static inline public function packDouble(o : Output, value : Float)
	{
		#if MessagePackDebug_Pack trace("packDouble: "+value); #end
		
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
		#if MessagePackDebug_Pack trace("packArrayHeader: "+arrayLength); #end
		
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
	
	static public function packMap(o : BytesOutput, map : Hash<Dynamic>)
	{
		#if MessagePackDebug_Pack trace("packMap: "+map); #end
		
		var keyCount = Lambda.count(map);
		Assert.that(keyCount >= 0);
		
		var b;
		
		if (keyCount <= 15) {
			o.writeByte(0x80 | keyCount);
			b = 1;
		}
		else if (keyCount < 0x10000) {
			o.writeByte(0xde);
			o.writeUInt16(keyCount);
			b = 3;
		}
		else {
			o.writeByte(0xdf);
			o.writeInt32(haxe.Int32.ofInt(keyCount));
			b = 5;
		}
		
		for (key in map.keys()) {
			b += packString(o, key);
			b += packDynamic(o, map.get(key));
		}
		
		return b;
	}
	
	static public function packDynamic(o : BytesOutput, value : Dynamic)
	{
		#if MessagePackDebug_Pack trace("packDynamic: "+value); #end
		
		return if (value == null)					packNil(o);
		  else if (value == true || value == false)	packBool(o, value);
		  else if (Std.is(value, Int))				packInt(o, value);
		  else if (Std.is(value, Float))			packDouble(o, value);
		  else if (Std.is(value, String))			packString(o, value);
		  else throw "Don't know how to pack: " + value;
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
	 * Writes a MessagePack 'nil'.
	 * Returns: 1 (bytes written).
	 */
	static public inline function packNil(o : Output) : Int {
		#if MessagePackDebug_Pack trace("packNil"); #end
		
		o.writeByte(0xc0);
		return 1;
	}
	
	/**
	 * Writes a MessagePack boolean.
	 * Returns: 1 (bytes written).
	 */
	static public inline function packBool(o : Output, value : Bool) : Int {
		#if MessagePackDebug_Pack trace("packBool: "+value); #end
		
		o.writeByte(if (value) 0xc3 else 0xc2);
		return 1;
	}
}

import primevc.types.RGBA;
import primevc.types.EMail;
import primevc.types.URI;
import primevc.types.FileRef;
import primevc.types.DateInterval;
import primevc.types.ObjectId;


/**
 * ValueObject MessagePack extensions
 * 
 * @author Danny Wilson
 * @creation-date Dec 14, 2010
 */
class VOFormat
{
	/**
	 *  Custom VO Type:
	 *  +-byte-+-----------+
	 *  | 0xD7 | VO Header |
	 *  +------+-----------+
	 */
	static inline public function packValueObject(o : Output)
	{
		#if MessagePackDebug_Pack trace("packValueObject"); #end
		
		o.writeByte(0xD7);
		return 1;
	}
	
	/**
	 *  
	 *  VO Header:
	 *  +---bits---+---------------------+----------------------------------+------------------------------------+
	 *  | 1tsssfff | 1 or 2 TypeID bytes | 0..s supertype VO Headers + data | 0..f Field bitflags + values group |
	 *  +----------+---------------------+----------------------------------+------------------------------------+
	 *     t: When set, TypeID data is ushort (2 bytes, max 65535) otherwise ubyte (max 255).
	 *   sss: Number of super type "VO Header, Field bitflags + values group" combinations following the TypeID.
	 *   fff: 'Field bitflags + values group' count: 0 - 4 bytes.
	 *  
	 *  
	 *  Field bitflags + values group:
	 *  +----byte----+-----------------------------+
	 *  | 7654  3210 | VOMsgPack value per set bit |
	 *  +------------+-----------------------------+
	 *  Each bit in the first byte indicates whether a value is available.
	 *  The deserializer keeps track of the group number. 
	 *  
	 *  Example: bits "1000 0100" indicate:
	 *  - there are 2 Values following
	 *  - first value is for field index: $groupnumber + 2
	 *  - second value for field index:   $groupnumber + 7
	 */
	static inline public function packValueObjectHeader(o : Output, voType : Int, superTypes : Int, fieldFlagBytes : Int)
	{
		Assert.that(fieldFlagBytes >= 0 && fieldFlagBytes <= 4);
		Assert.that(superTypes     >= 0 && superTypes     <= 7);
		
		if (voType <= 255) {
			#if MessagePackDebug_Pack
			 	trace("packValueObjectHeader { voType: "+ voType + ", superTypes: "+ superTypes + ", fieldFlagBytes: "+fieldFlagBytes +", hex: "+ StringTools.hex(0x80 | superTypes << 3 | fieldFlagBytes, 4));
			#end
			o.writeByte(0x80 | superTypes << 3 | fieldFlagBytes);
			o.writeByte(voType);
		//	o.writeUInt16((superTypes << 11) | (fieldFlagBytes << 8) | voType);
			return 2;
		}
		else {
			#if MessagePackDebug_Pack
				trace("packValueObjectHeader { voType: "+ voType + ", superTypes: "+ superTypes + ", fieldFlagBytes: "+fieldFlagBytes +", hex: "+ StringTools.hex(0xC0 | superTypes << 3 | fieldFlagBytes, 4));
			#end
			o.writeByte(0xC0 | superTypes << 3 | fieldFlagBytes);
			o.writeUInt16(voType);
		//	o.writeUInt24(0x40000 | (superTypes << 19) | (fieldFlagBytes << 16) | voType);
			return 3;
		}
	}
	
	/**
	 *  
	 *  VO valuetype Header:
	 *  +---bits---+-------+
	 *  | 0xxxxxxx | Value |
	 *  +----------+-------+
	 *  x: TypeID of value
	 *  
	 */
	static inline public function packVOValueTypeHeader(o : Output, valueType : Int)
	{
		#if MessagePackDebug_Pack trace("packVOValueTypeHeader: "+valueType);
		switch (valueType)
		{
			case DateInterval.TYPE_ID,
				 ObjectId.TYPE_ID:
			
			default:
			 	throw "What type is '" + valueType + "' ??";
		}
		#end
		Assert.that(valueType <= 127);
		
		packValueObject(o);
		o.writeByte(valueType);
		return 2;
	}
	
	static inline public function packRGBA(o : Output, value : RGBA)
	{
		#if MessagePackDebug_Pack trace("packRGBA: "+value); #end
		
		o.writeByte(0xce);
		
	#if neko
		o.writeByte(value.a);
		o.writeUInt24(value.color);
	#else
		o.writeInt32(Int32.ofInt(value));
	#end
		return 5;
	}
	
	static inline public function packEMail(o : BytesOutput, value : EMail)
	{
		#if MessagePackDebug_Pack trace("packEMail: "+value); #end
		
		return Format.packString(o, value.toString());
	}
	
	static inline public function packURI(o : BytesOutput, value : URI)
	{
		#if MessagePackDebug_Pack trace("packURI: "+value); #end
		
		return Format.packString(o, value.string);
	}
	
	static inline public function packFileRef(o : BytesOutput, value : FileRef)
	{
		#if MessagePackDebug_Pack trace("packFileRef: "+value); #end
		
		return Format.packString(o, value.string);
	}
	
	static inline public function packDateTime(o : Output, value : Date)
	{
		#if MessagePackDebug_Pack trace("packDateTime: "+value + ", float = "+ value.getTime() +", uint = " + Std.int(value.getTime() / 1000)); #end
		
		return Format.packDouble(o, value.getTime());
	}
	
	static inline public function packDate(o : Output, value : Date)
	{
		#if MessagePackDebug_Pack trace("packDate: "+value); #end
		
		return packDateTime(o, value);
	}
	
	static inline public function packDateInterval(o : Output, value : DateInterval)
	{
		#if MessagePackDebug_Pack trace("packDateInterval: "+value); #end
		
		return
			packVOValueTypeHeader(o, DateInterval.TYPE_ID)
		  +	packDate(o, value.start)
		  +	packDate(o, value.end);
	}
	
	
	static inline public function packObjectId(o : Output, value : ObjectId)
	{
		#if MessagePackDebug_Pack trace("packObjectId: "+value); #end
		
		packVOValueTypeHeader(o, ObjectId.TYPE_ID);
		value.writeBytes(o);
		return 13;
	}
}
