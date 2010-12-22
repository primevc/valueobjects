package primevc.utils.msgpack;
 import haxe.io.Input;
 import haxe.io.Eof;
 import primevc.core.traits.IValueObject;
 import primevc.utils.FastArray;
 import primevc.tools.valueobjects.ValueObjectBase;
 import primevc.types.RGBA;
 import primevc.types.EMail;
 import primevc.types.URI;
 import primevc.types.FileRef;
 import primevc.types.DateInterval;
 import primevc.types.ObjectId;
 import primevc.utils.IfUtil;
  using primevc.utils.IfUtil;

typedef ValueConverter = Dynamic -> PropertyID -> Dynamic -> Dynamic

/**
 * MessagePack stream reader
 * @author Danny Wilson
 * @creation-date nov 22, 2010
 */
class Reader
{
	public var input (default, null) : Input;
	
	private var context : IntHash<Class<Dynamic>>;
	
	public function new(input_ : Input, context_ : IntHash<Class<Dynamic>>)
	{
		this.input = input_;
		this.context = context_;
	}
	
	
	public function readMsgPackValue(?pid : PropertyID, ?itemType : Dynamic) : Dynamic
	{
		var value;
		try {
			value = readValue(input.readByte(), pid, itemType);
			if (IfUtil.notNull(itemType) && !Std.is(value, itemType))
				value = converter(value, pid, itemType);
		}
		catch (e:Eof)
		 	value = null;
		
		return value;
	}
	
	
	public function readMsgPackArray<T>(pid : PropertyID, itemType : Dynamic) : FastArray<T>
	{
		var i = input, b, arr : FastArray<T>;
		
		switch (b = i.readByte())
		{
			case 0xdc, 0xdd:
				var len = b == 0xdc? i.readUInt16() : i.readUInt30();
				arr = readArray(len, pid, itemType);
			
			default:
				arr = FastArrayUtil.create(1);
				var value = readValue(b, pid, itemType);
			 	arr[0] = Std.is(value, itemType)? value : converter(value, pid, itemType);
		}
		
		return arr;
	}
	
	
	//
	// Privates
	//
	
	private function converter(value : Dynamic, propertyID : PropertyID, typeClass : Dynamic) : Dynamic
	{
		switch (typeClass) {
			case FileRef:	return new FileRef(Std.string(value));
			case URI:		return new URI(Std.string(value));
			case Date:		return Date.fromTime(value);
//			case ObjectId:	
		}
		
		return value;
	}
	
	
	private function readValue(b : Int, pid : PropertyID, itemType : Dynamic) : Dynamic switch (b)
	{
		case /* uint8  */		0xcc:	#if MessagePackDebug_Read trace("readValue (uint8)"); #end	return input.readByte();
		case /* uint16 */		0xcd:	#if MessagePackDebug_Read trace("readValue (uint16)"); #end	return input.readUInt16();
		case /* uint32 */		0xce:	#if MessagePackDebug_Read trace("readValue (uint32)"); #end	#if neko return input.readUInt30(); #else var v:UInt = cast input.readInt32(); return v; #end
		case /* uint64 */		0xcf:	#if MessagePackDebug_Read trace("readValue (uint64)"); #end	throw "uint64 not implemented: " + input.read(8);
		
		case /*  int8  */		0xd0:	#if MessagePackDebug_Read trace("readValue (int8)"); #end	return input.readInt8();
		case /*  int16  */		0xd1:	#if MessagePackDebug_Read trace("readValue (int16)"); #end	return input.readInt16();
		case /*  int32  */		0xd2:	#if MessagePackDebug_Read trace("readValue (uint32)"); #end	return input.readInt32();
		case /*  int64  */		0xd3:	throw "int64 not implemented: " + input.read(8);
		
		case /* nil */			0xc0:	#if MessagePackDebug_Read trace("readValue (null)"); #end	return null;
		case /* true */			0xc3:	#if MessagePackDebug_Read trace("readValue (true)"); #end	return true;
		case /* false */		0xc2:	#if MessagePackDebug_Read trace("readValue (false)"); #end	return false;
		
		case /* float */		0xca:	return input.readFloat();
		case /* double */		0xcb:	return input.readDouble();
		
		case /* raw16 */		0xda:	var len = input.readUInt16(); return input.readString(len);
		case /* raw32 */		0xdb:	var len = input.readUInt30(); return input.readString(len);
		
		case /* array 16 */		0xdc:	return readArray(input.readUInt16(), pid, itemType);
		case /* array 32 */		0xdd:	return readArray(input.readUInt30(), pid, itemType);
		
		case /* map 16 */		0xde:	return readMap(input.readUInt16(), pid, itemType);
		case /* map 32 */		0xdf:	return readMap(input.readUInt30(), pid, itemType);
		
		case /* ValueObject */	0xd7:
			var header = input.readByte();
			if ((header & 0x80).not0())
				return deserializeVO(header);
			else
			 	return deserializeValue(header);
		
		default:
			     if (b & 0x80 == 0x00){ #if MessagePackDebug_Read trace("readValue, pos-int: "+0x7F); #end				return b & 0x7F;						 } // fixed positive int
			else if (b & 0xE0 == 0xE0){ #if MessagePackDebug_Read trace("readValue, neg-int: "+(-1 - (b & 31))); #end	return -1 - (b & 31);					 } // fixed negative int
			else if (b & 0xE0 == 0xA0){ #if MessagePackDebug_Read trace("readValue,  string: "+(b & 31)); #end			return input.readString(b & 31);		 } // fixed raw
			else if (b & 0xF0 == 0x90){ #if MessagePackDebug_Read trace("readValue:   array: "+(b & 15)); #end			return readArray(b & 15, pid, itemType); } // fixed array
			else if (b & 0xF0 == 0x80){ #if MessagePackDebug_Read trace("readValue:     map: "+(b & 15)); #end			return readMap  (b & 15, pid, itemType); } // fixed map
			else
			 	throw "unknown type: " + StringTools.hex(b, 2);
	}
	
	function readArray<T>(len:Int, pid : PropertyID, itemType : Dynamic) : FastArray<T>
	{
		var arr = FastArrayUtil.create(len);
		for (i in 0 ... len)
		 	arr[i] = readMsgPackValue(pid, itemType);
		
		return arr;
	}
	
	function readMap(elem:Int, pid : PropertyID, itemType : Dynamic)
	{
		var map:Hash<Dynamic> = new Hash();
		
		for (i in 0 ... elem)
		{
			var key:String = readMsgPackValue(pid, itemType);
			Assert.that(Std.is(key,String));
			
			map.set(key, readMsgPackValue(pid, itemType));
		}
		
		return map;
	}
	
/*	function readUInt(i : Input, bytes : Int) return switch (bytes)
	{
		case 1: input.readByte();
		case 2: input.readUInt16();
		case 3: input.readUInt24();
		case 4: #if neko input.readUInt30(); #else cast input.readInt32(); #end
		default: 0;
	}
*/	
	function deserializeValue(type : Int) : Dynamic
	{
		#if MessagePackDebug_Read
			trace("deserializeValue { typeID: "+ type);
		#end
		
		switch (type)
		{
			case ObjectId.TYPE_ID:
				return ObjectId.fromBytes(input);
				
			case DateInterval.TYPE_ID:
				var start = readMsgPackValue(0, Date);
				var end   = readMsgPackValue(0, Date);
				Assert.that(Std.is(start, Date));
				Assert.that(Std.is(end,   Date));
				return new DateInterval(start, end);
		
			default: throw "Unknown VO value-type ID: "+type;
		}
	}
	
	function deserializeVO(voHeader : Int, target : IValueObject = null)
	{
		var inp = this.input;
		var superTypeCount = (voHeader & 0x38 /* 0b_0011_1000 */) >> 3;
		var fieldsSetBytes =  voHeader & 0x07 /* 0b_0000_0111 */;
		
		var typeID = if ((voHeader & 0x40 /* 0b_0100_0000 */).not0())
			inp.readUInt16(); // 2 typeID bytes
		else
			inp.readByte();   // 1 typeID byte
		
		#if MessagePackDebug_Read
			trace("deserializeVO { typeID: "+ typeID + ", superTypeCount: "+ superTypeCount + ", fieldsSetBytes: " + fieldsSetBytes);
		#end
		
		var clazz = this.context.get(typeID);
		Assert.notNull(clazz, "voHeader: " + StringTools.hex(typeID, 2) + ", type: " + typeID + " not found...");
		
		if (target == null) {
			target = Type.createInstance(clazz, []);
			Assert.notNull(target);
		}
		
		while (superTypeCount-->0)
			deserializeVO(inp.readByte(), target);
		
		if (fieldsSetBytes != 0)
			(untyped clazz).msgpack_unpackVO(this, target, fieldsSetBytes, this.converter);
		
		return target; // done
	}
	
	public function discardRemainingVOProperties(propertyBytes : Int)
	{
		var inp = this.input;
		while (propertyBytes-->0) {
			var bits = inp.readByte();
			for (bit in 0 ... 8) if ((bits & (1 << bit)).not0())
				readMsgPackValue();
		}
	}
}

