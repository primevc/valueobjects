package primevc.utils.msgpack;
 import haxe.io.Input;
 import haxe.io.Eof;
 import primevc.core.traits.IDisposable;
 import primevc.core.traits.IValueObject;
 import primevc.utils.FastArray;
 import primevc.tools.valueobjects.ValueObjectBase;
 import primevc.types.RGBA;
 import primevc.types.RGBAType;
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
class Reader implements IDisposable
{
#if MessagePackDebug_Read
    public var verbose : Bool;
#end

#if flash10

#else    
	public var input	: Input;
#end
	private var context	: IntHash<Class<Dynamic>>;
	
	public function new(context_ : IntHash<Class<Dynamic>>, ?input_ : Input)
	{
		Assert.notNull(context_);
		this.context = context_;
	#if flash10
	    this.bytes = untyped input_.b;
		bytes.position = 0;
		if (bytes.length < 1024)
			bytes.length = 1024;
		select(bytes);
	#else
	    this.input = input_;
	#end
	}
	
	
	public function dispose ()
	{
		#if flash10 this.bytes #else this.input #end = null;
		context	= null;
	}
	
	
	public function readMsgPackValue(?pid : PropertyID, ?itemType : Dynamic) : Dynamic
	{
	    #if MessagePackDebug_Read if (verbose) trace("readMsgPackValue(pid: "+pid+", itemType: "+ itemType +")"); #end
		Assert.notNull(#if flash10 this.bytes #else this.input #end);
		
		var value;
		try {
			value = readValue(readByte(), pid, itemType);
			if (IfUtil.notNull(itemType) && !Std.is(value, itemType))
				value = converter(value, pid, itemType);
		}
		catch (e:Eof)
		 	value = null;
		
		return value;
	}
	
	
	public inline function readMsgPackArray<T>(pid : PropertyID, itemType : Dynamic) : FastArray<T>
	{
		return readArray(readArrayLength(), pid, itemType);
	}
	
	
	//
	// Privates
	//
	
	private function readArrayLength() : Int
	{
		var b = readByte();
		
		return switch (b)
		{
			case 0xdc:	readUInt16();
			case 0xdd:	readUInt30();
			default:
				return if (b & 0xF0 == 0x90) b & 15;
				else 1;
		}
	}
	
	
	private function readConvert(packedType : Int, propertyID : PropertyID, typeClass : Dynamic) : Dynamic
	{
		var value = readValue(packedType, propertyID, typeClass);
		return Std.is(value, typeClass)? value : converter(value, propertyID, typeClass);
	}
	
	
	private function converter(value : Dynamic, propertyID : PropertyID, typeClass : Dynamic) : Dynamic
	{
		switch (typeClass) {
			case FileRef:	return new FileRef(Std.string(value));
			case URI:		return new URI(Std.string(value));
			case Date:		return Date.fromTime(value);
			case RGBAType:	return value;
//			case String:	return Std.string(value);
//			case Bool:		return value > 0;
//			case ObjectId:	
		}
		
		try if (Std.is(value, Int) && Type.getEnumName(typeClass) != null)
		{
			var utils:Dynamic = Type.resolveClass(Type.getEnumName(typeClass) + "_utils");
			Assert.that(utils != null, "No converter available for: " + Type.getEnumName(typeClass) + ", enum name: " + Type.getEnumName(typeClass) + ", value: " + value);
			
			var v = utils.fromValue(value);
			Assert.that(v != null, "Converting to Enum instance by '"+ Type.getClassName(utils) +".fromValue("+value+")' failed");
			return v;
		}
		catch (e:Dynamic) {
			Assert.that(false, "Could not convert: " + value + ", to: " + typeClass);// + " - stack: " + haxe.Stack.callStack().join("\n"));
		}
		
		return value;
	}
	
	
	private function readValue(b : Int, pid : PropertyID, itemType : Dynamic) : Dynamic switch (b)
	{
		case /* uint8  */		0xcc:	#if MessagePackDebug_Read if (verbose) trace("readValue (uint8)"); #end	return readByte();
		case /* uint16 */		0xcd:	#if MessagePackDebug_Read if (verbose) trace("readValue (uint16)"); #end	return readUInt16();
		case /* uint32 */		0xce:	#if MessagePackDebug_Read if (verbose) trace("readValue (uint32)"); #end	#if neko return readUInt30(); #else var v:UInt = cast readInt32(); return v; #end
		case /* uint64 */		0xcf:	#if MessagePackDebug_Read if (verbose) trace("readValue (uint64)"); #end	throw "uint64 not implemented: " + read(8);
		
		case /*  int8  */		0xd0:	#if MessagePackDebug_Read if (verbose) trace("readValue (int8)"); #end	return readInt8();
		case /*  int16  */		0xd1:	#if MessagePackDebug_Read if (verbose) trace("readValue (int16)"); #end	return readInt16();
		case /*  int32  */		0xd2:	#if MessagePackDebug_Read if (verbose) trace("readValue (uint32)"); #end	return readInt32();
		case /*  int64  */		0xd3:	throw "int64 not implemented: " + read(8);
		
		case /* nil */			0xc0:	#if MessagePackDebug_Read if (verbose) trace("readValue (null)"); #end	return null;
		case /* true */			0xc3:	#if MessagePackDebug_Read if (verbose) trace("readValue (true)"); #end	return true;
		case /* false */		0xc2:	#if MessagePackDebug_Read if (verbose) trace("readValue (false)"); #end	return false;
		
		case /* float */		0xca:	return readFloat();
		case /* double */		0xcb:	return readDouble();
		
		case /* raw16 */		0xda:	var len = readUInt16(); return readString(len);
		case /* raw32 */		0xdb:	var len = readUInt30(); return readString(len);
		
		case /* array 16 */		0xdc:	return readArray(readUInt16(), pid, itemType);
		case /* array 32 */		0xdd:	return readArray(readUInt30(), pid, itemType);
		
		case /* map 16 */		0xde:	return readMap(readUInt16(), pid, itemType);
		case /* map 32 */		0xdf:	return readMap(readUInt30(), pid, itemType);
		
		case /* ValueObject */	0xd7:
			var header = readByte();
			if ((header & 0x80).not0())
				return deserializeVO(header);
			else
			 	return deserializeValue(header);
		
		default:
			     if (b & 0x80 == 0x00){ #if MessagePackDebug_Read if (verbose) trace("readValue, b: "+b+", pos-int: "+(b & 0x7F)); #end		return b & 0x7F;						 } // fixed positive int
			else if (b & 0xE0 == 0xE0){ #if MessagePackDebug_Read if (verbose) trace("readValue, b: "+b+", neg-int: "+(-1 - (b & 31))); #end	return -1 - (b & 31);					 } // fixed negative int
			else if (b & 0xE0 == 0xA0){ #if MessagePackDebug_Read if (verbose) trace("readValue, b: "+b+",  string: "+(b & 31)); #end			return readString(b & 31);				 } // fixed raw
			else if (b & 0xF0 == 0x90){ #if MessagePackDebug_Read if (verbose) trace("readValue, b: "+b+",   array: "+(b & 15)); #end			return readArray(b & 15, pid, itemType); } // fixed array
			else if (b & 0xF0 == 0x80){ #if MessagePackDebug_Read if (verbose) trace("readValue, b: "+b+",     map: "+(b & 15)); #end			return readMap  (b & 15, pid, itemType); } // fixed map
			else
			 	throw "unknown type: " + StringTools.hex(b, 2);
	}
	
	
	inline function readArray<T>(len:Int, pid : PropertyID, itemType : Dynamic) : FastArray<T>
	{
		var arr = FastArrayUtil.create(len);
		for (i in 0 ... len) //try {
		 	arr[i] = readMsgPackValue(pid, itemType);
//		} catch (e:Dynamic) {
//			throw "Could not unpack array data with property ID: 0x"+StringTools.hex(pid)+", and itemType: "+itemType;
//		}
		
		return arr;
	}/*		
	#if flash10
		var isVO = false, sc = itemType;
		while ((sc = Type.getSuperClass(sc)) != null)
			if (sc == ValueObjectBase) {
				isVO = true;
				break;
			}
		
		if (isVO) return readVOArray(len, pid, itemType);
		else return readObjectArray(len, pid, itemType);
	#else
		return readObjectArray(len, pid, itemType);
	#end
	}
	
	function readObjectArray(len:Int, pid : PropertyID, itemType : Dynamic) : FastArray<Dynamic>
	{
		var arr = FastArrayUtil.create(len);
		for (i in 0 ... len)
		 	arr[i] = readMsgPackValue(pid, itemType);
		
		return arr;
	}
	
	#if flash10
	function readVOArray(len:Int, pid : PropertyID, itemType : Dynamic) : FastArray<IValueObject>
	{
		var arr : FastArray<IValueObject> = FastArrayUtil.create(len);
		for (i in 0 ... len)
		 	arr[i] = readMsgPackValue(pid, itemType);
		
		return arr;
	}
	#end
*/
	
	private function readMap(elem:Int, pid : PropertyID, itemType : Dynamic)
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
	
	private function deserializeValue(type : Int) : Dynamic
	{
#if MessagePackDebug_Read if (verbose)
		trace("deserializeValue { typeID: "+ type);
#end
		switch (type)
		{
			case ObjectId.TYPE_ID:
			#if flash10
			    var oid = ObjectId.fromMemory(addr);
			    addr += 12;
			    return oid;
			#else
			    return ObjectId.fromInput(input);
			#end
				
			case DateInterval.TYPE_ID:
				var start = Date.fromTime(readDouble());
				var end   = Date.fromTime(readDouble());
				Assert.that(Std.is(start, Date));
				Assert.that(Std.is(end,   Date));
				return new DateInterval(start, end);
		
			default: throw "Unknown VO value-type ID: "+type;
		}
	}
	
	
	private function deserializeVO(voHeader : Int, target : ValueObjectBase = null)
	{
		var superTypeCount = (voHeader & 0x38 /* 0b_0011_1000 */) >>> 3;
		var fieldsSetBytes =  voHeader & 0x07 /* 0b_0000_0111 */;
		
		var typeID = if ((voHeader & 0x40 /* 0b_0100_0000 */).not0())
			readUInt16(); // 2 typeID bytes
		else
			readByte();   // 1 typeID byte
		
		#if MessagePackDebug_Read if (verbose)
			trace("deserializeVO { typeID: "+ typeID + ", superTypeCount: "+ superTypeCount + ", fieldsSetBytes: " + fieldsSetBytes + ", target: "+target);
		#end
		
		var clazz = this.context.get(typeID);
		Assert.notNull(clazz, "voHeader: " + StringTools.hex(typeID, 2) + ", type: " + typeID + " not found...");
		
		var targetWasNull = target == null;
		if (targetWasNull) {
			#if MessagePackDebug_Read if (verbose)
				trace("                create Instance: "+ clazz);
			#end
			target = Type.createInstance(clazz, []);
			Assert.notNull(target);
			target.beginEdit();
		}
		
	#if debug
		var clazzName = Type.getClassName(clazz);
		var lastDot = clazzName.lastIndexOf('.');
		
		var interfaze = Type.resolveClass(
			clazzName.substr(0, lastDot) + ".I" + clazzName.substr(lastDot+1)
		);
		Assert.that(Std.is(target, interfaze), target +" is not a "+ interfaze + " ; voHeader: 0x"+StringTools.hex(voHeader) + ", typeID: "+ typeID + ", superTypeCount: "+ superTypeCount + ", fieldsSetBytes: " + fieldsSetBytes);
	#end
		
		if (fieldsSetBytes != 0)// try {
			(untyped clazz).msgpack_unpackVO(this, target, fieldsSetBytes, this.converter);
//		} catch (e:Dynamic) {
//			throw "Could not unpack VO data with typeID: "+typeID+", using: "+clazz;
//		}
		
		while (superTypeCount-->0)
			deserializeVO(readByte(), target);
		
		if (targetWasNull) {
			untyped target._changedFlags = 0;
			target.commitEdit();
		}
		
		return target; // done
	}
	
	
	public function discardRemainingVOProperties(propertyBytes : Int)
	{
//		#if MessagePackDebug_Read if (verbose)
			trace("                discarding propertyBytes: "+ propertyBytes);
//		#end
		
		while (propertyBytes-->0) {
			var bits = readByte();
			for (bit in 0 ... 8) if ((bits & (1 << bit)).not0())
				readMsgPackValue();
		}
	}
	
	
	
#if flash10

	var bytes : flash.utils.ByteArray;
	var addr : Int;
	
	private inline function select( b : flash.utils.ByteArray ) : Void {
		flash.system.ApplicationDomain.currentDomain.domainMemory = b;
	}
#end

#if flash10

	public inline function readByte()		: Int {
	#if debug
		bytes.position = addr;
		Assert.equal(bytes.readUnsignedByte(), untyped __vmem_get__(0,addr));
	#end
		var v = untyped __vmem_get__(0,addr++);
		return v;
	}

	private inline function readUInt16()	: Int {
	#if debug
		bytes.position = addr;
		Assert.equal(bytes.readUnsignedShort(), untyped __vmem_get__(1,addr));
	#end
		var v = untyped __vmem_get__(1,addr);
		addr += 2;
		return v;
	}

	private inline function readInt32()		: Int {
	#if debug
		bytes.position = addr;
		Assert.equal(bytes.readInt(), untyped __vmem_get__(2,addr));
	#end
		var v = untyped __vmem_get__(2,addr);
		addr += 4;
		return v;
	}

	private inline function readFloat()		: Float {
	#if debug
		bytes.position = addr;
		Assert.equal(bytes.readFloat(), untyped __vmem_get__(3,addr));
	#end
		var v = untyped __vmem_get__(3,addr);
		addr += 4;
		return v;
	}

	private inline function readDouble()	: Float {
	#if debug
		bytes.position = addr;
		Assert.equal(bytes.readDouble(), untyped __vmem_get__(4,addr));
	#end
		var v = untyped __vmem_get__(4,addr);
		addr += 8;
		return v;
	}
	
	private inline function readString(n)	: String {
		bytes.position = addr;
		addr += n;
		return bytes.readUTFBytes(n);
	}
	private inline function read(n)			: Void {
		addr += n;
	}
	private inline function readInt8()		: Int {
	    var n = readByte();
		if( n >= 128 ) n -= 256;

    	#if debug
    		bytes.position = addr - 1;
    		Assert.equal(bytes.readByte(), n);
    	#end
		return n;
	}
	private inline function readInt16()		: Int {
	    var n = flash.Memory.signExtend16( readUInt16() );

    	#if debug
    		bytes.position = addr - 2;
    		Assert.equal(bytes.readShort(), n);
    	#end
        
		return n;
	}
	private inline function readUInt30()	: Int {
	#if debug
		bytes.position = addr;
		Assert.equal(bytes.readUnsignedInt(), untyped __vmem_get__(2,addr));
	#end
		return readInt32() & 0x3FFFFFFF;
	}

#else

	public  inline function readByte()		: Int		return input.readByte()
	private inline function readUInt16()	: Int		return input.readUInt16()
	private inline function readInt32()		: Int		return #if neko input.readInt31() #else cast input.readInt32() #end
	private inline function readFloat()		: Float		return input.readFloat()
	private inline function readDouble()	: Float		return input.readDouble()

	private inline function readString(n)	: String	return input.readString(n)
	private inline function read(n)						return input.read(n)
	private inline function readInt8()		: Int		return input.readInt8()
	private inline function readInt16()		: Int		return input.readInt16()
	private inline function readUInt30()	: Int		return input.readUInt30()
	
#end
}

