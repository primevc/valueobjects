package prime.utils.msgpack;
 import haxe.io.Input;
 import haxe.io.Eof;
 import prime.core.traits.IValueObject;
 import prime.utils.FastArray;
 import prime.tools.valueobjects.PropertyID;
 import prime.types.RGBA;
 import prime.types.RGBAType;
 import prime.types.EMail;
 import prime.types.URI;
 import prime.types.URL;
 import prime.types.FileRef;
 import prime.types.DateInterval;
 import prime.types.ObjectId;
 import prime.utils.IfUtil;
  using prime.utils.IfUtil;
  using Std;

typedef ValueConverter = Dynamic -> PropertyID -> Dynamic -> Dynamic

/**
 * MessagePack stream reader
 * @author Danny Wilson
 * @creation-date nov 22, 2010
 */
class Reader implements prime.core.traits.IDisposable
{
#if MessagePackDebug_Read
    public var verbose : Bool;
#end

#if flash10
    public var bytes(default,setBytes) : flash.utils.ByteArray;

    private function setBytes(b : flash.utils.ByteArray)
    {
        Assert.equal(b.endian, flash.utils.Endian.BIG_ENDIAN, "MessagePack integers must be read big-endian.");
        
        b.position = 0;
		if (b.length < 1024)
			b.length = 1024;
		flash.Memory.select(b);
		
		return this.bytes = b;
    }

    private var addr : Int;

#else
	public var input : Input;
#end

	private var context	: Map<Int,Class<Dynamic>>;
	
	public function new(context_ : Map<Int,Class<Dynamic>>, ?input_ : Input)
	{
		Assert.notNull(context_);
		this.context = context_;
	#if flash10
	    if (input_ != null)
	        this.bytes = input_.is(haxe.io.BytesInput) ? (untyped input_).b : input_.readAll().getData();
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
		
		var value = null;
		try {
			value = readValue(readByte(), pid, itemType);
			if (IfUtil.notNull(itemType) && !(#if flash9 untyped __is__(value, itemType) || #end Std.is(value, itemType)))
				value = converter(value, pid, itemType);
		}
		catch (e:Eof) {}
		// 	value = null;
		
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
			case 0xdd:	readInt32();
			default:
				if (b & 0xF0 == 0x90) b & 15;
				else 1;
		}
	}
	
	
	private function converter(value : Dynamic, propertyID : PropertyID, typeClass : Dynamic) : Dynamic
	{
		switch (typeClass) {
			case FileRef:	return new FileRef(Std.string(value));
            case URL:       return new URL(Std.string(value));
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
			trace("Could not convert: " + value + ", to: " + typeClass);// + " - stack: " + haxe.CallStack.callStack().join("\n"));
			return null;
		}
		
		return value;
	}
	
	
	private function readValue(b : Int, pid : PropertyID, itemType : Dynamic) : Dynamic switch (b)
	{
		case /* uint8  */		0xcc:	#if MessagePackDebug_Read if (verbose) trace("readValue (uint8)"); #end      return readByte();
		case /* uint16 */		0xcd:	#if MessagePackDebug_Read if (verbose) trace("readValue (uint16)"); #end     return readUInt16();
		case /* uint32 */		0xce:	#if MessagePackDebug_Read if (verbose) trace("readValue (uint32)"); #end     #if neko return readInt32(); #else var v:UInt = cast readInt32(); return v; #end
		case /* uint64 */		0xcf:	#if MessagePackDebug_Read if (verbose) trace("readValue (uint64)"); #end     throw "uint64 not implemented: " + read(8);
		
		case /*  int8  */		0xd0:	#if MessagePackDebug_Read if (verbose) trace("readValue (int8)"); #end       return readInt8();
		case /*  int16  */		0xd1:	#if MessagePackDebug_Read if (verbose) trace("readValue (int16)"); #end      return readInt16();
		case /*  int32  */		0xd2:	#if MessagePackDebug_Read if (verbose) trace("readValue (uint32)"); #end	 return readInt32();
		case /*  int64  */		0xd3:	throw "int64 not implemented: " + read(8);
		
		case /* nil */			0xc0:	#if MessagePackDebug_Read if (verbose) trace("readValue (null)"); #end       return null;
		case /* true */			0xc3:	#if MessagePackDebug_Read if (verbose) trace("readValue (true)"); #end       return true;
		case /* false */		0xc2:	#if MessagePackDebug_Read if (verbose) trace("readValue (false)"); #end      return false;
		
		case /* float */		0xca:	return readFloat();
		case /* double */		0xcb:	return readDouble();
		
		case /* raw16 */		0xda:	var len = readUInt16(); return readString(len);
		case /* raw32 */		0xdb:	var len = readInt32(); return readString(len);
		
		case /* array 16 */		0xdc:	return readArray(readUInt16(), pid, itemType);
		case /* array 32 */		0xdd:	return readArray(readInt32(), pid, itemType);
		
		case /* map 16 */		0xde:	return readMap(readUInt16(), pid, itemType);
		case /* map 32 */		0xdf:	return readMap(readInt32(), pid, itemType);
		
		case /* ValueObject */	0xd7:
			var header = readByte();
			if ((header & 0x80).not0())
				return deserializeVO(header);
			else
			 	return deserializeValue(header);
		
		default:
			     if (b & 0x80 == 0x00){ #if MessagePackDebug_Read if (verbose) trace("readValue, b: "+b+", pos-int: "+(b & 0x7F)); #end	    	return b & 0x7F;						 } // fixed positive int
			else if (b & 0xE0 == 0xE0){ #if MessagePackDebug_Read if (verbose) trace("readValue, b: "+b+", neg-int: "+(-1 - (b & 31))); #end	return -1 - (b & 31);					 } // fixed negative int
			else if (b & 0xE0 == 0xA0){ #if MessagePackDebug_Read if (verbose) trace("readValue, b: "+b+",  string: "+(b & 31)); #end			return readString(b & 31);				 } // fixed raw
			else if (b & 0xF0 == 0x90){ #if MessagePackDebug_Read if (verbose) trace("readValue, b: "+b+",   array: "+(b & 15)); #end			return readArray(b & 15, pid, itemType); } // fixed array
			else if (b & 0xF0 == 0x80){ #if MessagePackDebug_Read if (verbose) trace("readValue, b: "+b+",     map: "+(b & 15)); #end			return readMap  (b & 15, pid, itemType); } // fixed map
			else
			 	throw "unknown type: " + StringTools.hex(b, 2);
	}
	
	
	private inline function readArray<T>(len:Int, pid : PropertyID, itemType : Dynamic) : FastArray<T>
	{
		var arr = FastArrayUtil.create(len);
		for (i in 0 ... len)
		 	arr[i] = readMsgPackValue(pid, itemType);
		
		return arr;
	}

	
	private function readMap(elem:Int, pid : PropertyID, itemType : Dynamic)
	{
		var map:Map<String,Dynamic> = new Map();
		
		for (i in 0 ... elem)
		{
			var key:String = readMsgPackValue(pid, itemType);
			Assert.that(Std.is(key,String));
			
			map.set(key, readMsgPackValue(pid, itemType));
		}
		
		return map;
	}
	
	
	private function deserializeValue(type : Int) : Dynamic
	{
#if MessagePackDebug_Read if (verbose)
		trace("deserializeValue { typeID: "+ type);
#end
		switch (type)
		{
			case ObjectId.TYPE_ID:
			#if flash10
			    var a = addr;
			    addr += 12;
			    return ObjectId.fromMemory(a);
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
	
	
	private function deserializeVO(voHeader : Int)
	{
		var target : Dynamic = null;
		var superTypeCount = 0;
		do {
			superTypeCount += (voHeader & 0x38 /* 0b_0011_1000 */) >>> 3;
			var fieldsSetBytes =  voHeader & 0x07 /* 0b_0000_0111 */;
		
			var typeID = if ((voHeader & 0x40 /* 0b_0100_0000 */).not0())
				readUInt16(); // 2 typeID bytes
			else
				readByte();   // 1 typeID byte
		
#if MessagePackDebug_Read if (verbose)
				trace("deserializeVO { typeID: "+ typeID + ", superTypeCount: "+ superTypeCount + ", fieldsSetBytes: " + fieldsSetBytes + ", target: "+target);
#end
		    var clazz = this.context.get(typeID);
#if debug	Assert.notNull(clazz, "voHeader: " + StringTools.hex(typeID, 2) + ", type: " + typeID + " not found..."); #end
		
			if (target == null) {
#if MessagePackDebug_Read
					trace("                create Instance: "+ clazz);
#end
				target = Type.createEmptyInstance(clazz);
				Assert.notNull(target);
			//	target.beginEdit();
			}
		
#if debug
			var clazzName = Type.getClassName(clazz);
			var lastDot = clazzName.lastIndexOf('.');
		
			var interfaze = Type.resolveClass(
				clazzName.substr(0, lastDot) + ".I" + clazzName.substr(lastDot+1)
			);
			Assert.that(Std.is(target, interfaze), target +" is not a "+ interfaze + " ; voHeader: 0x"+StringTools.hex(voHeader) + ", typeID: "+ typeID + ", superTypeCount: "+ superTypeCount + ", fieldsSetBytes: " + fieldsSetBytes);
#end
			if (fieldsSetBytes != 0)
				(untyped clazz).msgpack_unpackVO(this, target, fieldsSetBytes);
			
			if (superTypeCount > 0)
				voHeader = readByte();
		}
		while (superTypeCount-->0);
	   
	//	untyped target._changedFlags = 0;
	//	target.commitEdit();
        target.init();
		
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

    public inline function readByte()       : Int
    {
        #if MessagePackDebug_Read
            var v:Int = flash.Memory.getByte( addr++ );
        
            bytes.position = addr - 1;
            var b:Int = bytes.readUnsignedByte();
            Assert.equal(b, v, "addr="+addr);
        
            return v;
        #else
            return flash.Memory.getByte( addr++ );
        #end
    }

/*
    // Byte swapping in AS3 seems to be slower then just OR-ing bytes read with alchemy opcodes

    private inline function byteSwapInt (inValue:Int)
    {
    	return  ((inValue & 0xff000000) >> 24) |
				((inValue & 0x00ff0000) >>  8) |
				((inValue & 0x0000ff00) <<  8) |
				((inValue & 0x000000ff) << 24) ;
    }
    
    private inline function byteSwapShort (inValue:Int)
    {
    	return  (((inValue >> 8)) | ((inValue & 0xFF) << 8));
    }
*/

    private inline function readUInt16()    : Int
    {
        var a = addr;
        addr += 2;
        
        #if MessagePackDebug_Read       var v =
        #else return                            #end
            flash.Memory.getByte(a)   << 8 |
            flash.Memory.getByte(a+1)      ;
        
        #if MessagePackDebug_Read
            bytes.position = a;
            Assert.equal(bytes.readUnsignedShort(), v, "addr="+a);
            return v;
        #end
    }

    private inline function readInt32()     : Int
    {
        var a = addr;
        addr += 4;
        
        #if MessagePackDebug_Read       var v =
        #else return                            #end
            flash.Memory.getByte(a)   << 24 |
            flash.Memory.getByte(a+1) << 16 |
            flash.Memory.getByte(a+2) <<  8 |
            flash.Memory.getByte(a+3)       ;
        
        #if MessagePackDebug_Read
            bytes.position = a;
            Assert.equal(bytes.readInt(), v);
            return v;
        #end
    }

    private inline function readFloat()     : Float
    {    
        var a = addr;
        addr += 4;
        
        bytes.position = a;
    	return bytes.readFloat();
    }

    private inline function readDouble()    : Float
    {    
        var a = addr;
        addr += 8;
        
        bytes.position = a;
        return bytes.readDouble();
    }
    
    private inline function readString(n)   : String {
        bytes.position = addr;
        addr += n;
        return bytes.readUTFBytes(n);
    }
    
    private inline function read(n)         : Void {
        addr += n;
    }
    
    private inline function readInt8()      : Int
    {
        var n = readByte();
        if( n >= 128 ) n -= 256;

        #if MessagePackDebug_Read
            bytes.position = addr - 1;
            Assert.equal(bytes.readByte(), n);
        #end
        return n;
    }
    
    private inline function readInt16()     : Int
    {
        #if MessagePackDebug_Read
            var n = flash.Memory.signExtend16( readUInt16() );
            bytes.position = addr - 2;
            Assert.equal(bytes.readShort(), n);
            return n;
        #else
            return flash.Memory.signExtend16( readUInt16() );
        #end
    }

#else

	public  inline function readByte()		: Int		return input.readByte();
	private inline function readUInt16()	: Int		return input.readUInt16();
	private inline function readFloat()		: Float		return input.readFloat();
	private inline function readDouble()	: Float		return input.readDouble();

	private inline function readString(n)	: String	return input.readString(n);
	private inline function read(n)						return input.read(n);
	private inline function readInt8()		: Int		return input.readInt8();
	private inline function readInt16()		: Int		return input.readInt16();
	private inline function readInt32()		: Int		return input.readInt32();
	
#end
}

