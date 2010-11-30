package primevc.utils.msgpack;
 import haxe.io.Input;
 import haxe.io.Eof;
 import primevc.core.traits.IValueObject;
 import primevc.utils.FastArray;

/**
 * MessagePack stream reader
 * @author Danny Wilson
 * @creation-date nov 22, 2010
 */
class Reader
{
	public function new() {}
	
	public function readMsgPackValue(i : Input) : Dynamic
	{
//		Assert.that(i.bigEndian);
		
		var b = 0;
		
		while (true) try switch (b = i.readByte())
		{
			case /* uint8  */		0xcc:	return i.readByte();
			case /* uint16 */		0xcd:	return i.readUInt16();
			case /* uint32 */		0xce:	#if neko return i.readUInt30(); #else var v:UInt = cast i.readInt32(); return v; #end
			case /* uint64 */		0xcf:	throw "uint64 not implemented: " + i.read(8);
			
			case /*  int8  */		0xd0:	return i.readInt8();
			case /*  int16  */		0xd1:	return i.readInt16();
			case /*  int32  */		0xd2:	return i.readInt32();
			case /*  int32  */		0xd3:	throw "int64 not implemented: " + i.read(8);
			
			case /* nil */			0xc0:	return null;
			case /* true */			0xc3:	return true;
			case /* false */		0xc2:	return false;
			
			case /* float */		0xca:	return i.readFloat();
			case /* double */		0xcb:	return i.readDouble();
			
			case /* raw16 */		0xda:	var len = i.readUInt16(); return i.readString(len);
			case /* raw32 */		0xdb:	var len = i.readUInt30(); return i.readString(len);
			
			case /* array 16 */		0xdc:	return readArray(i, i.readUInt16());
			case /* array 32 */		0xdd:	return readArray(i, i.readUInt30());
			
			case /* map 16 */		0xde:	return readMap(i, i.readUInt16());
			case /* map 32 */		0xdf:	return readMap(i, i.readUInt30());
			
			case /* ValueObject */	0xd7:	return deserializeVO(i);
			case /* Custom value */	0xd8:	return deserializeValue(i);
			
			default:
				     if (b & 0x80 == 0x00)	return b & 0x7F;				// fixed positive int
				else if (b & 0xE0 == 0xE0)	return -1 - (b & 31);			// fixed negative int
				else if (b & 0xE0 == 0xA0)	return i.readString(b & 31);	// fixed raw
				else if (b & 0xF0 == 0x90)	return readArray(i, b & 15);	// fixed array
				else if (b & 0xF0 == 0x80)	return readMap  (i, b & 15);	// fixed map
				else
				 	throw "unknown type: " + StringTools.hex(b, 2);
		}
		catch (e:Eof) {}
		return null;
	}
	
	function readArray(input : Input, len:Int)
	{
		var arr = FastArrayUtil.create(len);
		for (i in 0 ... len)
		 	arr[i] = readMsgPackValue(input);
		
		return arr;
	}
	
	function readMap(input : Input, elem:Int)
	{
		var map:Hash<Dynamic> = new Hash();
		
		for (i in 0 ... elem)
		{
			var key:String = readMsgPackValue(input);
			Assert.that(Std.is(key,String));
			
			map.set(key, readMsgPackValue(input));
		}
		
		return map;
	}
	
	static public function readUInt(i : Input, bytes : Int) return switch (bytes)
	{
		case 1: i.readByte();
		case 2: i.readUInt16();
		case 3: i.readUInt24();
		case 4: #if neko i.readUInt30(); #else cast i.readInt32(); #end
		default: 0;
	}
	
	function deserializeValue(i : Input)
	{
		var valueType = i.readByte();
		return //HelperClass.getTypeHelper(valueType).readFromInput(i);
		null;
	}
	
	function deserializeVO(i : Input, target : IValueObject = null)
	{
		var voHeader = i.readByte();
		var superTypeCount = (voHeader & 0x70 /* 0b_0111_0000 */) >> 4;
		var fieldsSetBytes =  voHeader & 0x0F /* 0b_0000_1111 */;
		
		var typeID = if (voHeader & 0x80 /* 0b_1000_0000 */ == 0)
			i.readByte();   // 1 typeID byte
		else
			i.readUInt16(); // 2 typeID bytes
		
		if (target == null) {
			//target = TypeMap.createInstance(typeID);
			Assert.that(target != null);
		}
		
		while (--superTypeCount != 0)
		{
			deserializeVO(i, target);
		}
		
		if (fieldsSetBytes != 0)
		{
			var fieldIndex = 0;
		//	var helper = HelperClass.getTypeHelper(typeID);
			
			do {
				var fieldsSet = i.readByte();
				for (bit in 0 ... 8)
				{
					if (fieldsSet & 1 << bit != 0) {
						var value = readMsgPackValue(i);
		//				helper.setField(target, fieldIndex, value);
					}
					++fieldIndex;
				}
			}
			while (--fieldsSetBytes != 0);
		}
		
		return target; // done
	}
}
