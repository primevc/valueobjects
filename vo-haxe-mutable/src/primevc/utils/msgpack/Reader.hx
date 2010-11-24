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
	static public function readMsgPackValue(i : Input) : Dynamic
	{
		Assert.that(i.bigEndian);
		
		var b = 0;
		
		while (true) try return switch (b = i.readByte())
		{
			case /* uint8  */		0xcc:	i.readByte();
			case /* uint16 */		0xcd:	i.readUInt16();
			case /* uint32 */		0xce:	#if neko i.readUInt30(); #else var v:UInt = cast i.readInt32(); #end
			case /* uint64 */		0xcf:	throw "uint64 not implemented: " + b.read(8); null;
			
			case /*  int8  */		0xd0:	i.readInt8();
			case /*  int16  */		0xd1:	i.readInt16();
			case /*  int32  */		0xd2:	i.readInt32();
			case /*  int32  */		0xd3:	throw "int64 not implemented: " + b.read(8); null;
			
			case /* nil */			0xc0:	null;
			case /* true */			0xc3:	true;
			case /* false */		0xc2:	false;
			
			case /* float */		0xca:	i.readFloat();
			case /* double */		0xcb:	i.readDouble();
			
			case /* raw16 */		0xda:	var len = i.readUInt16(); i.readString(len);
			case /* raw32 */		0xdb:	var len = i.readUInt30(); i.readString(len);
			
			case /* array 16 */		0xdc:	readArray(i, i.readUInt16());
			case /* array 32 */		0xdd:	readArray(i, i.readUInt30());
			
			case /* map 16 */		0xde:	readMap(i, i.readUInt16());
			case /* map 32 */		0xdf:	readMap(i, i.readUInt30());
			
			case /* ValueObject */	0xd7:	deserializeVO(i);
			case /* Custom value */	0xd8:	deserializeValue(i);
			
			default:
				     if (b & 0x80 == 0x00)	b & 0x7F;				// fixed positive int
				else if (b & 0xE0 == 0xE0)	-(b & 31) - 1;			// fixed negative int
				else if (b & 0xE0 == 0xA0)	i.readString(b & 31);	// fixed raw
				else if (b & 0xF0 == 0x90)	readArray(i, b & 15);	// fixed array
				else if (b & 0xF0 == 0x80)	readMap  (i, b & 15);	// fixed map
				else
				 	throw "unknown type: " + StringTools.hex(b, 2);
		}
		catch (e:Eof) {}
	}
	
	function readArray(input : Input, len:Int)
	{
		var arr = FastArray.create(len, true);
		for (i in 0 ... len)
		 	arr[i] = readMsgPackValue(input);
		
		return arr;
	}
	
	function readMap(input : Input, elem:Int)
	{
		var map:Hash<Dynamic> = new Hash();
		
		for (i in 0 .. elem)
		{
			var key:String = readMsgPackValue();
			Assert.that(Std.is(key,String));
			
			map.set(key, readMsgPackValue());
		}
		
		return map;
	}
	
	static public function readUInt(i : Input, bytes : Int) return switch (bytes)
	{
		case 1: i.readByte();
		case 2: i.readUInt16();
		case 3: i.readUInt24();
		case 4: i.readInt32();
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
		
		while (--superTypes != 0)
		{
			deserializeVO(i, target);
		}
		
		if (fieldsSetBytes != 0)
		{
			var fieldIndex = 0;
		//	var helper = HelperClass.getTypeHelper(typeID);
			
			do {
				fieldsSet = i.readByte();
				for (bit in 0 ... 8)
				{
					if (fieldsSet & 1 << bit != 0) {
						var value = readMsgPackValue();
		//				helper.setField(target, fieldIndex, value);
					}
					++fieldIndex;
				}
			}
			while (--fieldsSetBytes != 0);
		}
		
		return target; // done
		
		
	/*
	
	if (fieldsSetBytes != 0)
	{
		#if neko do boom! Need implementation... #end
		
		
		var fieldSetBuf = haxe
		var fieldFlags = i.read(fieldsSetBytes);
		
		var fieldIndex = 0, fieldsSet = i.readByte(), bits = 8;
		
		while (bits-->0) {
			var value = readStream();
			//HelperClass.getTypeHelper(typeID).setField(target, fieldIndex, value)
			++fieldIndex;
		}
	}
	
	var fieldsSet_1 = 0, fieldsSet_2 = 0, fieldsSet_3 = 0, fieldsSet_4 = 0; // 120 bits fits in 128 bits :-)
	
	fieldsSet_1 = if (fieldsSetBytes >= 4) i.readInt32() else readInt(i, fieldsSetBytes);
	if (fieldsSetBytes >=  5) fieldsSet_2 = readInt(i, fieldsSetBytes -  4);
	if (fieldsSetBytes >=  9) fieldsSet_3 = readInt(i, fieldsSetBytes -  8);
	if (fieldsSetBytes >= 13) fieldsSet_4 = readInt(i, fieldsSetBytes - 12);
	
	
		120 velden/bits in groepen van 32
		
		10987654321098765432109876543210
		
		10987654321098765432109876543210	10987654321098765432109876543210	10987654321098765432109876543210	321098765432109876543210
		00000000000000000000000000001111	1000000000000010000001	0101111110000000000000							000000000000000000000100
		=31							  =0	=63				   =32	=95				   =64	=120				 =96
		
		if (fields & 1 << 32 != 0) // field 0 is set ? or field 31
		
		
		76543210
		00000000
		
		read 'long':
		
		10987654321098765432109876543210 10987654321098765432109876543210
		00000000000000000000000000001111 00000000000000000000000000001111
		=31							  =0 =64						  =31
		
	*/
	}
}
