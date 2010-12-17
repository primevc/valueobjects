package primevc.utils.msgpack;
 import haxe.unit.TestCase;
 import haxe.io.Input;
 import haxe.io.Output;
 import haxe.io.BytesInput;
 import haxe.io.BytesOutput;
 import primevc.utils.FastArray;
 import primevc.utils.msgpack.Reader;
  using primevc.utils.msgpack.Format;

class MessagePackTest extends TestCase
{
	static function main()
	{
		var r = new haxe.unit.TestRunner();
		r.add(new MessagePackTest());
		r.run();
	}
	
	var r	: Reader;
	var inp	: Input;
	var out	: BytesOutput;
	
	override function setup() {
		r	= new Reader();
		out	= new BytesOutput();
	}
	
	function test__tinyint()		checkInt(1,  0, 0)
	function test2_tinyint()		checkInt(1,  127, 127)
	
	function test__uint8()			checkInt(2, 0xcc, 128)
	function test2_uint8()			checkInt(2, 0xcc, 255)
	function test__uint16()			checkInt(3, 0xcd, 256)
	function test2_uint16()			checkInt(3, 0xcd, 65535)
	
	function test__tinyint_neg()	checkInt(1, 0xE0, -1)
	function test2_tinyint_neg()	checkInt(1, 0xFF, -32)
	function test__int8()			checkInt(2, 0xd0, -33)
	function test2_int8()			checkInt(2, 0xd0, -128)
	function test__int16()			checkInt(3, 0xd1, -129)
	function test2_int16()			checkInt(3, 0xd1, -32000)
	function test__int32()			checkInt(5, 0xd2, -65000)
	function test2_int32()			checkInt(5, 0xd2, -1073741823)
	
	function test_nil()
	{
		assertEquals(1, out.packNil());
		assertEquals(0xc0, unpack(1, null).get(0));
	}
	
	function test_false()
	{
		assertEquals(1, out.packBool(false));
		assertEquals(0xc2, unpack(1, false).get(0));
	}
	
	function test_true()
	{
		assertEquals(1, out.packBool(true));
		assertEquals(0xc3, unpack(1, true).get(0));
	}
	
	function test_float()
	{
		out.writeByte(0xca);
		out.writeFloat(10.0);
		
		var b = out.getBytes();
		
		inp	= new BytesInput(b);
		assertEquals(10.0, r.readMsgPackValue(inp));
	}
	
	function test_double()
	{
		var r = out.packDouble(1.234);
		assertEquals(9, r);

		var b = unpack(9, 1.234);
		assertEquals(0xcb, b.get(0));
	}
	
	function test_tinyString()
	{
		var r = out.packString("1234567890123456789012345678901");
		assertEquals(32, r);
		
		var b = unpack(32, "1234567890123456789012345678901");
		assertEquals(0xBF, b.get(0));
	}
	
	function test_shortString()
	{
		var r = out.packString("12345678901234567890123456789012");
		assertEquals(35, r);
		
		var b = unpack(35, "12345678901234567890123456789012");
		assertEquals(0xda, b.get(0));
	}
	
	function test2_shortString()
	{
		var r = out.packString("12345678901234567890123456789012∑");
		assertEquals(38, r);
		
		var b = unpack(38, "12345678901234567890123456789012∑");
		assertEquals(0xda, b.get(0));
	}
	
	function test3_bigString()
	{
		var buf = new StringBuf();
		for (i in 0 ... 65535) buf.addChar("_".code);
		var str = buf.toString();
		
		var r = out.packString(str);
		assertEquals(65538, r);
		
		var b = unpack(65538, str);
		assertEquals(0xda, b.get(0));
	}
	
	function test_bigString()
	{
		var buf = new StringBuf();
		for (i in 0 ... 65536) buf.addChar("_".code);
		var str = buf.toString();
		
		var r = out.packString(str);
		assertEquals(65541, r);
		
		var b = unpack(65541, str);
		assertEquals(0xdb, b.get(0));
	}
	
	function test2_bigString()
	{
		var buf = new StringBuf();
		for (i in 0 ... 50000) buf.add("∑");
		var str = buf.toString();
		
		var r = out.packString(str);
		assertEquals(150005, r);
		
		var b = unpack(150005, str);
		assertEquals(0xdb, b.get(0));
	}
	
	function test_tinyArray()
	{
		var r = out.packArrayHeader(2);
		assertEquals(1, r);
		out.packInt(1);
		out.packInt(2);
		
		var arr:Array<Dynamic> = [1, 2];
		unpackArray(3, FastArrayUtil.ofArray(arr));
	}
	
	function test_shortArray()
	{
		var r = out.packArrayHeader(16);
		assertEquals(3, r);
		
		var arr:Array<Dynamic> = [];
		for (i in 0 ... 16) {
			out.packInt(i);
			arr.push(i);
		}
		
		unpackArray(19, FastArrayUtil.ofArray(arr));
	}
	
	function test_bigArray()
	{
		var r = out.packArrayHeader(65536);
		assertEquals(5, r);
		
		var arr:Array<Dynamic> = [];
		for (i in 0 ... 65536) {
			out.packInt(1);
			arr.push(1);
		}
		
		unpackArray(65536 + 5, FastArrayUtil.ofArray(arr));
	}
	
	function test_tinyMap()
	{
		var m = makeMap(15);
		var mapBytes = 1 /* Fixed Map */ + 15 /* Fixed Raw (key type) */ + 15 /* 15 FixedInt values */ + m.stringKeyBytes;
		
		var r = out.packMap(m.map);
		assertEquals(mapBytes, r);
		
		var b = unpackMap(mapBytes, 15);
		assertEquals(0x8F, b.get(0));
	}
	
	function test_shortMap()
	{
		var m = makeMap(16);
		var mapBytes = 1 + 2 /* map 16 */ + 16 /* Fixed Raw (key type) */ + 16 /* FixedInt values */ + m.stringKeyBytes;
		
		var r = out.packMap(m.map);
		assertEquals(mapBytes, r);
		
		var b = unpackMap(mapBytes, 16);
		assertEquals(0xde, b.get(0));
	}
	
	function test_bigMap()
	{
		var m = makeMap(65536);
		var mapBytes = 1 + 4 /* map 32 */
		 	+ 65536				// Fixed Raw (key type)
			+ 128				// first 0-127
			+ 2 * 128			// uint 8
			+ 3 * (65536 - 256)	// remaining values
			+ m.stringKeyBytes;
		
		var r = out.packMap(m.map);
		assertEquals(mapBytes, r);
		
		var b = unpackMap(mapBytes, 16);
		assertEquals(0xdf, b.get(0));
	}

#if flash9

	function test__uint32()	checkUInt(5, 0xce, primevc.types.Number.UINT_MAX)
	function test2_uint32()	checkUInt(1, 0x00, 0)
	function test3_uint32()	checkUInt(2, 0xcc, 255)
	function test4_uint32()	checkUInt(3, 0xcd, 256)
	
	
	// ---
	// Helpers
	// ---
	
	function checkUInt(bytes:Int, firstByte:Int, v:UInt)
	{
		var r = out.packUInt(v);
		assertEquals(bytes, r);
		
		var b = unpack(bytes, v);
		assertEquals("0x"+StringTools.hex(firstByte).substr(-2), "0x"+StringTools.hex(b.get(0)));
	}

#end
	
	function checkInt(bytes:Int, firstByte:Int, v:Int)
	{
		var r = out.packInt(v);
		assertEquals(bytes, r);
		
		var b = unpack(bytes, v);
		assertEquals("0x"+StringTools.hex(firstByte).substr(-2), "0x"+StringTools.hex(b.get(0)));
	}
	
	function unpack(bytes:Int, v:Dynamic)
	{	
		var b = out.getBytes();
		Assert.that(b != null);
		assertEquals(bytes, b.length);
		
		inp	= new BytesInput(b);
		assertEquals(v, r.readMsgPackValue(inp));
		
		return b;
	}
	
	function unpackArray(bytes:Int, v:FastArray<Dynamic>)
	{
		var b = out.getBytes();
		Assert.that(b != null);
		assertEquals(bytes, b.length);
		
		inp	= new BytesInput(b);
		var arr : FastArray<Dynamic> = r.readMsgPackValue(inp);
		
	#if flash10
		// How to check if it's a flash Vector ?  Std.is doesn't work...
	#else
		assertTrue(Std.is(arr, FastArray));
	#end
		
		for (i in 0 ... v.length)
			assertEquals(v[i], arr[i]);
		
		return b;
	}
	
	function unpackMap(bytes:Int, size:Int)
	{
		var b = out.getBytes();
		Assert.that(b != null);
		assertEquals(bytes, b.length);
		
		inp	= new BytesInput(b);
		var map : Hash<Int> = r.readMsgPackValue(inp);
		assertTrue(Std.is(map, Hash));
		
		for (i in 0 ... size) {
			var key = "k" + i;
			assertTrue(map.exists(key));
			assertEquals(i, map.get(key));
		}
		
		return b;
	}
	
	function makeMap(size:Int)
	{
		var m = new Hash<Int>();
		var stringBytes = 0;
		for (i in 0 ... size) {
			var s = "k" + i;
			stringBytes += s.length;
			m.set(s, i);
		}
		
		return {stringKeyBytes: stringBytes, map: m};
	}
}
