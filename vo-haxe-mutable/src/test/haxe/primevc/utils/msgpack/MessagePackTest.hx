package primevc.utils.msgpack;
 import haxe.unit.TestCase;
 import haxe.io.Input;
 import haxe.io.Output;
 import haxe.io.BytesInput;
 import haxe.io.BytesOutput;
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
//	function test_uint32()			checkInt(5, 0xce, 0x3FFFFFFF) is packed as Int32
	
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
		assertEquals(0xc0, packUnpack(1, null).get(0));
	}
	
	function test_false()
	{
		assertEquals(1, out.packBool(false));
		assertEquals(0xc2, packUnpack(1, false).get(0));
	}
	
	function test_true()
	{
		assertEquals(1, out.packBool(true));
		assertEquals(0xc3, packUnpack(1, true).get(0));
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

		var b = packUnpack(9, 1.234);
		assertEquals(0xcb, b.get(0));
	}
	
	function test_tinyString()
	{
		var r = out.packString("1234567890123456789012345678901");
		assertEquals(32, r);
		
		var b = packUnpack(32, "1234567890123456789012345678901");
		assertEquals(0xBF, b.get(0));
	}
	
	function test_shortString()
	{
		var r = out.packString("12345678901234567890123456789012");
		assertEquals(35, r);
		
		var b = packUnpack(35, "12345678901234567890123456789012");
		assertEquals(0xda, b.get(0));
	}
	
	function test2_shortString()
	{
		var r = out.packString("12345678901234567890123456789012∑");
		assertEquals(38, r);
		
		var b = packUnpack(38, "12345678901234567890123456789012∑");
		assertEquals(0xda, b.get(0));
	}
	
	function test3_bigString()
	{
		var buf = new StringBuf();
		for (i in 0 ... 65535) buf.addChar("_".code);
		var str = buf.toString();
		
		var r = out.packString(str);
		assertEquals(65538, r);
		
		var b = packUnpack(65538, str);
		assertEquals(0xda, b.get(0));
	}
	
	function test_bigString()
	{
		var buf = new StringBuf();
		for (i in 0 ... 65536) buf.addChar("_".code);
		var str = buf.toString();
		
		var r = out.packString(str);
		assertEquals(65541, r);
		
		var b = packUnpack(65541, str);
		assertEquals(0xdb, b.get(0));
	}
	
	function test2_bigString()
	{
		var buf = new StringBuf();
		for (i in 0 ... 50000) buf.add("∑");
		var str = buf.toString();
		
		var r = out.packString(str);
		assertEquals(150005, r);
		
		var b = packUnpack(150005, str);
		assertEquals(0xdb, b.get(0));
	}
	
	function test_tinyArray()
	{
		var r = out.packArrayHeader(2);
		assertEquals(1, r);
		out.packInt(1);
		out.packInt(2);
		
		packUnpackArray(3, [1, 2]);
	}
	
	function test_shortArray()
	{
		var r = out.packArrayHeader(16);
		assertEquals(3, r);
		
		var arr = [];
		for (i in 0 ... 16) {
			out.packInt(i);
			arr.push(i);
		}
		
		packUnpackArray(19, arr);
	}
	
	function test_bigArray()
	{
		var r = out.packArrayHeader(65536);
		assertEquals(5, r);
		
		var arr = [];
		for (i in 0 ... 65536) {
			out.packInt(1);
			arr.push(1);
		}
		
		packUnpackArray(65536 + 5, arr);
	}
	
	// ---
	// Helpers
	// ---
	
	function checkInt(bytes:Int, firstByte:Int, v:Int)
	{
		var r = out.packInt(v);
		assertEquals(bytes, r);
		
		var b = packUnpack(bytes, v);
		assertEquals("0x"+StringTools.hex(firstByte).substr(-2), "0x"+StringTools.hex(b.get(0)));
	}
	
	function packUnpack(bytes:Int, v:Dynamic)
	{	
		var b = out.getBytes();
		Assert.that(b != null);
		assertEquals(bytes, b.length);
		
		inp	= new BytesInput(b);
		assertEquals(v, r.readMsgPackValue(inp));
		
		return b;
	}
	
	function packUnpackArray(bytes:Int, v:Array<Dynamic>)
	{	
		var b = out.getBytes();
		Assert.that(b != null);
		assertEquals(bytes, b.length);
		
		inp	= new BytesInput(b);
		var arr : Array<Dynamic> = r.readMsgPackValue(inp);
		assertTrue(Std.is(arr, Array));
		
		for (i in 0 ... v.length)
			assertEquals(v[i], arr[i]);
		
		return b;
	}
	
/*	
	Variable	110xxxxx	0xc0 - 0xdf
	
	FixMap		1000xxxx	0x80 - 0x8f
	map 16		11011110	0xde
	map 32		11011111	0xdf
*/
}
