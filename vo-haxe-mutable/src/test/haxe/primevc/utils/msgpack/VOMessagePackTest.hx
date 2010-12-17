package primevc.utils.msgpack;
 import primevc.utils.msgpack.MessagePackTest;
 import haxe.unit.TestCase;
 import haxe.io.Input;
 import haxe.io.Output;
 import haxe.io.BytesInput;
 import haxe.io.BytesOutput;
 import primevc.utils.FastArray;
 import primevc.utils.msgpack.Reader;
  using primevc.utils.msgpack.Format;

class VOMessagePackTest extends TestCase
{
	static function main()
	{
		var r = new haxe.unit.TestRunner();
		r.add(new MessagePackTest());
		r.add(new VOMessagePackTest());
		r.run();
	}
	
	var r	: Reader;
	var inp	: Input;
	var out	: BytesOutput;
	
	override function setup() {
		r	= new Reader();
		out	= new BytesOutput();
	}
	
	function test_VO()
	{
		var r = out.packValueObjectHeader(255, 0, 0);
		assertEquals(3, r);
		
	//	out.writeByte(0);
		
		
		var b = unpack(9, 1.234);
		assertEquals(0xcb, b.get(0));
	}
	
	function test_Date()
	{
		
	}
	
	function test_UniqueID()
	{
		
	}
}
