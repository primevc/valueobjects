package primevc.types;
 import primevc.core.traits.IObjectId;
 import haxe.io.BytesOutput;
  using primevc.utils.IfUtil;
  using primevc.utils.msgpack.Format;

class ObjectId
{
	static inline public var TYPE_ID = 0x01D;

	static var sessionMID = Std.int(Math.random() * 0xFFFFFF);
	static var sessionPID = Std.int(Math.random() * 0xFFFF);
	
	public  var timestamp (default,null) : #if flash9 UInt #else Float #end;
	private var machine		: Int;
	private var pid			: Int;
	private var increment	: Int;
	
	private function new() {}
	
	static public function make()
	{
		var oid = new ObjectId();
		
		oid.machine	= sessionMID;
		oid.pid		= sessionPID;
		oid.setTimeAndIncrement();
		
		return oid;
	}
	
#if debug
	static public function __init__() { // Run selftest at startup
		var str = "47cc67093475061e3d95369d";
		var o = fromString(str);
		Assert.equal(str, o.toString().toLowerCase());
#if !neko
		Assert.equal(o.timestamp,	0x47cc6709);
#end
		Assert.equal(o.machine,	    0x347506  );
		Assert.equal(o.pid,		    0x1e3d    );
		Assert.equal(o.increment,	0x95369d  );
		
		var bytes = new haxe.io.BytesOutput();
		o.writeBytes(bytes);
		var byteArr = bytes.getBytes();
		var copy = fromInput(new haxe.io.BytesInput(byteArr));
		
		Assert.equal(copy.timestamp, o.timestamp);
		Assert.equal(copy.machine,   o.machine);
		Assert.equal(copy.pid,       o.pid);
		Assert.equal(copy.increment, o.increment);
		
	#if flash10
		var b:flash.utils.ByteArray = untyped byteArr.b;
		b.length = 1024;
		flash.Memory.select(b);
		copy = fromMemory(0);
		
		Assert.equal(copy.timestamp, o.timestamp);
		Assert.equal(copy.machine,   o.machine);
		Assert.equal(copy.pid,       o.pid);
		Assert.equal(copy.increment, o.increment);
	#end
	}
#end
	
	static public function fromString(hex:String)
	{
		Assert.that(hex.length == 24); // 47cc67093475061e3d95369d
		
		var oid = new ObjectId();
		oid.timestamp	= Std.parseInt("0x" + hex.substr( 0, 8));
		oid.machine		= Std.parseInt("0x" + hex.substr( 8, 6));
		oid.pid			= Std.parseInt("0x" + hex.substr(14, 4));
		oid.increment	= Std.parseInt("0x" + hex.substr(18   ));
		
		return oid;
	}
	
	static public function fromInput(input: haxe.io.Input)
	{
		var oid = new ObjectId();
		
		oid.timestamp	= cast input.readInt32();
		oid.machine		= input.readUInt24();
		oid.pid			= input.readUInt16();
		oid.increment	= input.readUInt24();
		
		return oid;
	}

#if flash10
	
	static public inline function fromMemory(addr : Int)
	{
		var oid = new ObjectId();
		
		oid.timestamp	= flash.Memory.getI32(addr);
		oid.machine		= flash.Memory.getByte(addr +  4) | (flash.Memory.getUI16(addr +  5) << 8); // input.readUInt24();
		oid.pid			= flash.Memory.getUI16(addr +  7); // input.readUInt16();
		oid.increment	= flash.Memory.getByte(addr +  9) | (flash.Memory.getUI16(addr + 10) << 8); // input.readUInt24();
		
		return oid;
	}
	
#end
    
	public function writeBytes(out : haxe.io.Output)
	{
		out.writeInt32(haxe.Int32.ofInt( cast timestamp ));
		out.writeUInt24(machine);
		out.writeUInt16(pid);
		out.writeUInt24(increment);
	}
	
	public function toString()
	{
		var S = StringTools;
		return S.hex(cast timestamp, 8) + S.hex(machine, 6) + S.hex(pid, 4) + S.hex(increment, 6);
	}
	
	public inline function equals(other : ObjectId)
	{
		return this.timestamp == other.timestamp
		 	&& this.machine   == other.machine
			&& this.pid		  == other.pid
			&& this.increment == other.increment;
	}
	
	
	//
	// Timestamping
	//
	
	static var counter		= -1;
	private function setTimeAndIncrement()
	{
		this.timestamp =
		 	#if flash9	untyped    Date.now().getTime() * 0.001 ; #end
			#if js		Math.floor(Date.now().getTime() * 0.001); #end
		
		if ((this.increment = ++ObjectId.counter) == 0xFFFFFF)
		 	ObjectId.counter = -1;
	}
}
