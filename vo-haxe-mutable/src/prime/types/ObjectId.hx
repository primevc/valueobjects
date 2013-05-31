package prime.types;
 import prime.core.traits.IObjectId;
 import haxe.io.BytesOutput;
  using prime.utils.msgpack.Format;
  using prime.utils.IfUtil;

class ObjectId
{
	static inline public var TYPE_ID = 0x01D;

	static var sessionMID = Std.int(Math.random() * 0xFFFFFF);
	static var sessionPID = Std.int(Math.random() * 0xFFFF);
	
	public  var timestamp (default,null) : #if flash9 Float #else Float #end;
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
		Assert.isEqual(str, o.toString().toLowerCase());
	#if !neko
		Assert.isEqual(o.timestamp,	0x47cc6709);
	#end
		Assert.isEqual(o.machine,	    0x347506  );
		Assert.isEqual(o.pid,		    0x1e3d    );
		Assert.isEqual(o.increment,	0x95369d  );
		
		var bytes = new haxe.io.BytesOutput();
		bytes.bigEndian = true;
		o.writeBytes(bytes);
		var byteArr = bytes.getBytes();
		var input = new haxe.io.BytesInput(byteArr);
		input.bigEndian = true;
		var copy1 = fromInput(input);
		
		Assert.isEqual(copy1.timestamp, o.timestamp);
		Assert.isEqual(copy1.machine,   o.machine);
		Assert.isEqual(copy1.pid,       o.pid);
		Assert.isEqual(copy1.increment, o.increment);
		
	#if flash10
		var b:flash.utils.ByteArray = (untyped byteArr).b;
		b.length = 1024;
		b.position = 0;
		flash.Memory.select(b);
		var copy2 = fromMemory(0);
		
		Assert.isEqual(copy2.timestamp, o.timestamp);
		Assert.isEqual(copy2.machine,   o.machine);
		Assert.isEqual(copy2.pid,       o.pid);
		Assert.isEqual(copy2.increment, o.increment);
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
	
	static public inline function fromMemory(addr : Int, bigEndian:Bool = true)
	{
		var oid = new ObjectId();
		
		if (bigEndian)
		{
			oid.timestamp =
				flash.Memory.getByte(addr)   << 24 |
    	        flash.Memory.getByte(addr+1) << 16 |
        	    flash.Memory.getByte(addr+2) <<  8 |
            	flash.Memory.getByte(addr+3)       ;
            
			oid.machine	  = flash.Memory.getByte(addr +  4) << 16 | (flash.Memory.getByte(addr +  5) << 8) | flash.Memory.getByte(addr +  6);
			oid.pid       = flash.Memory.getByte(addr +  7) <<  8 |  flash.Memory.getByte(addr +  8);
			oid.increment = flash.Memory.getByte(addr +  9) << 16 | (flash.Memory.getByte(addr + 10) << 8) | flash.Memory.getByte(addr +  11);
		}
		else
		{
			oid.timestamp	= flash.Memory.getI32(addr);
			oid.machine		= flash.Memory.getByte(addr +  4) | (flash.Memory.getUI16(addr +  5) << 8);
			oid.pid			= flash.Memory.getUI16(addr +  7);
			oid.increment	= flash.Memory.getByte(addr +  9) | (flash.Memory.getUI16(addr + 10) << 8);
		}
		
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
		return other 		  != null
			&& this.timestamp == other.timestamp
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
		this.timestamp = prime.utils.TimerUtil.stamp();
		if ((this.increment = ++ObjectId.counter) == 0xFFFFFF)
		 	ObjectId.counter = -1;
	}
}
