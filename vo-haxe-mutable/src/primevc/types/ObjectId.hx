package primevc.types;
 import haxe.io.BytesOutput;
  using primevc.utils.IfUtil;
  using primevc.utils.msgpack.Format;

class ObjectId
{
	static var sessionMID = Std.int(Math.random() * 0xFFFFFF);
	static var sessionPID = Std.int(Math.random() * 0xFFFF);
	
	static inline public var TYPE_ID = 1;
	
	static public function msgpack_packVO(o : BytesOutput, obj : IObjectId, propertyBits : Int, prependMsgpackType : Bool = false) : Int
	{
		Assert.notNull(o);
		Assert.notNull(obj);
		
		var b /* bytes written */ : Int;
		if (prependMsgpackType) {
			if (propertyBits.not0()) b = o.packValueObject();
			else return o.packNil();
		}
		else b = 0;
		
		b += o.packValueObjectHeader(TYPE_ID, 0, propertyBits);
		
		if (propertyBits.not0()) {
			o.writeByte(0x01); ++b;
			b += o.packObjectId(obj.id);
		}
		
		return b;
	}
	
	
	public  var timestamp (default,null) : #if flash9 UInt #else Float #end;
	private var machine		: Int;
	private var pid			: Int;
	private var increment	: Int;
	
	private function new();
	
	static public function make()
	{
		var oid = new ObjectId();
		
		oid.machine	= sessionMID;
		oid.pid		= sessionPID;
		oid.setTimeAndIncrement();
		
		return oid;
	}
	
#if debug
	static public function selftest() {
		var str = "47cc67093475061e3d95369d";
		var o = fromString(str);
		Assert.that(str == o.toString().toLowerCase());
#if !neko
		Assert.that(o.timestamp	== 0x47cc6709);
#end
		Assert.that(o.machine	== 0x347506  );
		Assert.that(o.pid		== 0x1e3d    );
		Assert.that(o.increment	== 0x95369d  );
		
		var bytes = new haxe.io.BytesOutput();
		o.writeBytes(bytes);
		var copy = fromBytes(new haxe.io.BytesInput(bytes.getBytes()));
		
		Assert.that(copy.timestamp	== o.timestamp);
		Assert.that(copy.machine	== o.machine);
		Assert.that(copy.pid		== o.pid);
		Assert.that(copy.increment	== o.increment);
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
	
	static public function fromBytes(input: haxe.io.Input)
	{
		var oid = new ObjectId();
		
		oid.timestamp	= cast input.readInt32();
		oid.machine		= input.readUInt24();
		oid.pid			= input.readUInt16();
		oid.increment	= input.readUInt24();
		
		return oid;
	}
	
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
