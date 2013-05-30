package prime.tools.valueobjects.xmlmap;
 import prime.types.ObjectId;
 import prime.types.EMail;
 import prime.types.URI;
 import prime.types.RGBA;
// import prime.types.Asset;

class XMLString
{
	// Value -> String
	static public inline function fromBool		(v:Null<Bool>, notSet:Bool)	: String 	{ if (v == null) v = notSet; return v? "true" : "false"; }
	static public inline function fromInt		(v:Int)		: String	{ return Std.string(v); }
	static public inline function fromFloat		(v:Float)	: String	{ return Std.string(v); }
	
	static public inline function fromUniqueID	(v:ObjectId): String	{ return Std.string(v); }
	static public inline function fromURI		(v:URI)		: String	{ return v.string; }
	static public inline function fromEMail		(v:EMail)	: String	{ return v; }
	
	static public inline function fromColor		(v:RGBA)	: String	{ return Std.string(v); }
//	static public inline function fromAsset		(v:Asset)	: String	{ return Std.string(v); }
	
	static public inline function fromDate (v:Date, ?format:String) : String
	{
		return "Date format not implemented yet";
	}
	
	
	// String -> Value
	static public inline function toBool		(v:String, notSet:Bool)	: Bool	 	{ return v == null || v == ""? notSet : v == "true" || v == "1"; }
	static public inline function toInt			(v:String)	: Int		{ return Std.parseInt(v); }
	static public inline function toFloat		(v:String)	: Float		{ return Std.parseFloat(v); }
	
	static public inline function toUniqueID	(v:String)	: ObjectId	{ return cast v; }
	static public inline function toURI			(v:String)	: URI		{ return new URI().parse(v); }
	static public inline function toEMail		(v:String)	: EMail		{ return v; }
	
	static public inline function toColor		(v:String)	: RGBA		{
		var rgb = v == null? 0 : Std.parseInt(v.charCodeAt(0) == '#'.code? "0x"+v.substr(1) : v);
		return #if neko { color: rgb, a:0xFF } #else rgb #end;
	}
//	static public inline function toAsset		(v:String)	: Asset		{ return Asset.fromString(v); }
	
	
	static public function toString(v:String) {
		return ~/&#(.*)?;/.map(v, fromEntityCodeRegex);
	}
	static private function fromEntityCodeRegex(e:EReg) {
		return String.fromCharCode(Std.parseInt("0" + e.matched(1)));
	}
	
	static public inline function toDate (v:String, ?format:String) : Date
	{
		return Date.now();
	}
}
