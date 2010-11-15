package primevc.tools.valueobjects.xmlmap;
 import primevc.types.UniqueID;
 import primevc.types.EMail;
 import primevc.types.URI;
 import primevc.types.RGBA;
 import primevc.types.Bitmap;

class XMLString
{
	// Value -> String
	static public inline function fromBool		(v:Null<Bool>, notSet:Bool)	: String 	{ if (v == null) v = notSet; return v? "true" : "false"; }
	static public inline function fromInt		(v:Int)		: String	{ return Std.string(v); }
	static public inline function fromFloat		(v:Float)	: String	{ return Std.string(v); }
	
	static public inline function fromUniqueID	(v:UniqueID): String	{ return Std.string(v); }
	static public inline function fromURI		(v:URI)		: String	{ return v.string; }
	static public inline function fromEMail		(v:EMail)	: String	{ return v; }
	
	static public inline function fromColor		(v:RGBA)	: String	{ return Std.string(v); }
	static public inline function fromBitmap	(v:Bitmap)	: String	{ return Std.string(v); }
	
	static public inline function fromDate (v:Date, ?format:String) : String
	{
		return "Date format not implemented yet";
	}
	
	
	// String -> Value
	static public inline function toBool		(v:String, notSet:Bool)	: Bool	 	{ return v == null || v == ""? notSet : v == "true" || v == "1"; }
	static public inline function toInt			(v:String)	: Int		{ return Std.parseInt(v); }
	static public inline function toFloat		(v:String)	: Float		{ return Std.parseFloat(v); }
	
	static public inline function toUniqueID	(v:String)	: UniqueID	{ return cast v; }
	static public inline function toURI			(v:String)	: URI		{ return new URI().parse(v); }
	static public inline function toEMail		(v:String)	: EMail		{ return v; }
	
	static public inline function toColor		(v:String)	: RGBA		{
		return v == null? 0 : Std.parseInt(v.charCodeAt(0) == '#'.code? "0x"+v.substr(1) : v);
	}
	static public inline function toBitmap		(v:String)	: Bitmap	{ return new Bitmap().parse(v); }
	
	
	static public function toString(v:String) {
		return ~/&#(.*)?;/.customReplace(v, fromEntityCodeRegex);
	}
	static private function fromEntityCodeRegex(e:EReg) {
		return String.fromCharCode(Std.parseInt("0" + e.matched(1)));
	}
	
	static public inline function toDate (v:String, ?format:String) : Date
	{
		return Date.now();
	}
}
