package primevc.tools.valueobjects.output;
 import primevc.tools.valueobjects.VODefinition;

class ScalaUtil
{
	static public function bitmask(numBits:Int, offset:Int=0)
	{
		var mask = 0;
		for (bit in 0 ... numBits) {
			mask |= 1 << (bit + offset);
		}
		return "0x" + StringTools.hex(mask, 4);
	}

	static public function propertyName(p:Property) return quote(p.name)
	static public function quote(name:String) return switch (name) {
		case "abstract": "`abstract`";
		case "type": "`type`";
		default: name;
	}

	static public function typeNameInMutablePkg(t:PType, ?surroundWithType:String) : {name:String, defaultValue:Dynamic}
	{
		var res = {name:null, defaultValue: null};
		res.name = (surroundWithType != null? surroundWithType + "[" : "") +
		  (switch(t) {
			case Tarray(innerT,_,_):	"IndexedSeq["+ typeNameInMutablePkg(innerT).name +"]";
			case Turi, Turl:			"prime.types.URI";
			case TuniqueID:				"prime.types.ObjectId";
			case TfileRef:				"prime.types.FileRef";
			case Tstring:				"String";
			case Tinteger(_,_,_):		"Int";
			case Tdecimal(_,_,_):		"Double";
			case Tbool(v):				res.defaultValue = v; "Boolean";
			case TenumConverter(_):		throw t; //"";
			case Temail:				"prime.types.InternetAddress";
			case Tdate:					"prime.types.Date";
			case Tdatetime:				"prime.types.DateTime";
			case Tinterval:				"prime.types.Interval";
			case Tcolor:				"prime.types.RGBA";
			case TclassRef(className):	className;

			case Tdef(ptypedef): switch (ptypedef) {
				case Tclass		(def):	def.mutableFullName + "VO";
				case Tenum		(def):	def.mutableFullName + ".EValue";
			};
		  }) + (surroundWithType != null? "]" : "");

		return res;
	}
}
