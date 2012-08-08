package primevc.tools.valueobjects.output;
 import primevc.tools.valueobjects.VODefinition;
  using primevc.tools.valueobjects.VODefinition;

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

	static public function quote(name:String, suffix:String = null) return switch (name) {
		case "abstract": suffix == null? "`abstract`" : "abstract" + suffix;
		case "type":     suffix == null? "`type`"     : "type"     + suffix;
		case "values":   suffix == null? "_values"    : "_values"  + suffix;
		default: 		 suffix == null? name         : name       + suffix;
	}

	static public function typeNameInMutablePkg(t:PType, ?surroundWithType:String) : {name:String, defaultValue:Dynamic}
	{
		var res = {name:null, defaultValue: null};
		res.name = (surroundWithType != null? surroundWithType + "[" : "") +
		  (switch(t) {
			case Tarray(innerT,_,_):	"Array["+ typeNameInMutablePkg(innerT).name +"]";
			case Turi, Turl:			"prime.types.URI";
			case TuniqueID:				"prime.types.ObjectId";
			case TfileRef:				"prime.types.FileRef";
			case Tstring:				"String";
			case Tinteger(_,_,_):		"Int";
			case Tdecimal(_,_,_):		"Double";
			case Tbool(v):				res.defaultValue = v; "Boolean";
			case TenumConverter(_):		throw t; //"";
			case Temail:				"prime.types.EmailAddr";
			case Tdate:					"prime.types.Date";
			case Tdatetime:				"prime.types.DateTime";
			case Tinterval:				"prime.types.Interval";
			case Tcolor:				"prime.types.RGBA";
			case TclassRef(className):	className;

			case Tdef(ptypedef): switch (ptypedef) {
				case Tclass		(def):	def.mutableFullName + "VO";
				case Tenum		(def):	def.fullName;
			};
		  }) + (surroundWithType != null? "]" : "");

		return res;
	}

	static public function scalaType(t:PType, ?surroundWithType:String) : {name:String, defaultValue:Dynamic}
	{
		var res = {name:null, defaultValue: null};
		res.name = (surroundWithType != null? surroundWithType + "[" : "") +
		  (switch(t) {
			case Tarray(innerT,_,_):	"IndexedSeq["+ scalaType(innerT).name +"]";
			case Turi, Turl:			"prime.types.URI";
			case TuniqueID:				"prime.types.ObjectId";
			case TfileRef:				"prime.types.FileRef";
			case Tstring:				"String";
			case Tinteger(_,_,_):		"Int";
			case Tdecimal(_,_,_):		"Double";
			case Tbool(v):				res.defaultValue = v; "Boolean";
			case TenumConverter(_):		throw t; //"";
			case Temail:				"prime.types.EmailAddr";
			case Tdate:					"prime.types.Date";
			case Tdatetime:				"prime.types.DateTime";
			case Tinterval:				"prime.types.Interval";
			case Tcolor:				"prime.types.RGBA";
			case TclassRef(className):	className;

			case Tdef(ptypedef): switch (ptypedef) {
				case Tclass		(def):	def.fullName;
				case Tenum		(def):	def.fullName;
			};
		  }) + (surroundWithType != null? "]" : "");

		return res;
	}

	static public function scalaConversionExpr(t:PType, inputExpr:String = null) : String
	{
		var arrayType = null;
		var converterFn = switch(t)
		{
			case Tarray(innerT,_,_):	arrayType = innerT; "Vector";
			case Turi, Turl:			"      URI";
			case TuniqueID:				" ObjectId";
			case TfileRef:				"  FileRef";
			case Tstring:				"   String";
			case Tinteger(_,_,_):		"      Int";
			case Tdecimal(_,_,_):		"   Double";
			case Tbool(v):				"  Boolean";
			case Temail:				"EmailAddr";
			case Tdate:					"     Date";
			case Tdatetime:				" DateTime";
			case Tinterval:				" Interval";
			case Tcolor:				"     RGBA";
			case TclassRef(className):	className + ".valueOf";
			case TenumConverter(t):		throw t;

			case Tdef(ptypedef): switch (ptypedef) {
				case Tclass		(def):	def.fullName + ".valueOf";
				case Tenum		(def):	def.fullName + ".valueOf";
			}
		}

		return if (inputExpr == null) converterFn
		  else if (arrayType != null)
			converterFn + "(" + inputExpr + ")(" + scalaConversionExpr(arrayType) + ")";
		  else
			converterFn + "(" + inputExpr + ")";
	}

	static public function lazyInit(t:PType) return switch(t)
	{
		case Tarray(innerT,_,_): lazyInit(innerT);
		case Tdef(innerT): switch(innerT) {
			case Tclass(_): true;
			case Tenum(_): false;
		}
		default: false;
	}
}
