package primevc.tools.valueobjects.output;
 import primevc.tools.valueobjects.VODefinition;
 import primevc.utils.NumberUtil;
  using primevc.utils.TypeUtil;
  using primevc.tools.valueobjects.output.ScalaUtil;
  using primevc.tools.valueobjects.VODefinition;

class ScalaTypeMap implements CodeGenerator
{
	public var list    : List   <String>;
	public var map     : IntHash<String>;
	public var enumMap : IntHash<String>;

	public function new() {
		list    = new List();
		map     = new IntHash();
		enumMap = new IntHash();
	}

	function strippedName(def : TypeDefinition) return def.fullName.substr(def.module.getPackageRoot().fullName.length + 1)

	public function genClass(def : ClassDef) {
		if (!def.isMixin && !map.exists(def.index)) {
			var name = strippedName(def);
			list.add(name);
			map.set(def.index, name);
		}
	}

	public function genEnum(def:EnumDef) {
		if (!enumMap.exists(def.index))
			enumMap.set(def.index, strippedName(def));
	}

	public function newModule(module:Module) : CodeGenerator {
		module.generateWith(this);
		return this;
	}
}

private class ScalaBase
{
	var mod			: Module;
	var code		: StringBuf;
	var dir			: String;
	var shouldWrite : Bool = false;

	inline function a(str) code.add(str)
	inline function ac(ch) code.addChar(ch)

	private function new(m) {
		code = new StringBuf();
		mod  = m;
	}

	function writeHeader(file) throw "abstract"

	function write(pkgName, def:TypeDefinition)
	{
//		if (!shouldWrite) return;

		var filename = dir +"/"+ def.name + ".scala";
		trace("WRITING: "+filename);

		var file = neko.io.File.write(filename, false);
		writeHeader(file);
		file.writeString("\n\npackage ");
		file.writeString(pkgName);

		file.writeString("\n{\n\n");
		file.writeString(code.toString());
		file.writeString("\n\n} // end ");
		file.close();

		code = new StringBuf();
	}

	function writeValueLiteral(type:PType, val:Dynamic) switch (type)
	{
		case Tbool(dval):
			a(Std.string(val == null? dval : val));

		case Tinteger(_,_,_):
			a(val == null? "Int.MinValue" : Std.string(val));

		case Tdecimal(_,_,_):
			a(val == null? "Double.NaN" : Std.string(val));

		case TfileRef:
			if (val != null) {
				a("FileRef("); ac('"'.code); a(Std.string(val)); a('")');
			}
			else a("prime.types.emptyFileRef");

		case Tstring:
			if (val != null) {
				ac('"'.code); a(Std.string(val)); ac('"'.code);
			}
			else a('""');

		case Turi, Turl:
			if (val != null) {
				a("URI("); ac('"'.code); a(Std.string(val)); a('")');
			}
			else a("prime.types.emptyURI");

		case Temail:
			if (val != null) {
				a("EmailAddr("); ac('"'.code); a(Std.string(val)); a('")');
			}
			else a("prime.types.emptyEmailAddr");

		case Tdate:
			if (val != null) {
				a("Date("); ac('"'.code); a(Std.string(val)); a('")');
			}
			else a("prime.types.minDate");

		case Tdatetime:
			if (val != null) {
				a("DateTime("); ac('"'.code); a(Std.string(val)); a('")');
			}
			else a("prime.types.minDateTime");

		case Tinterval:
			if (val != null) throw val;
			else a("prime.types.minInterval");

		case Tcolor:
			if (val != null) {
				if (Std.is(val, String)) {
					ac('"'.code); a(Std.string(val)); ac('"'.code);
				}
				else
					a(StringTools.hex(val, 6));
			} else {
				a("Colors.black");
			}

		case TenumConverter(enums):
			throw "Unsupported value literal";

		case Tarray(innerT,_,_):
			if (val == null)
				a("IndexedSeq.empty");
			else {
				var values : Array<Dynamic> = val;
				var first = true;
				a("IndexedSeq(");
				for (v in values) {
					if (!first) a(","); else first = false;
					writeValueLiteral(innerT, v);
				}
				a(")");
			}

		case TuniqueID:
			if (val == null) a("prime.types.emptyObjectId"); else throw val;

		case Tdef(_), TclassRef(_):
			if (val == null) a("null"); else throw val;
	}
}

class Scala extends ScalaBase, implements CodeGenerator
{
	//private static var writelist = new List<Scala>();

	public static function generate() {
		MutableScala.generate();
		Module.root.generateWith(new Scala(Module.root));

		var filename = "ValueObjects.scala";
		trace("WRITING: "+filename);
		var file = neko.io.File.write(filename, false);

		for (m in Module.pkgRoots)
		{
			var map = new ScalaTypeMap();
			m.generateWith(map);
			file.writeString("
package "+ m.fullName +" {
  object VO {
  // Force correct initialization of ValueObject meta objects:\n  ");
			var i = 0;
			for (type in map.list) {
				file.writeString("implicit val voCompanion" + (i++) + " = "); file.writeString(type); file.writeString(";\n  ");
			}
			for (thing in [{m:map.enumMap, n:"enumMap"}, {m:map.map, n:"typeMap"}]) {
				file.writeString("\n  final val "+ thing.n +" = scala.collection.immutable.IntMap(");
				var first = true;
				for (index in thing.m.keys()) {
					if (first) first = false;
					else file.writeString(",");
					file.writeString("\n    " + index + " -> " + thing.m.get(index));
				}
				file.writeString("
  )");

			}

file.writeString("}
}
");
		}
		//for (m in writelist) {
		//	m.mod.mkdir();
		//	m.write(file);
		//}
		file.close();
	}

	var currentFieldBitNum : Int;

	private function new(m:Module) {
		super(m);
		dir  = m.mkdir();
		//writelist.add(this);
	}

	override function writeHeader(file) file.writeString(
"// GENERATED BY VOCompiler - MODIFICATIONS WILL BE LOST

import prime.vo._;
import prime.vo.source._;
import prime.types.{Enum, EnumValue, Conversion, Colors, FileRef, VORef};
import prime.types.Conversion._;
import prime.types.ValueTypes._;

")

	function propFlag(p:Property) return '0x' + StringTools.hex(1 << p.bitIndex(), 8)
	function propHex (p:Property) return '0x' + StringTools.hex((p.propertyID() << 8) | p.bitIndex(), 6)

	public function genClass(def : ClassDef)
	{
		shouldWrite = true;

		// -----
		// Inferred Class metadata

		var leafNode                  = true;
		var onlyDoubles               = true;
		var onlyIntegers              = true;
		var hasDoubles                = false;
		var hasIntegers               = false;
		var lastField                 = null;
		var idType                    = null;
		var idFromSuperclassOrTrait   = false;
		var idField : Property        = null;
		var longestFieldNameLength    = 0;
		var longestSubFieldNameLength = 0;

		inline function spaces(n:Int) while(n-->0) ac(' '.code);

		function copyPrototype(defaultParams = true, types = true, defaultFromEmpty = false)
		{
			var first = true;
			for (p in def.propertiesSorted) {
				if (first) first = false; else a(", ");
				a(p.name.quote());
				if (types) {
					a(": "); a(p.scalaType());
				}
				if (defaultParams) {
					a(" = ");
					if      (defaultFromEmpty) { a("empty."); a(p.name.quote());    }
					else if (p.lazyInit())     { a("this.");  a(p.name.quote("0")); }
					else                       { a("this.");  a(p.name.quote());    }
				}
			}
		}
		function fieldDefinition(p : Property, protectedOnly = false) {
			if (!p.lazyInit() || !protectedOnly) {
				a(!p.lazyInit()? "  val " : "  def "); a(p.name.quote());
				spaces(longestFieldNameLength - p.name.quote().length); a(" : "); a(p.scalaType());
			}
			if (p.lazyInit()) {
				if (!protectedOnly) a(";"); a("  protected var "); a(p.name.quote('0')); a(": "); a(p.scalaType());
			}
		}

		var fields = def.propertiesSorted;

		// Do the Meta inferring
		for (p in fields)
		{
			lastField = p;
			longestFieldNameLength = IntMath.max(p.name.quote().length, longestFieldNameLength);
			if (!p.type.isSingleValue()) longestSubFieldNameLength = IntMath.max(p.name.quote().length, longestSubFieldNameLength);

			if (p.hasOption(unique))
			{
				if (idField != null) throw "ValueObjects may have only 1 ID field...\n  idField = "+idField+"\n"+def;

				idField = p;
				idType  = p.type.scalaType().name;
				Assert.notNull(idType);

				if (p.definedIn != def)
					idFromSuperclassOrTrait = true;
			}
			switch(p.type)
			{
				case Tdef(pt):
					onlyDoubles = onlyIntegers = false;
					switch(pt) {
						case Tenum (_):
						case Tclass(_): if (!p.isReference()) leafNode = false;
					}

				case Tarray(t,_,_):
					onlyDoubles = onlyIntegers = false;
					if (leafNode) leafNode = t.isSingleValue();

				case Tinteger(_,_,_): onlyDoubles  = false; hasIntegers = true;
				case Tdecimal(_,_,_): onlyIntegers = false; hasDoubles  = true;

				case Tbool(_), Turi, Turl, TuniqueID, Tstring, TfileRef, Temail, Tcolor, Tdate, Tdatetime, Tinterval,
				     TenumConverter(_), TclassRef(_):
				    onlyDoubles = onlyIntegers = false;
			}
		}

		// -----
		// Trait definition

a(Std.format("

//---

trait ")); a(def.name); a(" extends ");
		// Supertypes
		if (def.superClass != null) {
			a(def.superClass.fullName);
		}
		else {
			a("ValueObject");
		}
		if (idField != null && !idFromSuperclassOrTrait) {
			// Superclass has no Unique ID. Add the required trait now.
			a(" with ID");
		}
		for (t in def.supertypes) if (Std.is(t, MagicClassDef) || def.superClass != t) {
			 a("\n with "); a(t.fullName);
		}
		a(" {\n");

		for (p in fields) if (p.definedIn == def) {
			fieldDefinition(p); a(";\n");
		}
		// def _id
		if (idField != null && !idFromSuperclassOrTrait) {
			// Superclass has no Unique ID. Add the required trait now.
			a("\n type IDType = "); a(idField.type.scalaType().name); a("; def _id = "); a(idField.name); a(";\n");
		}

		if (!def.isMixin && !def.implementedBy.iterator().hasNext()){
			a("\n  def copy(");
			copyPrototype();
			a(") : this.type;\n}\n");
		} else {
			a("}\n");
		}

		// -----
		// Class definition
		function voSourceAt(p:Property, localEmpty = false) return 'At("'+ p.name +'", '+ propHex(p) +', '+ (localEmpty? 'this' : def.fullName +'.empty') + ((p.type.isTclass() && p.type.getPTypedef().unpackPTypedef() == def)? '' : '.' + p.name.quote()) +')';
		function anyAt     (p:Property, localEmpty = false) return 'voSource.any'    + voSourceAt(p, localEmpty);
		function intAt     (p:Property, localEmpty = false) return '          voSource.int' + voSourceAt(p, localEmpty);
		function doubleAt  (p:Property, localEmpty = false) return '       voSource.double' + voSourceAt(p, localEmpty);

		if (!def.isMixin)
		{

			a("\nfinal class "); a(def.name); a("VO protected["); a(def.module.getPackageRoot().name != ""? def.module.getPackageRoot().name : def.module.name);
			a("](\n  "); if (fields.length > 1) a("voIndexSet : Int, srcDiff : Int, "); a("@transient final val voSource : ValueSource");

			for (p in fields)
			{
				a(",\n");
				fieldDefinition(p, true);
			}

			a("\n) extends ValueObject_");
			switch (fields.length) {
				case 0:                      ac( "0".code);
				case 1:                      ac( "1".code);
				case 2,3,4:                   a( "4(voIndexSet, srcDiff)");
				case 5,6,7,8:                 a( "8(voIndexSet.toByte,  srcDiff.toByte)");
				case 9,10,11,12,13,14,15,16:  a("16(voIndexSet.toShort, srcDiff.toShort)");
				default:                      a("32(voIndexSet, srcDiff)");
			}
			a(" with "); a(def.name);

			if (leafNode) { if (fields.length > 0) a(" with LeafNode"); }
			else          a(" with BranchNode");

			if (fields.length > 0) {
				if      (onlyDoubles)  a(" with Doubles");
				else if (onlyIntegers) a(" with Integers");
				else if (!(hasDoubles || hasIntegers)) a(" with NoPrimitives");
			}
			a(" {\n");
			// VOType, voCompanion, voManifest
			a("  type VOType     = "); a(def.name); a("\n");
			a("  def voCompanion = "); a(def.name); a("\n");
			a("  def voManifest  = "); a(def.name); a(".manifest\n\n");

			// Lazy getters
			for (p in fields) if (p.lazyInit())
			{
				function p0() { a(p.name); ac("0".code); }

				a("  final def "); spaces(4 - p.name.quote().length); a(p.name.quote()); a(" = if ("); p0(); a(" != null) "); p0(); a(" else {\n");
				a("    synchronized"); spaces(p.name.quote().length - 4); a(" { if ("); p0(); a(" == null) "); p0(); a(' = try '); a(p.type.scalaConversionExpr(anyAt(p))); a(" catch { case NoInputException => "); a(def.name); a(".empty."); a(p.name.quote()); a(' }; }\n');
				a("    "); p0();
				a("\n  }\n");
			}

			// foreach
			if (fields.length > 1)
			{
				a("\n  def foreach (fieldIndexMask : Int)(f: (ValueObjectField["); a(def.name); a("], Any) => Unit) {\n");
				a("    val voIndexSet = this._voIndexSet & fieldIndexMask;\n    val voField = "); a(def.name); a(".field;\n");
				if (!leafNode) { a("    val empty = "); a(def.name); a(".empty;\n"); }
				for (p in fields)
				{
					if (!p.lazyInit()) {
						a("    if ((voIndexSet   "); if (!leafNode) spaces(longestSubFieldNameLength - 6); a(" & 0x"); a(StringTools.hex(1 << p.bitIndex(), 8)); a(") != 0)"); if (!leafNode) spaces(longestSubFieldNameLength - 8);
					} else {
						a("    if ((fieldIndexMask"); a(" & 0x"); a(StringTools.hex(1 << p.bitIndex(), 8)); a(") != 0 && ");
						if (p.isArray()) a("!"); a("this."); a(p.name.quote()); spaces(IntMath.max(6, longestSubFieldNameLength) - p.name.quote().length);
						if (!p.isArray()){ a(" != empty."); a(p.name.quote()); ac(")".code); } else { a(".isEmpty)  "); }
						spaces(IntMath.max(8, longestSubFieldNameLength) - p.name.quote().length);
					}
					a(" f(voField."); a(p.name.quote()); ac(",".code); spaces(longestFieldNameLength - p.name.quote().length); a(" this.");
					a(p.name.quote(p.lazyInit()? "0" : null));
					a(");\n");
				}
				a("  }\n");
			}

			if (!leafNode)
			{
				if (fields.length > 1) {
					// def voIndexSet
					a("\n  override def voIndexSet = initIndexSet");
					for (p in fields) if (p.lazyInit()) {
						a(" | (if (this."); a(p.name.quote());
						if (p.type.isArray()) a(" isEmpty"); else {
							a(" == "); a(def.name); a(".empty."); a(p.name.quote());
						}
						a(") 0 else "); a(propFlag(p)); a(")");
					}
					a(";\n");
				}

				// def realized, isRealized
				a("\n  def isRealized =   ");
				var first = true;
				for (p in fields) if (p.lazyInit()) {
					if (!first) a(" && "); else first = false;
					a("this."); a(p.name); a("0 != null");
				}
				a(";\n");

				a("  def realized   = { ");
				for (p in fields) if (p.lazyInit()) {
					a("this."); a(p.name.quote()); a(!p.type.isSingleValue()? (p.type.isArray()? ".foreach(_.realized); " : ".realized; ") : "; ");
				}
				a("self }\n\n");
			}

			function unboxedAt(name : String, type : String, converter : String, hasOfType : Bool, predicate : Property -> Bool)
			{
				a("  def "); a(name); a("At (name: String, idx: Int, notFound: "); a(type); a("): "); a(type); a(" = ");
				if (!hasOfType) {
					a("if (this.contains(name,idx)) "); a(converter); a("(anyAt(name,idx)) else notFound;\n");
				}
				else
				{
					a("name match {\n");
					for (p in fields) if (predicate(p)) {
						a('    case "'); a(p.name); a('" => this.'); a(p.name.quote()); a(";\n");
					}
					a("    case _ => try voManifest.index(idx) match {\n");
					for (p in fields) if (predicate(p)) {
						a("      case " + p.bitIndex()); a(" => this."); a(p.name.quote()); a(";\n");
					}
					a("    } catch { case _ => if (this.contains(name,idx)) "); a(converter); a("(anyAt(name,idx)) else notFound; }\n  }\n");
				}
			}

			if (hasDoubles || hasIntegers)
			{
				if (!onlyIntegers)
					unboxedAt("double", "Double", "Decimal", hasDoubles,  function (p) return switch (p.type) { default: false; case Tdecimal(_,_,_): true; });
				if (!onlyDoubles)
					unboxedAt("int",    "Int",    "Integer", hasIntegers, function (p) return switch (p.type) { default: false; case Tinteger(_,_,_): true; });
			}

			// def copy(...)
			if (fields.length > 0)
			{
				function ifFieldDiff(p:Property, s:Int, os:Int, isObj:Bool, nanCheck:Bool = false, obj:String = "this") {
					var objCheckWidth = fields.length == 1? 0 : leafNode? 0 : longestSubFieldNameLength + 5 + longestSubFieldNameLength + 4 + 5 + longestSubFieldNameLength;

					a("    if ("); a(p.name.quote());
					if (isObj)
					{
						spaces(s);  a(" != null "); spaces(os + p.name.quote().length); a(" && "); a(p.name.quote()); spaces(os); a(" != "); a(obj); a("."); a(p.name.quote());
						spaces(os + 1);
					}
					else if (!nanCheck)
					{
						spaces(s); a(" != "); a(obj); a("."); a(p.name.quote());
						spaces(IntMath.max(objCheckWidth - p.name.quote().length, s));
					} else {
						spaces(s + 1); a(".compare("); a(obj); a("."); a(p.name.quote()); a(") != 0"); spaces(objCheckWidth - p.name.quote().length - (10 - obj.length) - 6);
					}
					ac(")".code);
				}

				a("\n  protected def copy("); copyPrototype(false); a(", voRoot : ValueSource) : this.type = {\n");
				if (fields.length > 1)
				{
					a("    val empty     = "); a(def.name); a(".empty;\n");
					a("    var voDiff    = 0;\n");
					if (!leafNode) {
						a("    var voLazy    = 0;\n");
					}
					a("    var voEmptied = 0;\n");
					for (p in fields)
					{
						var bitFlag  = propFlag(p);
						var isObj    = p.lazyInit();
						var s        = longestFieldNameLength    - p.name.quote().length;
						var os       = longestSubFieldNameLength - p.name.quote().length;
						var nanCheck = switch (p.type) {
							case Tdecimal(_,_,_): p.defaultValue == null || Math.isNaN(p.defaultValue);
							default: false;
						}

						ifFieldDiff(p,s,os,isObj,nanCheck); a(" { voDiff |= "); a(bitFlag);
						a("; if ("); a(p.name.quote()); spaces(s);
						if (nanCheck) {
							a("    isNaN"); spaces(p.name.quote().length + 1);
						} else if (p.isArray()) {
							a("  isEmpty"); spaces(p.name.quote().length + 1);
						} else {
							a(" == empty."); a(p.name.quote());
						}
						spaces(s);
						a(") voEmptied |= "); a(bitFlag); a("; }");

						if (p.lazyInit()) {
							a(" else if ("); a(p.name.quote()); a(" == null)"); a(" voLazy |= "); a(bitFlag);
						}
						a(";\n");
					}
					a("
    if ("); a(leafNode? "voDiff" : "!(voRoot eq this.voSource) || voDiff"); a(" != 0) {
      val voNewIndexSet = (this._voIndexSet | voDiff) ^ voEmptied;
      if (voNewIndexSet != 0"); if (!leafNode) a(" || voLazy != 0"); a(")\n        new "); a(def.name); a("VO(voNewIndexSet, voDiff, voRoot, "); copyPrototype(false,false); a(").asInstanceOf[this.type]
      else                    empty.asInstanceOf[this.type];
    }
    else this;\n  }\n");
		  		}
		  		else // fields.length == 1
		  		{
		  			var p = fields[0];
		  			var isObj = p.lazyInit();
		  			ifFieldDiff(p,0,0,isObj); a(" {\n      val empty = "); a(def.name); a(".empty;\n  ");
		  			ifFieldDiff(p,0,0,isObj, "empty"); a(" new "); a(def.name); a("VO(voRoot, "); copyPrototype(false,false); a(").asInstanceOf[this.type]
      else empty.asInstanceOf[this.type];\n    }\n    else this;\n  }\n");
		  		}

				a("  override def conj(voSource : ValueSource, newRoot : ValueSource) : this.type = { val voField = "); a(def.name); a(".field; val empty = "); a(def.name); a(".empty;");

				if (!leafNode) for (p in fields) if (p.lazyInit())
	      		{
	      			a("\n    val "); a(p.name.quote()); a(" = ");
	      			if (p.type.isArray() && p.lazyInit()) {
						a("try (if (voSource != newRoot) "); a(p.type.scalaConversionExpr(anyAt(p, true))); a(" else if (this.voSource == ValueSource.empty) null else "); a(p.name); a("0)");
						a(" catch { case NoInputException => empty."); a(p.name.quote()); a(" };");
					}
					else {
						a("voField."); a(p.name.quote()); a("(this, voSource, newRoot, "); a(p.name); a("0);");
					}
				}

				a("\n    this.copy(\n");
	      		for (p in fields)
	      		{
	      			a("      "); a(p.name.quote()); spaces(longestFieldNameLength - p.name.quote().length); a(" = "); if (!p.lazyInit()) a("try ");
	      			switch (p.type) {
	      				case Tinteger(_,_,_): a(   intAt(p, true));
						case Tdecimal(_,_,_): a(doubleAt(p, true));
						default:
							if (p.isReference()) {
								var idType = p.type.getPTypedef().unpackPTypedef().as(ClassDef).getIDPropertyFromTWithProperties().type;
								a("    VORef("); a(anyAt(p, true)); a(")("); a(p.type.scalaType().name); a(", "); a(idType.scalaConversionExpr()); a(")");
							}
							else if (!p.lazyInit()) {
								a(p.type.scalaConversionExpr(anyAt(p, true)));
							}
							else {
								a(p.name.quote()); a(",\n");
							}
	      			}
	      			if (!p.lazyInit()) {
	      				a(" catch { case NoInputException => empty."); a(p.name.quote()); a(" },\n");
	      			}
	      		}
				a("      voRoot = newRoot\n    );\n  }\n  def copy("); copyPrototype(); a(") : this.type = this.copy(");
				var first = true;
				for (p in def.propertiesSorted) {
					if (!first) a(", "); else first = false;
					switch(p.type) {
						case TenumConverter(_), Temail, Tdatetime, Tdate, Tcolor, TclassRef(_), TfileRef, Turl, Turi, TuniqueID, Tinterval, Tstring, Tdef(_), Tarray(_):
							var name = p.name.quote();
							a("(if ("); a(name); a(" != null) "); a(name); a(" else this."); a(p.name.quote(p.lazyInit()? "0" : null)); ac(")".code);

						case Tbool(_), Tinteger(_,_,_), Tdecimal(_,_,_):
							a(p.name.quote());
					}
				}
				a(", voRoot = this.voSource);\n");

				// -- end copy(...)
			}
			else { //fields.length = 0
				a("  def conj(voSource : ValueSource, newRoot : ValueSource) : this.type = if (voSource == this.voSource) this else new "); a(def.name); a("VO(newRoot).asInstanceOf[this.type];\n");
			}
			a("}\n"); // end final class
		} // End class definition

		// -----
		// Companion definition

		a("\nobject "); a(def.name);

		if (!def.isMixin)
		{
			a(" extends ValueObjectCompanion["); a(def.name); a("] {\n");

			// val empty
			a("  final val empty : "); a(def.name); a("VO = new "); a(def.name); a("VO("); if (fields.length > 1) a("0,0,"); a("ValueSource.empty");
			for (p in fields)
			{
				a(", ");
				if (p.isReference())
					a("null");
				else if (!p.type.isTclass())
					writeValueLiteral(p.type, p.defaultValue);
				else {
					a(p.type.scalaType().name); a(".empty");
				}
			}
			a(");\n");

			if (def.implementedBy.iterator().hasNext()) {
				// def subtype(...)
				a("  override def subtype(typeID:Int) = typeID match {\n");
					a("    case "); a(def.index + " => this;\n");
					addSubtypeCases(def.implementedBy);
				a("  }\n\n");
			}

			// def apply(...)
			a("  def apply("); copyPrototype(true,true,true); a(") = empty.copy("); copyPrototype(false,false); a(");\n\n");
		}
		else {
			a(" {\n");
		}
		// object manifest
		function manifestFieldType(p:Property) {
			var voField = !p.isReference() && p.type.isTclass();
			a(!voField? "ValueObjectField[" : "VOValueObjectField["); a(def.name);
			if (voField) { a(", "); a(p.type.scalaType().name); }
			a("]");
		}
		a("  object field {\n");// type ID
		for (p in fields) if (p.definedIn == def)
		{
			var voField = !p.isReference() && p.type.isTclass();

			a("    object    "); a(p.name.quote()); spaces(longestFieldNameLength - p.name.quote().length); a(" extends "); manifestFieldType(p);
			a("(0x"); a(StringTools.hex(p.propertyID())); a(", '"); a(p.name); a(", ");

			if (p.isReference() || !p.type.isTclass()) {
				addFieldTypeConstructor(p.type, p.isReference());
				a(", ");
			}

			if (def.isMixin) {
				if (!voField)
					writeValueLiteral(p.type, p.defaultValue);
				else {
					a(p.type.scalaType().name); a(".empty");
				}
			}
			else if (p.isReference()) {
				a("null");
			}
			else {
				a("empty");
				// Check for the special case of referring to itself
				if (p.type.isArray() || p.type.getPTypedef().unpackPTypedef() != def){
					ac('.'.code); a(p.name.quote());
				}
			}

			a(") { def apply(vo: "); a(def.name); a("): "); a(!voField? "Any" : p.type.scalaType().name); a(" = vo."); a(p.name.quote()); a("; }\n");
		} else {
			a("    final val "); a(p.name.quote()); spaces(longestFieldNameLength - p.name.quote().length); a(" = ");
			a(p.definedIn.fullName); a(".field."); a(p.name.quote()); a(";\n");
		}
		a("  }\n");// end object field

		// val fields : Array
		{
			var first = true;
			var bit   = 0;
			a("  final val fields = { import field._; Array(");
			for (p in fields) if (!p.isTransient()) {
				if (!first) a(", "); else first = false;
				while(bit++ < p.bitIndex()) {
					//trace((bit - 1) +" != "+ p.bitIndex());
					a("null, ");
				}
				a(p.name.quote());
			}
			for (p in fields) if (p.isTransient()) {
				if (!first) a(", "); else first = false;
				a(p.name.quote());
			}
			a("); }\n\n");
		}

		a("  object manifest extends {\n");
		a("    final val ID  = "); a(def.index + ";\n");
		// val _id
		if (idField != null)
		{
			a("    final val _id = field."); a(idField.name.quote()); a(";\n");
		}
		// Fields
		if (fields.length > 1) {
			a("    val fields = "); a(def.name); a(".fields");
		} else if (fields.length == 1) {
			a("    final val first          = "); a(def.name); a(".field."); a(fields[0].name.quote()); a(";\n  ");
			a("    final val lastFieldIndex = "); a(Std.string(fields[0].bitIndex()));
		}
		// Mixins
		a("\n    val mixins = Array[ValueObjectMixin](");
		addMixinManifests(def, def.supertypes);
		a(");\n  } with ValueObjectManifest_"); a(fields.length == 0? "0[" : fields.length == 1? "1[" : "N["); a(def.name);
		if (idField != null) a("] with IDField;\n");	else a("];\n");
		a("}\n");

		write(def.module.fullName, def);
	}

	function addSubtypeCases(types : Hash<BaseTypeDefinition>) {
		for (subType in types) {
			addSubtypeCases(subType.implementedBy);
			a("    case "); a(subType.index + " => "); a(subType.fullName); a(";\n");
		}
	}

	function addMixinManifests(owner : BaseTypeDefinition, types : List<BaseTypeDefinition>, first = true) {
		for (mixin in types) {
			addMixinManifests(owner, mixin.supertypes, first);
			if (!mixin.supertypes.isEmpty()) first = false;
			if (first) first = false; else a(", ");
			var mask = 0; for (p in mixin.propertiesDefined) if (!p.isTransient()) mask |= 1 << owner.bitIndex(p);

			var lastProp = null;
			var offset = 0; for (p in owner.propertiesSorted) if (p.definedIn == mixin) { if (lastProp != null) offset = lastProp.bitIndex() + 1; break; } else if (!p.hasOption(transient)) lastProp = p;
			a("ValueObjectMixin("); a(offset + ", 0x"); a(StringTools.hex(mask)); a(", "); a(mixin.fullName); a(".manifest)");
		}
	}

	function addFieldTypeConstructor(t:PType, isRef:Bool) : Void
	{
		switch(t)
		{
			case Tdef(cl): switch (cl) {
				case Tclass(cl):	a("Tdef("); a(cl.fullName); a(".empty, "); a(Std.string(isRef)); a(")");
				case Tenum(e):		a("Tenum("); a(e.fullName); ac(")".code);
			}
			case Tbool(v):			a("Tbool("); a(Std.string(v)); ac(")".code);

			case Tinteger(l,u,s):
				a("Tinteger(");
				if (l != null){											a("min = "); a(Std.string(l)); }
				if (u != null){ if (l != null)				a(", ");	a("max = "); a(Std.string(u)); }
				if (s != null){ if (u != null || l != null)	a(", ");	a("stride = "); a(Std.string(s)); }
				ac(")".code);

			case Tdecimal(l,u,s):
				a("Tdecimal(");
				if (l != null){											a("min = "); a(Std.string(l)); }
				if (u != null){ if (l != null)				a(", ");	a("max = "); a(Std.string(u)); }
				if (s != null){ if (u != null || l != null)	a(", ");	a("stride = "); a(Std.string(s)); }
				ac(")".code);

			case Tarray(t,l,u):
				a("Tarray("); addFieldTypeConstructor(t,isRef);
				if (l != null || u != null) a(', ');
				if (l != null){							a("min = "); a(Std.string(l)); }
				if (u != null){ if (l != null) a(", ");	a("max = "); a(Std.string(u)); }
				ac(")".code);

			case TuniqueID:
				a("TobjectId");

			default:
				a(Std.string(t));
		}
	}

	public function genEnum(def:EnumDef)
	{
		shouldWrite = true;

		var a = code.add;
		function addConversionParams(withVal) {
			for (prop in def.conversions) if (prop.name != "toString") {
				a(", ");
				if (withVal) a("val ");
				a(prop.name.quote()); a(":String"); // a(conv); ac('"'.code);
			}
		}

		a(Std.format("

//---

sealed abstract class ${def.name}(val value:Int, override val toString:String"));

		addConversionParams(true);

		a(Std.format(") extends EnumValue { final def owner = ${def.name} };\n
object ${def.name} extends Enum {
  type Value = ${def.name};\n  val  ID    = ${def.index};\n\n"));

		var overrideValueOf = null;

		var enumerations = Lambda.array(def.enumerations);
		enumerations.sort(function(a,b){ return a.intValue - b.intValue; });
		for (e in enumerations)
		{
			if (e.type != null)
			{
				a("\n  override protected def stringCatchAll(value : String) = "); a(e.name); a("(value);");
				a("\n  case class  _"); a(e.name); a('(override val toString : '); a(e.type.typeNameInMutablePkg().name); a(') extends Value(');
				a(e.intValue + ', toString');
				for (key in e.conversions.keys()) if (key != "toString") {
					var conv = e.conversions.get(key);
					a(', "'); a(conv); ac('"'.code);
				}
				a(");  def "); a(e.name); a("(value : String) = _"); a(e.name); a("(value);\n");

				overrideValueOf = e;
			}
			else {
				a("  final val "); a(e.name); a(' = _'); a(e.name); a(";  protected object _"); a(e.name); a(' extends Value('); a(e.intValue + ', "'); a(e.conversions.get("toString")); ac('"'.code);
				for (prop in def.conversions) if (prop.name != "toString") {
					var conv = e.conversions.get(prop.name);
					a(', "'); a(conv != null? conv : e.name); ac('"'.code);
				}
				a(");\n");
			}
		}
		a("\n  final def apply(v: Int) : Value = v match {");
		for (e in enumerations) if (e.type == null)
		{
        	a("\n    case " + e.intValue); a(" => _"); a(e.name);
		}
		a("\n    case _ => this.Null");
		a("\n  }");

		var first = true;
		a("\n  final val valueSet : Set[Value] = Set(_");
		for (e in def.enumerations) if (e.type == null) {
			if (first) first = false; else a(", _");
			a(e.name);
		}
		a(");\n\n");

  		for (conv in def.conversions)
		{
			a("  final def from"); a(conv.name.substr(2)); a("(str:String) : Value = valueSet.find(_."); a(conv.name); a(" == str).getOrElse(apply(str));\n");
		}
		a("}");

		write(def.module.fullName, def);
	}

	public function newModule(module:Module) {
		return new Scala(module);
	}
}


class MutableScala extends ScalaBase, implements CodeGenerator
{
	//private static var writelist = new List<MutableScala>();

	public static function generate() {
		Module.root.generateWith(new MutableScala(Module.root));

		var filename = "MutableValueObjects.scala";
		trace("WRITING: "+filename);
		var file = neko.io.File.write(filename, false);

		for (m in Module.pkgRoots)
		{
			var map = new ScalaTypeMap();
			m.generateWith(map);
			file.writeString("
package "+ m.mutableFullName +"\n{\n
  object VO { final val typeMap = scala.collection.immutable.IntMap(");
			var first = true;
			for (index in map.map.keys()) {
				if (first) first = false;
				else file.writeString(",");
				file.writeString("\n    " + index + " -> " + map.map.get(index) + "VO");
			}

file.writeString("
  )}
}
");
		}
		//for (m in writelist) m.write(file);
		file.close();
	}
	
	private function new(m:Module) {
		super(m);
		dir  = m.mkdir(true);
		//writelist.add(this);
	}

	var currentFieldBitNum : Int;
	
	public function newModule	(m:Module)		: CodeGenerator
	{
		trace("\n- Scala module: "+m.fullName);
		return new MutableScala(m);
	}

	override function writeHeader(file) file.writeString(
"// GENERATED BY VOCompiler - MODIFICATIONS WILL BE LOST

import scala.collection.JavaConversions
import scala.xml.NodeSeq
import prime.types.{Type, Field, Ref, Enum, EnumValue, FileRef}
import prime.types.Conversion._;
import prime.utils._
import prime.utils.msgpack._
import prime.vo.mutable._

")

	public function genClass	(def:ClassDef)	: Void
	{
		shouldWrite = true;
		
		trace("  - Scala class:  "+def.name);
		
	// --- VO class definition
		var thisPropsStartIndex = null, idType:String = null, idFromSuperclassOrTrait = false, idProperty:Property = null, 
			nonEmptyChecks = 0, emptyChecks = new List<{expr:String, id:Int}>();
		
		var hasSubtypes = false;
		var subtypes : List<ClassDef> = new List();
		for (imp in def.implementedBy) if (cast(imp, ClassDef).extendsType(def)) {
			subtypes.add(cast imp);
			hasSubtypes = true;
		}
		
		var superClassHasBitflags = false;
		var fullName = def.mutableFullName;
		// Check if this is a NamedSetDef
		var ns:NamedSetDef = Std.is(def, NamedSetDef)? cast def : null;
		
		for (i in 0 ... def.propertiesSorted.length)
		{
			var p = def.propertiesSorted[i];
			
			if (thisPropsStartIndex == null && !Util.isDefinedInSuperClassOf(def, p))
				thisPropsStartIndex = i;
			
			// Check if empty_? needs to be overridden
			if (!Util.isDefinedInSuperClassOf(def, p)) switch (p.type)
			{
				case Tdef(cl): switch (cl) {
					case Tclass(cl):	emptyChecks.add({expr: "(__"+ p.name +" == null || __"+ p.name + ".empty_?)", id: i});
					case Tenum(e):		emptyChecks.add({expr: "(__"+ p.name +" == null || __"+ p.name + ".toString.isEmpty)", id: i});
				}
				
				case Tarray(_,_,_):
					emptyChecks.add({expr: "(__"+ p.name +" == null || __"+ p.name +".length == 0)", id: i});
				
				case Tstring, TfileRef, Tdate, Tdatetime, Tinterval, Tcolor, Temail, Turi, Turl, TuniqueID, Tinteger(_,_,_), Tdecimal(_,_,_), Tbool(_):
					++nonEmptyChecks;
				
				case TenumConverter(_):		throw p;
				case TclassRef(_):			continue; //throw p;
			}
			else switch (p.type) {
				case TfileRef, Tdate, Tdatetime, Tinterval, Tcolor, Temail, Turi, Turl, TuniqueID, Tinteger(_,_,_), Tdecimal(_,_,_), Tbool(_):
					superClassHasBitflags = true;
				default:
			}
			
			if (p.hasOption(unique))
			{
				if (idProperty != null) throw "VO's may have only 1 ID property...\n  idProperty = "+idProperty+"\n"+def;
				idProperty = p;
				idType = p.type.typeNameInMutablePkg().name;
				Assert.notNull(idType);
				
				if (p.definedIn != def)
					idFromSuperclassOrTrait = true;
			}
		}
		
		a("\n\n//---\n\ntrait "); a(def.name); a(" extends ");
		// extra stuff
		if (ns != null)
		{
			if (ns.keys.length > 0) {
			//	a("scala.collection.mutable.HashSet[");
			//	a(typeNameInMutablePkg(ns.baseType).name);
				a("prime.vo.mutable.NamedSet["); a(ns.baseType.typeNameInMutablePkg().name); // a("] with ");
			//	a("scala.collection.mutable.SetLike["); a(typeNameInMutablePkg(ns.baseType).name); a(", "); a(def.name); a("VO");
			}
			else {
				a("ValueObject"); if (idType != null) a("WithID");
				a(" with scala.collection.Traversable[");
				a(ns.baseType.typeNameInMutablePkg().name);
			}
			a("]");
		}
		else if (def.superClass != null) {
			a(def.superClass.mutableFullName);
			if (!def.superClass.hasUniqueID() && idType != null) {
				// Superclass has no Unique ID. Add the required trait now.
				a(" with ValueObjectWithID");
			}
		}
		else {
			a("ValueObject"); if (idType != null) a("WithID");
		}
		
		
		//TODO: Generate traits for supertypes ?
		for (t in def.supertypes) if (Std.is(t, MagicClassDef) || def.superClass != t) {
			 a("\n with "); a(t.mutableFullName);
		}
		
		a("\n{");
		
		// properties
		var requiredProps = new List<Int>();
		currentFieldBitNum = thisPropsStartIndex;
		
		for (p in def.propertiesDefined)
		//	if (!p.isTransient())
				writeVarGetter(p);
		a("\n}\n\n");
		
		if (def.isMixin) {
			genCompanion(def, idProperty, ns, thisPropsStartIndex, hasSubtypes, subtypes);
			write(def.module.mutableFullName, def);
			return;
		}
		
		// -- end trait
		//
		// VO Class
		
		a("\n\n"); if (!def.implementedBy.iterator().hasNext()) a("final "); a("class "); a(def.name); a("VO");
		
		if (idType != null) {
			a(def.superClass != null && def.superClass.hasUniqueID()? " (_id:" : " (val _id:"); a(idType); a(")");
		}
		a(" extends ");
		
		if (def.superClass != null) {
			a(def.superClass.mutableFullName); a("VO");
			if (idType != null && def.superClass.hasUniqueID()) a("(_id)");
			a(" with ");
		}
		else if (!def.isMixin && ns == null) {
			a("AbstractValueObject with ");
		}

		a(def.name); a("\n{");
		
		for (i in thisPropsStartIndex ... def.propertiesSorted.length) {
			var p = def.propertiesSorted[i];
		//	if (p.isTransient())
		//		continue;
			var r = p.hasOption(required);
			if (r) requiredProps.add(i);
			writeSetter(i, p);
		}
		
		
		
		// IDType
		if(idType != null) {
			if (!idFromSuperclassOrTrait) {
				a("\n  type IDType = "); a(idType);
			}
			a("\n  def this()  = this("); a(nilValue(idProperty.type)); a(")");
		}
		
		// Companion getter
		a("\n  override def voCompanion : VOCompanion[_] with VOMessagePacker[_] = "); a(def.name); a("VO");
		
/*		// partial_?
		if (def.propertiesSorted.length != 0) {
			a("\n  override def partial_? = numFieldsSet_? != "); a(Std.string(def.propertiesSorted.length));
		}
*/		
		// Non-bitflag empty checks overrides
		if (emptyChecks.length > 0)
		{
			var andAnd;
			
			// updateFieldsSet_!
			a("\n  override def updateFieldsSet_! = {");
			if (def.superClass != null || nonEmptyChecks != 0) {
				a("\n    super.updateFieldsSet_!;");
			}
			for (check in emptyChecks) {
				a("\n    if (!"); a(check.expr); a(") $fieldsSet |= 0x"); a(StringTools.hex(1 << def.propertiesSorted[check.id].bitIndex())); a(";");
			}
			a("\n  }");
			
			// fieldIsSet_?
			a("\n  override def fieldIsSet_?(index:Int) = index match {");
			for (check in emptyChecks) {
				a("\n    case " + def.propertiesSorted[check.id].bitIndex()); a(" => !"); a(check.expr);
			}
		 	a("\n    case _ => super.fieldIsSet_?(index)");
			a("\n  }");
		}
		
		
		// validation
		if (requiredProps.length != 0)
		{
			a("\n  override def validationErrors_? = {\n    var errors : List[(Symbol,String)] = Nil");
			for (i in requiredProps)
			{
				a("\n    if (!fieldIsSet_?("); a(Std.string(i)); a(")) errors ::= ('"); a(def.propertiesSorted[i].name); a(', "error.required")');
			}
			a("\n    errors\n  }\n");
		}
		
		// extra stuff
		if (Std.is(def, NamedSetDef))
			genNamedSetDefMethods(cast def);
		
		a("\n}\n\n");
		
	// --- End VO class
		genCompanion(def, idProperty, ns, thisPropsStartIndex, hasSubtypes, subtypes);

		write(def.module.mutableFullName, def);
	}
	
	private function genCompanion(def:ClassDef, idProperty:Property, ns:NamedSetDef, thisPropsStartIndex, hasSubtypes, subtypes:List<ClassDef>)
	{
		// VO MessagePacker object
		a("object "); a(def.name);
		
		if (def.isMixin)
		{
			a(" extends VOMessagePacker["); a(def.name); a("]\n{");
			
			// MessagePack serialization
			new ScalaMessagePacking(code, def).genSerialization();
			
			a("}\n\n");
			return;
		}
		
		// ---
		// Regular VO class Companion
		// ---
		a("VO extends VOCompanion["); a(def.name); a("VO"); a("] with VOMessagePacker["); a(def.name); a("]");
		
		if (idProperty != null) {
			a(" with "); a(def.name); a("IDAccessor");
		}
		a("\n{");
		
		// MessagePack serialization
		new ScalaMessagePacking(code, def).genSerialization();
		
		a("\n  override def empty: "); a(def.name); a("VO = new "); a(def.name); a("VO;\n");
		
		// afasjfakjfhaf
		if (ns != null && ns.keys.length > 0)
		{
//			a("\n  override def getValue(vo:"); a(def.name); a("VO, key:String): AnyRef = vo.get(key)");
			a("\n  override def putValue(vo:"); a(def.name); a("VO, key:String, value:AnyRef): "); a(def.name); a("VO = { vo.addEntry(ConvertTo.vo["); a(ns.baseType.typeNameInMutablePkg().name); a("](value)); vo; }");
//			a("\n  override def fieldsSetNames(vo:"); a(def.name); a("VO) = vo.keySet");
		}
		
		// field()
		if (thisPropsStartIndex != null)
		{
			for (i in 0 ... def.propertiesSorted.length)
			{
				var p = def.propertiesSorted[i];
				a("\n  final val "); a(p.name.quote());
				if (p.parent == def)
				{
				 	a(" = Field('"); a(p.name); a(", ");
					addFieldTypeConstructor(p.type, p.hasOption(reference));
					if (p.hasOption(required)) {
						a(", required = true");
					}
					a(")");
				}	
				else {
					a(" = "); a(p.parent.mutableFullName); a("VO."); a(p.name.quote());
				}
			}
			
			// fields : Array
			a("\n\n  final override val fields = Array[Field](");
			for (i in 0 ... def.propertiesSorted.length)
			{
				var p = def.propertiesSorted[i];
				if (i != 0) a(", "); a(p.name.quote());
			}
			a(");");
			
			// field(Int)
			a("\n\n  override def field(index: Int) = index match {");
			for (i in 0 ... def.propertiesSorted.length)
			{
				var p = def.propertiesSorted[i];
				a("\n    case "+ p.bitIndex()); a(" => this."); a(p.name.quote());
			}
			a("\n    case _ => super.field(index)");
			a("\n  }");
			
			// field(String)
			a("\n\n  override def field(key: String) = key match {");
			for (i in 0 ... def.propertiesSorted.length)
			{
				var p = def.propertiesSorted[i];
				a('\n    case "'); if (p.hasOption(unique)) a('_id" | "');  a(p.name); a('" => '+ p.bitIndex());
			}
			a("\n    case _ => super.field(key)");
			a("\n  }");
		}
		
		if (def.propertiesSorted.length == 0)
		{
			a("\n  def getValue(vo:"); a(def.name); a("VO, index:Int) : AnyRef = null");
			a("\n  def putValue(vo:"); a(def.name); a("VO, index:Int, value:AnyRef) : "); a(def.name); a("VO = vo\n");
		}
		else
		{
			// getValue()
			a("\n\n  def getValue(vo:"); a(def.name); a("VO, index:Int) : AnyRef = ");
			if (def.superClass != null && thisPropsStartIndex == null) {
				a(def.superClass.mutableFullName); a("VO.getValue(vo, index);\n");
			}
			else
			{
				a("(index match {\n");
				for (i in 0 ... def.propertiesSorted.length) {
					var p = def.propertiesSorted[i];
					a('    case ' + p.bitIndex()); a(' => vo.'); a(p.name.quote()); a('\n');
				}
				a("    case _ => null\n  }).asInstanceOf[AnyRef]\n\n");
			}

			// putValue(vo, index:Int, value)
			a("  def putValue(vo:"); a(def.name); a("VO, index:Int, value:AnyRef) : "); a(def.name); a("VO = ");
			if (def.superClass != null && thisPropsStartIndex == null) {
				a(def.superClass.mutableFullName); a("VO.putValue(vo, index, value).asInstanceOf["); a(def.name); a("VO];\n");
			}
			else
			{
				a("{ index match {\n");
				for (i in 0 ... def.propertiesSorted.length) {
					var p = def.propertiesSorted[i];
					a('    case ' + p.bitIndex()); a(' => vo.'); a(p.name.quote("_")); a("(value);\n");
				}
				a("  }; vo; }\n");
			}
		}
		
		a("}\n\n");
		
		// VOAccessor
		a("trait "); a(def.name); a("VOAccessor //extends VOAccessor["); a(def.name); a("VO]\n{\n");
			// _fields
			a("  def field    (vo: "); a(def.name); a("VO, key:String) : Int    = "); a(def.name); a("VO.field(key);\n");
			a("  def field    (vo: "); a(def.name); a("VO, index: Int) : Field  = "); a(def.name); a("VO.field(index);\n");
			a("  def fieldsFor(vo: "); a(def.name); a("VO) : IndexedSeq[Field]  = "); a(def.name); a("VO.fields;\n");
			a("  def isSet    (vo: "); a(def.name); a("VO, key:String): Boolean = vo.fieldIsSet_?(field(vo, key));\n");
			
			a("  def getValue (vo: "); a(def.name); a("VO, key:String) : AnyRef = "); a(def.name); a("VO.getValue(vo, key)\n");
			a("  def putValue (vo: "); a(def.name); a("VO, key:String, value:AnyRef) : "); a(def.name); a("VO = "); a(def.name); a("VO.putValue(vo, key, value)\n");
		a("}\n");	
		
		// IDAccessor & VOProxy
		if (idProperty != null)
		{
			var idType = idProperty.type.typeNameInMutablePkg().name;
			
			a("trait "); a(def.name); a("IDAccessor extends IDAccessor["); a(def.name); a("VO] {\n");
				a("  val idField = " ); a(idProperty.definedIn.mutableFullName); a("VO."); a(idProperty.name); a('\n');
				a("  def idValue(vo:"); a(def.name); a("VO): "); a(idType); a(" = if(vo == null) "); a(nilValue(idProperty.type)); a(" else vo."); a(idProperty.name); ac("\n".code);
				a("  def idValue(vo:"); a(def.name); a("VO, idValueToSet:"); a(idType); a(") = { vo."); a(idProperty.name); a(" = idValueToSet; }\n");
			a("}\n");
			// VOFieldInfo
			a("trait "); a(def.name); a("FieldInfo extends VOFieldInfo {\n");
//				a('  override val numFields = '); a(def.name); a('VO.numFields;\n');
				a("  override def field(index: Int): Field = "); a(def.name); a('VO.field(index);\n');
				a("  override def field(key: String): Int = " ); a(def.name); a('VO.field(key);\n');
			a("}\n");
			a("trait "); a(def.name); a("VOProxy extends "); a(def.name); a("IDAccessor with "); /*a(def.name); a("VOAccessor with ");*/ a(def.name); a("FieldInfo\n");
		}
		
		// XML mapping
		genXMLComponent(def, idProperty);
		
//		if (def.options.length > 0)
//			trace(def.options.first().toString());
	}
	
	
	private function genSerialization(def:ClassDef)
	{	
		new ScalaMessagePacking(code, def).genSerialization();
	}
	
	
	function addPropnameCase(p:Property) {
		a('      case "'); a(p.name); a('" => ');
	}
	function addVOGetterCase(p:Property) {
		addPropnameCase(p); a('vo.'); a(p.name.quote());
	}
	
	function writeLiteral(type:PType, val:Dynamic) switch (type)
	{
		case Tbool(_), Tinteger(_,_,_), Tdecimal(_,_,_):
			a(Std.string(val));
		
		case TfileRef:
			ac('"'.code); a(Std.string(val)); ac('"'.code);
		
		case Tstring:
			ac('"'.code); a(Std.string(val)); ac('"'.code);
		
		case Turi, Turl:
			ac('"'.code); a(Std.string(val)); ac('"'.code);
		
		case Tdate, Tdatetime, TuniqueID, Temail:
			ac('"'.code); a(Std.string(val)); ac('"'.code);
		
		case Tcolor:
			if (Std.is(val, String)) {
				ac('"'.code); a(Std.string(val)); ac('"'.code);
			}
			else
				StringTools.hex(val, 6);
		
		case TenumConverter(enums):
			throw "Unsupported value literal";
		
		case Tdef(_), Tinterval, Tarray(_,_,_), TclassRef(_):
			throw "Unsupported value literal: "+type;
	}

	function getSimpleValue(t:PType, path:String) return switch(t) {
		case Tdef(ptypedef):		switch (ptypedef) {
			case Tenum(def):		path;
			default:				path;
		}
		case Turi, Turl, Temail, TuniqueID, TfileRef, Tdate, Tdatetime:
			'"" + ' + path; //+".toString";
		case Tstring:				path;
		case Tinteger(_,_,_):		path;
		case Tdecimal(_,_,_):		path;
		case Tbool(_):				path;
		case Tcolor:				path;
		
		case Tinterval:				throw t;
		case Tarray(innerT,_,_):	throw t;
		case TenumConverter(prop):	throw t;
		case TclassRef(_):			throw t;
	}
	
	function genNamedSetKeyMatcher(def: NamedSetDef, wildcardMethod:String, action:Int->Property->String)
	{
		if (def.keys.length == 1)
		{
			var k = def.keys[0];
			a(getSimpleValue(k.prop.type, "item." + k.path)); a(" match {");
			
			for (i in 0 ... def.propertiesSorted.length)
			{
				var p = def.propertiesSorted[i];
				a("\n    case "); writeLiteral(k.prop.type, def.getValueByPath(p.defaultValue, k.path)); a(" => "); a(action(i,p));
			}
		}
		else
		{
			a("(");
			for (i in 0 ... def.keys.length) {
				var k = def.keys[i];
				a(getSimpleValue(k.prop.type, "item." + k.path));
				if (i+1 != def.keys.length) a(", ");
			}
			a(") match {");
			
			for (i in 0 ... def.propertiesSorted.length)
			{
				var p = def.propertiesSorted[i];
				a("\n    case (");
				
				var firstKey = true;
				for (k in def.keys)
				{
					if (!firstKey) a(", ");
					else firstKey = false;
					
					writeLiteral(k.prop.type, def.getValueByPath(p.defaultValue, k.path));
				}
				
				a(") => "); a(action(i,p));
			}
		}
		a("\n    case _ => "); a(wildcardMethod); a("(item)\n");
	}
	
	function genNamedSetDefMethods(def: NamedSetDef)
	{
		var type = def.baseType.typeNameInMutablePkg();
		
		a("\n  override def foreach[U](f: "); a(type.name); a(" => U) {");
		for (i in 0 ... def.propertiesSorted.length) {
			var p = def.propertiesSorted[i];
			a("\n    if (__"); a(p.name); a(" != null && fieldIsSet_?("); a(p.bitIndex() +")) f(__"); a(p.name); a(");");
		}
		if (def.keys.length > 0)
			a("\n    super.foreach(f);");
		a("\n  }\n");
		
		if (def.keys.length == 0) return; // No keys, no way to map...
		
//		a("\n  def this$ = "); a(def.name); a("VO.asInstanceOf[VOCompanion[org.valueobjects.traits.NamedSet["); a(type.name); a("]]];");
//		a("\n  override def clear() { "); a(def.name); a("VO.clear(this); super.clear(); }\n");
		a("\n  override def empty: "); a(def.name); a("VO = new "); a(def.name); a("VO;\n");
//		a("\n  override def clone(): this.type = new "); a(def.name); a("VO ++= this;\n");
//		a("\n  override def companion() = "); a(def.name); a("VO;\n");
		
		
		var cl:ClassDef = switch (def.baseType)
		{
			case Tdef(tdef): switch(tdef) {
				case Tclass(cl): cl;
				default: null;
			}				
			default: null;
		}
		if (cl == null) throw def; // Not a set of VO's, so we're done.
		
		a("\n  final override def containsEntry(item:"); a(type.name); a(") = ");
		genNamedSetKeyMatcher(def, "super.containsEntry", function(i,p) return "__"+ p.name +" != null && fieldIsSet_?("+ p.bitIndex() +");");
		a("  }\n");
		
		a("\n  final override def addEntry(item:"); a(type.name); a(") = {");
		genNamedSetKeyMatcher(def, "super.addEntry", function(i,p) return "this."+p.name.quote()+" = item;");
		a("  }; true; }\n");
		
		a("\n  final override def removeEntry(item:"); a(type.name); a(") = ");
		genNamedSetKeyMatcher(def, "super.removeEntry", function(i,p) return "val old = __"+ p.name +"; if (old != null && fieldIsSet_?("+ p.bitIndex() +")) Some(old) else None;");
		a("  }\n");
		
		a("}\n");
		
		a("\ntrait "); a(def.name); a("VOXMLComponent extends XMLComponent\n{");
		a("\n  this: "); a(cl.mutableFullName); a("VOXMLComponent =>\n");
		
		a('\n  def setValueObject(valueobject: '); a(def.name); a('VO, xml: NodeSeq) : '); a(def.name); a('VO = ');
		a("\n  {");
		a("\n    val vo = if (valueobject != null) valueobject else new "); a(def.name); a("VO;");
		a("\n    for (xml <- xml) for (node <- xml.child) if (node.isInstanceOf[scala.xml.Elem]) vo.add(setValueObject(new "); a(cl.mutableFullName); a("VO, node));");
		a("\n    vo");
		a("\n  }");
		
		a("\n  def toXML(vo:"); a(def.name); a("VO): NodeSeq = if (vo.empty_?) NodeSeq.Empty else vo.toSeq.flatMap(toXML(_));\n");
		
/*		a("\n  override def get(key: String): Option["); a(type.name); a("] = {");
		for (i in 0 ... def.propertiesSorted)
		{
			var p = def.propertiesSorted[i];
			
			a("\n    if (fieldIsSet_?("+i+")) "); a("this."); a(p.name.quote());
		}
		a("\n    ");
		
/*		
		To implement a concrete map, you need to provide implementations of the following methods:
		      def get(key: A): Option[B]
		      def iterator: Iterator[(A, B)]
		      def + [B1 >: B](kv: (A, B1)): This
		      def -(key: A): This

		If you wish that methods like take, drop, filter also return the same kind of map you should also override:
		      def empty: This

		It is also good idea to override methods foreach and size for efficiency.
		
		a("\n  def get(key: String): Option["); a(type); a("] = ");
		a("\n  def iterator: Iterator[(String, "); a(type); a(")] = ");
		a("\n  def + [B1 >: B](kv: (A, B1)): This = ");
		a("\n  def -(key: A): This = ");
*/	}
	
	static public function addComponentDependencies(code:StringBuf, dependencies:Hash<Bool>, suffix:String)
	{
		var a = code.add;
		var first = true;
		
		for (clname in dependencies.keys())
		{
			a("\n  ");
			if (first) {
				first = false;
				a("this: ");
			}
			else {
				a("with  ");
			}
			a(clname); a(suffix);
		}
		if (!first) a(" =>\n");
	}
	
	function addLowerCaseFirst(str:String)
	{
		code.addChar(str.charAt(0).toLowerCase().charCodeAt(0));
		code.addSub(str, 1);
	}
	
	function className(t:PType) return Util.unpackPTypedef(Util.getPTypedef(t)).fullName
	
	function writeVarGetter(p:Property)
	{
		a("\n  ");
		
		var propName = p.name.quote();
		
		// Storage
		a("/*@field*/ protected[this] var __"); a(p.name); a(": ");
		var tdef = getPropertyTypename(p);
		a(tdef.name);
		a(" = "); a((tdef.defaultValue != null)? tdef.defaultValue : nilValue(p.type));
		
		// Getter
		a("\n  final def "); a(propName); a(" : "); a(tdef.name); a(" = ");
		switch (p.type)
		{
			case Tdef(cl): switch (cl) {
				case Tclass(cl):
					var vo = cl.mutableFullName + "VO";
					add_ifVarNotNull(p.name, (!p.hasOption(reference))? "new " + vo : "new Ref[" + vo +"]()");
				
				case Tenum(e):
					add_ifVarNotNull(p.name, e.mutableFullName + ".Null");
			}
			
			case Tarray(innerT,_,_):
				add_ifVarNotNull(p.name, (p.hasOption(reference)? "RefArray[" : "Array[") + innerT.typeNameInMutablePkg().name +"]()");
			
			case Tstring:
				add_ifVarNotNull(p.name, '""');
			
			case Tdate, Tdatetime, Tinterval, Tcolor, Temail, Turi, Turl, TuniqueID, TfileRef, Tinteger(_,_,_), Tdecimal(_,_,_), Tbool(_):
				a('__'); a(p.name);
			
			case TclassRef(className):	throw p;
			case TenumConverter(_):		throw p;
		}
		
		// optional hasProperty()
		if (p.hasOption(optional)) {
			a("\n  final def has"); a(p.name.substr(0,1).toUpperCase()); a(p.name.substr(1)); a("_? = fieldIsSet_?("); a(p.bitIndex() + ")");
		}
	}
	
	function writeSetter(i:Int, p:Property)
	{
		var tdef = getPropertyTypename(p);
		var bit = (1 << this.currentFieldBitNum);
		
		// Setter
		a("\n  final def "); a(p.name.quote("_")); a("=(v:"); a(tdef.name); a(") : Unit = { ");
		
		switch (p.type) {
			case Tdef(_), Tarray(_,_,_): // Don't set any bits
			default:
				var nilChecked = false;
				if (nilValue(p.type) == "null")
				{
					nilChecked = true;
					a("if (");
					if (p.type == Tstring) a("v != null && !v.isEmpty");
					else {
						a("v != "); a(nilValue(p.type));
					}
				 	a(") ");
				}
				a("$fieldsSet |= 0x"); a(StringTools.hex(1 << p.bitIndex()));
				if (nilChecked) {
					a("; else $fieldsSet &= ~0x"); a(StringTools.hex(1 << p.bitIndex()));
				}
				a("; ");
		}
		a("__"); a(p.name); a(" = v } ");
		
		if (p.hasOption(reference))
		{
			var clname = p.type.typeNameInMutablePkg().name;
			a("\n  final def "); a(p.name.quote("_")); a("=(v:"); a(clname); a(") : Unit = this."); a(p.name.quote());
			switch (p.type) {
				case Tarray(innerType, _,_):
					var innerName = innerType.typeNameInMutablePkg().name;
					a(" = new RefArray["); a(innerName); a("](v.map("); a(innerName); a(".idValue(_)).toArray, v)");
				default:
					a(" = new Ref["); a(clname); a("]("); a(clname); a(".idValue(v), v)");
			}
			a("\n  final def "); a(p.name.quote("_")); a("(v:AnyRef) : Unit = ");
			a(p.name.quote());
			a(" = ");
			a(convertFromAnyRefTo(p.type, p.hasOption(reference)));
			a("(v);\n");
		}
		else
		{
			a("\n  final def "); a(p.name.quote("_")); a("(v:AnyRef) : Unit = { val __value");
			if (Util.isEnum(p.type)) { a(": "); a(p.type.typeNameInMutablePkg().name); }
			a(" = "); a(convertFromAnyRefTo(p.type));
//			if (Util.isEnum(p.type))
//				a("(v.toString).getOrElse(null);");
//			else
				a("(v);");
			
			a(" "); a(p.name.quote()); a(" = __value;");
			switch (p.type) {
				case Tdef(_), Tarray(_,_,_): // Don't set any bits
				default:
					a(" if (v == null) "); a("$fieldsSet &= ~0x" + StringTools.hex(1 << p.bitIndex())); //nilValue(p.type)); ac(")".code);
			}
			a(" }\n");
		}
		
		// Bit number of next flag
		++this.currentFieldBitNum;
	}
	
	function addClassProxy(clname:String, valSuffix:String = "Proxy")
	{
		addLowerCaseFirst(clname.substr(mod.mutableFullName.length + 1).split('.').join('_')); a(valSuffix); a(": VOProxy["); a(clname); a("VO]");
	}
	
	function add_ifVarNotNull(name:String, defaultValue:String)
	{
		a("{ if (__"); a(name); a(" == null) __"); a(name); a(" = "); a(defaultValue); a("; __"); a(name); a(" }");
	}
	
	function commaNewline(comma = true) {
		if (comma) ac(','.code);
		ac('\n'.code);
	}
	
	function addFieldTypeConstructor(t:PType, isRef:Bool) : Void
	{
		a("Type.");
		switch(t)
		{
			case Tdef(cl): switch (cl) {
				case Tclass(cl):	a("Tdef("); a(cl.mutableFullName); a("VO, "); a(Std.string(isRef)); a(")");
				case Tenum(e):		a("Tenum("); a(e.mutableFullName); ac(")".code);
			}
			case Tbool(v):			a("Tbool("); a(Std.string(v)); ac(")".code);
			
			case Tinteger(l,u,s):
				a("Tinteger(");
				if (l != null){											a("min = "); a(Std.string(l)); }
				if (u != null){ if (l != null)				a(", ");	a("max = "); a(Std.string(u)); }
				if (s != null){ if (u != null || l != null)	a(", ");	a("stride = "); a(Std.string(s)); }
				ac(")".code);
			
			case Tdecimal(l,u,s):
				a("Tdecimal(");
				if (l != null){											a("min = "); a(Std.string(l)); }
				if (u != null){ if (l != null)				a(", ");	a("max = "); a(Std.string(u)); }
				if (s != null){ if (u != null || l != null)	a(", ");	a("stride = "); a(Std.string(s)); }
				ac(")".code);
			
			case Tarray(t,l,u):
				a("Tarray("); addFieldTypeConstructor(t,isRef);
				if (l != null || u != null) a(', ');
				if (l != null){							a("min = "); a(Std.string(l)); }
				if (u != null){ if (l != null) a(", ");	a("max = "); a(Std.string(u)); }
				ac(")".code);
			
			default:
				a(Std.string(t));
		}
	}
	
	static public function convertFromAnyRefTo(t:PType, ref = false) return switch(t) {
		case Tdef(ptypedef):		switch (ptypedef) {
			case Tenum(def):		def.mutableFullName+".valueOf";
			default:				"ConvertTo." + (ref? "voRef" : "vo") + "["+ t.typeNameInMutablePkg().name + "]";
		}
		case Tarray(innerT,_,_):	switch (innerT) {
			case Tdef(_):			(Util.isEnum(innerT)? "ConvertTo.array[" : "ConvertTo.voArray[") + innerT.typeNameInMutablePkg().name + "]";
			default:				"ConvertTo.array["+innerT.typeNameInMutablePkg().name+"]";
		}
		case Turi, Turl:			"ConvertTo.uri";
		case Temail:				"ConvertTo.email";
		case Tinterval:				"ConvertTo.interval";
		case TuniqueID:				"ConvertTo.uniqueID";
		case TfileRef:				"FileRef";
		case Tstring:				"ConvertTo.string";
		case Tinteger(_,_,_):		"ConvertTo.integer";
		case Tdecimal(_,_,_):		"ConvertTo.decimal";
		case Tdate:					"ConvertTo.date";
		case Tdatetime:				"ConvertTo.datetime";
		case Tbool(_):				"ConvertTo.boolean";
		case Tcolor:				"RGBA";
		case TenumConverter(prop):	prop.parent.mutableFullName + ".from" + prop.name.substr(2); //"";
		case TclassRef(className):	"ConvertTo."+className; //throw t;
	}
	
	function arrayInnerType(p:Property) return switch(p.type) {
		case Tarray(innerT,_,_): innerT;
		default: null;
	}
	
	function isRefArray(p:Property) return switch(p.type) {
		case Tarray(_,_,_): p.hasOption(reference);
		default: false;
	}
	
	function getPropertyTypename(p:Property) return switch(p.type)
	{	
		case Tarray(innerT, _, _):
			if (p.hasOption(reference)) {name:"RefArray["+innerT.typeNameInMutablePkg().name+"]", defaultValue: null};
			else p.type.typeNameInMutablePkg();
		
		default:
			var type = p.type.typeNameInMutablePkg();
			if (p.hasOption(reference)) {
			 	type.name = "Ref["+type.name+"]";
			}
			type;
	}
	
	function nilValue(t:PType) return switch(t) {
		case Tstring, Turi, Turl, TuniqueID, Tinterval, Tdate, Tdatetime, Temail, Tdef(_), Tarray(_,_,_), Tcolor, TfileRef:
			"null";
		
		case Tinteger(_,_,_):		"0";
		case Tdecimal(_,_,_):		"Double.NaN";
		case Tbool(v):				Std.string(v);
		
		case TclassRef(_):			"null";
		case TenumConverter(_):		throw t; //"";
	}
	
	public function genEnum	(def:EnumDef)	: Void
	{ }
	
	function genXMLComponent(def:ClassDef, idProperty:Property)
	{
		var map = def.getOptionDef(XMLMapping);
		if (map == null) return;
		
		a("trait "); a(def.name);
		a("VOXMLComponent extends XMLComponent\n{");
		
		var g = new XMLProxyGenerator(code, def, map);
		g.genDependencies();
		g.fromXML();
		g.toXML();
//		g.implicits();
		
		if (idProperty != null)
		{
			a("  trait "); a(def.name); a("VOXMLProxy\n   extends XMLProxy["); a(def.name); a("VO] with "); a(def.name); a("VOProxy {\n");
			a("    def toXML(vo:"); a(def.name); a("VO): NodeSeq = outer.toXML(vo);\n");
			a('    def setValueObject(vo: '); a(def.name); a('VO, xml: NodeSeq) : Option['); a(def.name); a('VO] = outer.setValueObject(vo, xml);\n');
			a("  }\n");
		}
		
		ac('}'.code);
	}
}

class XMLProxyGenerator
{
	var code	: StringBuf;
	var def		: ClassDef;
	var map		: XMLMapping;
	var root	: XMLMapNode;
	var addSuperClassMapping : Bool;
	
	var xmlConverters : Hash<String>;
	var dependencies: Hash<Bool>;
	
	public function new(code:StringBuf, def:ClassDef, map:XMLMapping)
	{
		Assert.that(map != null);
		Assert.that(Std.is(map, XMLMapping), Std.string(map));
		
		this.code = code;
		this.def  = def;
		this.map  = map;
		this.root = map.getMergedMapping();
		
		if (root.children.length == 0) throw "no child?!";
		
		this.xmlConverters = new Hash();
		dependencies = new Hash();
		findDependencies(map.root);
		
		if (def.superClass != null && def.superClass.defaultXMLMap != null && !def.superClass.defaultXMLMap.root.isXMLTypeMap()) {
			addSuperClassMapping = true;
			dependencies.set(def.superClass.mutableFullName, true);
		}
	}
	
	function addDependencyForValue(value) switch (value)
	{
		case XM_children(path, prop),
		 	 XM_child	(path, prop):
			
			var cl = Util.unpackPTypedef(Util.getPTypedef(prop.type));
		/*	if (Std.is(cl, NamedSetDef)) {
				cl = Util.unpackPTypedef(Util.getPTypedef(cast(cl, NamedSetDef).baseType));
			}
		*/	dependencies.set(cl.mutableFullName, true);
		
		case XM_typeMap(map):			for (k in map.keys()) dependencies.set(map.get(k).mutableFullName, true);
		
		default:
	}
	
	function findDependencies(node:XMLMapNode)
	{
		if (node.value != null)
			addDependencyForValue(node.value);
		
		if (node.attributes != null) for (a in node.attributes)
			addDependencyForValue(a);
		
		for (c in node.children)
			findDependencies(c);
	}
	
	public function implicits()
	{
		for (i in xmlConverters) a(i);
	}
	
	public function genDependencies()
	{
		MutableScala.addComponentDependencies(code, dependencies, "VOXMLComponent");
	}
	
	public function fromXML()
	{
		a('\n  def setValueObject(valueobject: '); a(def.name); a('VO, xml: NodeSeq) : '); a(def.name); a('VO =');
		
	 	if (gen_fromXMLValueToTypeMapping(map.root))
			return;
		
		a("\n  {");
		a("\n    val vo = if (valueobject != null) valueobject else new "); a(def.name); a("VO;\n");
		if (addSuperClassMapping)
		{
			a("\n    setValueObject(vo.asInstanceOf["); a(def.superClass.mutableFullName); a("VO], xml);");
			// Don't use the merged map, since we can rely on superClass setValueObject calls.
			fromXMLNode(map.root, "vo.", 2);
		}
		else
		{
			fromXMLNode(root, "vo.", 2);
		}
		a("\n    vo");
		a("\n  }\n");
	}
	
	function fromXMLNode(node:XMLMapNode, varPrefix:String, indentLevel:Int)
	{
/*		for (c in node.children) if (isMultiChildXMLMap(c.value)) {
			//TODO: iterables
			ac('\n'.code);
			indent(indentLevel);
			add_fromXMLValue(c.value, varPrefix);
		}
*/		
		#if debug
		a('println("'); for (i in 0 ... indentLevel) ac(' '.code); a(def.mutableFullName); a(': " + xml.headOption.map(_.label).getOrElse("-"))\n');
		#end
		
		// Multiple childnodes mapped props:
		for (child in node.children) if (isMultiChildXMLMap(child.value)) {
	 		indent(indentLevel); a("//  Multi, firstNode: "); a(child.nodeName); a('\n');
			indent(indentLevel);
			add_fromXMLValue("multi", child.value, varPrefix);
			a('\n');
		}
		
		// The rest...
		
		if (node.value != null && node.valuePath != null && !isMultiChildXMLMap(node.value)) {
	 		a('\n');
			indent(indentLevel); a("//  Node value: "); a(node.nodeName); a('\n');
			indent(indentLevel);
			add_fromXMLValue("xml.text", node.value, varPrefix);
		}
		
		if (node.children.length == 1)
		{
			var root = node.children[0];
			
			ac('\n'.code);
			indent(indentLevel); a("//  <"); a(root.nodeName); a('>\n');
			indent(indentLevel);
			if (isChildXMLMap(root.value)) {
				add_fromXMLValue("I#^%@HKJD", root.value, varPrefix);
			}
			else if (!isMultiChildXMLMap(root.value))
			{
				a('xml.headOption map { xml =>\n');
				fromXMLNode(root, varPrefix, indentLevel+1);
				gen_fromNodeAttr(root, varPrefix, indentLevel+1);
				ac('\n'.code);
				indent(indentLevel); a('}\n');
			}
		}
		else if (node.children.length > 1)
		{
			ac('\n'.code);
			indent(indentLevel);
			if (node.nodeName == "")
			 	a('for (xml <- xml) ');
			a('for (xml <- xml.child) if (xml.isInstanceOf[scala.xml.Elem]) {\n');
			
			#if debug
			a('println("'); for (i in 0 ... indentLevel + 1) ac(' '.code); a('" + xml.label)\n');
			#end
			indent(indentLevel+1);	a('xml.label match {');
			
			for (child in node.children) if (!isMultiChildXMLMap(child.value)) {
				ac('\n'.code);
				indent(indentLevel + 2); a('case "'); a(child.nodeName); a('" => ');
				#if debug
				a('println("'); for (i in 0 ... indentLevel + 2) ac(' '.code); a('" + xml.label)\n');
				#end
				fromXMLNode(child, varPrefix, indentLevel+3);
				gen_fromNodeAttr(child, varPrefix, indentLevel+3);
			}
			indent(indentLevel+2); a('case _ =>\n');
			indent(indentLevel+1); a('}\n');
			indent(indentLevel);   a('}');
		}
	}
	
	function gen_fromXMLValueToTypeMapping(node:XMLMapNode) : Bool
	{
		var code_added = false;
		
		// Handle value->type mappings first
		if (node.attributes != null) for (attr in node.attributes.keys()) switch (node.attributes.get(attr))
		{
			case XM_typeMap(map):
				if (code_added) throw "Error: multiple type-maps found";
				
				var matchAdded = false;
				
				for (k in map.keys())
				{
					if (!matchAdded) {
						a(' (xml \\ "@'); a(attr); a('").text match\n  {');
						matchAdded = true;
					}
					
					var clname = map.get(k).mutableFullName;
					a('\n    case "'); a(k); a('" => outer.setValueObject(valueobject match { case v: '); a(clname); a("VO => v; case _ => new "); a(clname); a("VO }, xml);");
					
					code_added = true;
				}
				if (matchAdded) a("\n    case _ => valueobject\n  }\n");
			
			default:
		}
		
		for (child in node.children)
			if (code_added) throw "Error: multiple type-maps found";
			else code_added = gen_fromXMLValueToTypeMapping(child);
		
		return code_added;
	}
	
	function gen_toXMLValueToTypeMapping(node:XMLMapNode) : Bool
	{
		var code_added = false;
		
		// Handle value->type mappings first
		if (node.attributes != null) for (attr in node.attributes.keys()) switch (node.attributes.get(attr))
		{
			case XM_typeMap(map):
				if (code_added) throw "Error: multiple type-maps found";
				
				var added = new Hash<Bool>();
				var clname = null;
				
				a("vo match {");
				for (val in map) if (!added.exists(clname = val.mutableFullName)) {
					a("\n    case vo:"); a(clname); a('VO => toXML(vo)');
					added.set(clname, true);
				}
				a('\n    case _ => null\n  }\n');
				
				code_added = true;
				
			default:
		}
		
		for (child in node.children)
			if (code_added) throw "Error: multiple type-maps found";
			else code_added = gen_toXMLValueToTypeMapping(child);
		
		return code_added;
	}
	
	function gen_fromNodeAttr(node:XMLMapNode, varPrefix:String, indentLevel:Int)
	{
		if (node.attributes != null)
		{
			ac('\n'.code);
			indent(indentLevel); a("for (xml <- xml.attributes) xml.key match\n");
			indent(indentLevel); ac("{".code);
			for (name in node.attributes.keys()) if (matchAttribute(node.attributes.get(name)))
			{
				ac('\n'.code);
				indent(indentLevel+1); a('case "'); a(name); a('" => ');
				add_fromXMLValue("xml.value.text", node.attributes.get(name), varPrefix);
			}
			ac('\n'.code);
			indent(indentLevel+1); a('case _ =>\n');
			indent(indentLevel);   a('}\n');
		}
	}
	
	public function toXML()
	{
		a("\n  def toXML(vo:"); a(def.name); a("VO): NodeSeq = ");
		
		if (gen_toXMLValueToTypeMapping(map.root))
			return;
		
		if (root.children.length == 1)
			gen_toNode(root.children[0]);
		else {
			a("\n  <xml>");
			for (child in root.children) gen_toNode(child, 2);
			a("\n  </xml>.child");
		}
		
		a('\n\n');
	}
	
	function gen_toNode(node:XMLMapNode, indentLevel:Int=1)
	{
		ac('\n'.code);
		indent(indentLevel);
		
		if (hasValue(node) && (isChildXMLMap(node.value) ||isMultiChildXMLMap(node.value)))
		{
			a('{ ');
			addToXMLWithCheckFieldIsSet(node.value, "vo.");
			a(' }');
			return;
		}
		
		ac('<'.code); a(node.nodeName);
	
		var atCount = 0;
	
		if (node.attributes != null) for (name in node.attributes.keys())
		{
			var attr = node.attributes.get(name);
			ac('\n'.code); indent(indentLevel + 1);
			
			// Handle simple strings first
			switch(attr) {
				case XM_empty:
				case XM_String(v):
					ass(name); ac('='.code); ac('"'.code); a(v); ac('"'.code);
			
				default:
					ass(name); a('={');
					addToXMLWithCheckFieldIsSet(attr, "vo.");
					ac('}'.code);
			}
		}
	
		if (!hasValue(node) && node.children.length == 0) {
			a("/>");
			return; // done
		}
	
		
		// Value or children
		a('>');
		
		if (hasValue(node))
		{
			Assert.that(node.children.length == 0);
			
			var brackets = false;
			
			switch(node.value) {
				case XM_empty:
				case XM_String(v): a(v);
				default:
					brackets = true;
			}
			
			if (brackets)
			{
				a('{ ');
				addToXMLWithCheckFieldIsSet(node.value, "vo.");
				a(' }');
			}
		}
		
		if (node.children.length > 0)
		{
		 	for (child in node.children) if (isMultiChildXMLMap(child.value)) {
				ac('\n'.code);
				indent(indentLevel+1); a("{ "); addToXMLWithCheckFieldIsSet(child.value, "vo."); a(" }");
			} else {
				gen_toNode(child, indentLevel + 1);
			}
			
			ac('\n'.code); indent(indentLevel);
		}
		a('</'); a(node.nodeName); a('>');
	}
	
	inline function addPathEscaped(path:String)
	{
		var pieces = path.split('.');
		var i = 0;
		for (s in pieces) {
			a(s.quote());
			if (++i != pieces.length) ac('.'.code);
		}
	}
	
	function add_ifStringSet(varName:String) {
		a("val s = "); a(varName); a("; if (s.length != 0) ");
	}
	
	function add_setVarFromString(varName:String, varPrefix:String, setterPath:String, type:PType, converterPrefix:String, format:String)
	{
		add_ifStringSet(varName);
		add_setVar(varPrefix, setterPath, type);
		
//		if (format != null) {
			a(converterPrefix);
			add_conversionFromStringFunction(type);
			ac("(".code); a("s"); if (format != null) { a(', "'); a(format); ac('"'.code); } ac(")".code);
//		}
//		else {
//			a("s");
//		}
//		if (Util.isEnum(type)) {
//			a(".getOrElse(null)");
//		}
	}
	
	function add_setVar(varPrefix:String, setterPath:String, type:PType)
	{
		a(varPrefix);
		switch (type) {
			case TenumConverter(_):
				addPathEscaped(setterPath.substr(0, setterPath.lastIndexOf('.'))); // strip enum converter property
			default:
				addPathEscaped(setterPath);
		}
		a(" = ");
	}
	
	function add_conversionFromStringFunction(type) switch (type)
	{
		case Tstring:
		default:
			a(MutableScala.convertFromAnyRefTo(type));
	}
	
	function matchAttribute(v:XMLMapValue) return switch (v) {
		case XM_empty, XM_String(_), XM_typeMap(_):
		 	false;
		default: true;
	}
	
	function add_fromXMLValue(varName:String, v:XMLMapValue, varPrefix:String, converterPrefix:String = "")
	{
		// Handle expressions
		switch(v)
		{
			case XM_empty, XM_String(_), XM_typeMap(_):
				// Not a value type
				
			case XM_binding		(path, prop), XM_toInt(path, prop):
				add_setVarFromString(varName, varPrefix, path, prop.type, converterPrefix, null);
			
			case XM_not			(v):
				add_fromXMLValue(varName, v, varPrefix, converterPrefix + "!");
			
			case XM_format		(path, prop, format):
				add_setVarFromString(varName, varPrefix, path, prop.type, converterPrefix, format);
			
			case XM_join		(path, prop, seperator):
				add_ifStringSet(varName);
				add_setVar(varPrefix, path, prop.type);
				a(converterPrefix);
				add_conversionFromStringFunction(prop.type);
				a('('); a(varName); a('.split("'); a(seperator); a('"))');
				
			case XM_concat		(values):
				throw "not implemented";

			case XM_children	(path, prop),
			 	 XM_child		(path, prop):
				
				
				var clname = Util.unpackPTypedef(Util.getPTypedef(prop.type)).mutableFullName;
				
				if (Util.isPTypeIterable(prop.type)) {
					a("val tmp_"); a(path); a(" = ");
					a("xml.child.iterator.filter(_.isInstanceOf[scala.xml.Elem]) map { node => ");
					a("outer.setValueObject(new "); a(clname); a("VO, node) ");
					a("} filterNot(_.empty_?) toArray;");
					a("  if (tmp_"); a(path); a(".length > 0) ");
					add_setVar(varPrefix, path, prop.type);
					a("tmp_"); a(path);
				}
				else {
					a("outer.setValueObject("); a(varPrefix); a(path); a(", xml)");
				}
		}
	}
	
	function isMultiChildXMLMap(value:XMLMapValue)
	{
		return value != null && switch (value)
		{
			case XM_children(_,_): true;
			default: false;
		}
	}
	
	function isChildXMLMap(value:XMLMapValue)
	{
		return value != null && switch (value)
		{
			case XM_child(_,_): true;
			default: false;
		}
	}
	
	function addCheckFieldIsSet(v:XMLMapValue, pathPrefix) switch (v)
	{
		// Handle fieldIsSet_?
		case XM_binding(path,prop),
		 	 XM_toInt(path,prop),
		 	 XM_format(path,prop,_),
			 XM_child(path,prop),
		 	 XM_children(path,prop),
		 	 XM_join(path,prop,_):
			
			if (Util.isEnum(prop.type))
				return false;
			
			var objPathEnd = path.lastIndexOf('.');
			
			var fieldIndex = prop.bitIndex();
			
			a("if ("); a(pathPrefix);
			if (objPathEnd != -1) {
				a(path.substr(0, objPathEnd).quote()); ac('.'.code);
			}
			a("fieldIsSet_?("); a(fieldIndex+" /*"); a(prop.parent.mutableFullName); a(" */)) ");
			
			return true;
		
		case XM_not(v):
			return addCheckFieldIsSet(v, pathPrefix);
		
		default:
			return false;
	}
	
	function addToXMLWithCheckFieldIsSet(v:XMLMapValue, pathPrefix)
	{
		if (addCheckFieldIsSet(v, pathPrefix)) {
			addToXMLValue(v, pathPrefix);
			a(' else NodeSeq.Empty'); // Check if else null will not give problems with the PHP cms
		}
		else
			addToXMLValue(v, pathPrefix);
	}
	
	function addToXMLValue(v:XMLMapValue, pathPrefix) switch (v)
	{
		case XM_typeMap(_), XM_String(_): throw "Should have been handled already...";
		
		case XM_binding		(path,prop):
			addConvertedToString(pathPrefix + path, prop.type);
		
		case XM_toInt		(p,v):
			a("new scala.xml.Unparsed(("); addIntString(pathPrefix + p, v.type); a(").toString)");
		
		case XM_not			(v): 
			addToXMLValue(v, "!"+pathPrefix);
		
		case XM_format		(path,prop,format):
			a("xml.Text(ConvertTo.string(");
			addPathEscaped(pathPrefix + path);
			a(",\"");
			a(format);
			a("\"))");
		
		case XM_join		(path,val,seperator):
			var a = a;
			switch (val.type) {
				default:
					throw "Only array joins are supported. v="+v+", pathPrefix="+pathPrefix;
				
				case Tarray(elemT, min, max):
					switch(elemT) {
						case Tstring:
						default:
							throw "Only Array<String> joins are supported.";
					}
					a(pathPrefix); addPathEscaped(path); a('.mkString("'); a(seperator); a('")');
			}
		
		case XM_concat		(values):
			for (i in 0 ... values.length) {
				addToXMLValue(values[i], pathPrefix);
				if (i <= values.length - 2) a(" + ");
			}
		
			trace(code.toString());
			//throw "not implemented";
		
		case XM_empty:								throw "Empty attribute? ";
		
		case XM_children	(path,from), XM_child(path,from):
			add_toXMLChildren(pathPrefix + path, from.type);
	}
	
	function add_toXMLChildren(path, type) switch(type)
	{
		case Tdef(t):
			var clname = Util.unpackPTypedef(t).mutableFullName;
			a("toXML("); addPathEscaped(path); a(")");
		
		default:
			switch(type)
			{
				case Tarray(t,_,_):
					addPathEscaped(path);
				 	a(".iterator.map({ i => "); add_toXMLChildren("i", t); a(" })"); // _.toXML())");
				
				default: throw "impossible";
			}
	}
	
	function addConvertedToString(path, type) switch (type)
	{
/*		case Tdef(tdef): switch (tdef) {
			case Tclass(cl):	throw "class to string?";
			case Tenum(cl):		a("(if ("); addPathEscaped(path); a(" != null) xml.Text(ConvertTo.string("); addPathEscaped(path); a(")) else NodeSeq.Empty)");
		}
*/		
		case TenumConverter(_): a("(if (!"); addPathEscaped(path); a(".isEmpty) xml.Text("); addPathEscaped(path); a(") else NodeSeq.Empty)");
		
		case Tstring:
			a("xml.Text(");
			addPathEscaped(path);
			a(")");
			
		case Tbool(_), Tinteger(_,_,_):
			a("new scala.xml.Unparsed(("); addPathEscaped(path); a(").toString)");
			
		default:
			a("xml.Text(ConvertTo.string("); addPathEscaped(path); a("))");
	}
	
	inline function indent(times:Int) for (i in 0 ... times) a("  ")
	
	function addIntString(path, type) switch(type)
	{
		case Tbool(val):				a("if ("); addPathEscaped(path); a(") 1 else 0");
		case Tinteger(min,max,stride):	addPathEscaped(path);
		case Tdecimal(min,max,stride):	addPathEscaped(path); a(".toInt");
		case Tstring:					addPathEscaped(path); throw "Int String? "+path+" : "+type;
		case Tcolor:					addPathEscaped(path); a(".toInt");
		case Tdate:						addPathEscaped(path); a(".getMillis()");
		case Tdatetime:					addPathEscaped(path); a(".getMillis()");
		
		default: throw "not implemented for type: "+type;
	}
	
	inline function hasValue(node) return node.value != null && node.value != XM_empty
	
	// Adds space then String :-)
	inline function ass(s:String) {
		ac(" ".code);
		a(s);
	}
	
	inline function a(str) code.add(str)
	inline function ac(ch) code.addChar(ch)
}

class ScalaMessagePacking extends MessagePacking
{
	override private function expr_incrementMixinCount()	return "mixin += 1"
	override private function a_return() a("return")
	override private function a_not0(v:String) {
		a(v); a(" != 0");
	}
	
	override private function a_is0(v:String) {
		a("("); a(v); a(") == 0");
	}
	
	override private function a_packVOHeaderCallStart() {
		a("\n\t\to.packValueObjectHeader("); a(Std.string(def.index)); a(", ");
	}
	
	override private function a_msgpackVOCallStart(t : TypeDefinition) {
		a("\n\t\t\t"); addMutableFullName(t); a(".msgpack_packVO(o, obj, ");
	}
	
	override private function a_assert(v:String) {
		a("require("); a(v); a(");");
	}

	override private function a_maskByte(mask:Int, byte:String) {
		return if (mask > 0xFF){ a(byte); a(" & 0xFF"); } else { a(byte); a(" & 0x"); a(StringTools.hex(mask, 2)); };
	}
	override private function a_writeByte(byte:String, mask:Int = 0xFFFFFF) {
		a("o.writeByte("); a(byte); a(");");
	}
	override private function endPackerFunction() {
		a("\n\t}\n");
	}
	override private function addFieldIndexOffsetCase(t : TypeDefinition, offset : Int) {
		a("\n    case "); a(Std.string(t.index)); a(" => "); a(offset + ";"); a(" // "); a(t.mutableFullName);
	}
	
	override private function addPropertyPackerCall(path:String, pType:PType, bindable:Bool)
	{
		if (path.indexOf("obj.") == 0) {
			path = "obj." + path.substr(4).quote();
		}
		
		switch (pType)
		{
			case Tdef(tdef): switch(tdef) {
				case Tclass(_):	a('o.pack('); a(path); ac(")".code);
				case Tenum(_):	a('o.pack('); a(path); a(".value)");
			} 
			
			case Tarray(_,_,_), Tbool(_), Tinteger(_,_,_), Tdecimal(_,_,_), Tstring, Tdate, Tdatetime, Tinterval, Turi, Turl, TuniqueID, Temail, Tcolor, TfileRef:
				a('o.pack('); a(path); ac(")".code);
			
			
			case TenumConverter(_), TclassRef(_):
				throw "Not implemented";
		}
	}
	
	override private function definePackerFunction()
	{
		a("\n\tfinal def msgpack_packVO(o : MutableVOPacker, obj : "); a(def.name); a(", flagsToPack : Int)\n\t{"); //"); a(Module.pkgRoots.first().name); a("]
		a("\n		require(o != null && obj != null);");
		a("\n		");
		a("\n		var propertyBits = flagsToPack;");
	}
	
	override private function genDeSerialization(lastProp)
	{
		fieldIndexOffset = new IntHash();
		
		a("\n  def defaultVOCompanionMap = ");
		var pkgroot = def.module.getPackageRoot();
		if (pkgroot != Module.root) {
			a(pkgroot.mutableFullName); a(".VO.typeMap");
		}
		else a("null");
		
		a("\n  val TypeID = "); a(Std.string(def.index));
		a("\n  final def fieldIndexOffset(typeID: Int) = typeID match {");
		genFieldOffsetCases(def);
		a("\n  }\n");
	}
	
	override private function a_unpackProperty(p:Property)
	{
		a(p.name.quote()); a(" = "); a("input.unpack();");
	}
}
