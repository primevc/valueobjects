package primevc.tools.valueobjects.output;
 import primevc.tools.valueobjects.VODefinition;
  using primevc.tools.valueobjects.output.Util;
  using primevc.utils.TypeUtil;

class HaxeTypeMap implements CodeGenerator
{
	var map : IntHash<String>;
	
	public function new() {
		map = new IntHash();
		map.set(0x1D, "primevc.core.traits.ObjectId");
	}
	
	public function genClass(def : ClassDef) {
		map.set(def.index, if (def.isMixin) def.fullName; else def.fullName + "VO");
	}
	
	public function write(module : Module)
	{
		var dir = module.mkdir();
		var filename = dir + "/VO.hx";
		
		var file = neko.io.File.write(filename, false);

		file.writeString("package " +module.fullName+";\n");

		file.writeString("
class VO
{
	static public var typeMap : IntHash<Class<Dynamic>>;
	static function __init__ ()
	{
		var map : IntHash<Class<Dynamic>> = typeMap = new IntHash();
");

		for (typeID in map.keys()) {
			file.writeString("\n\t\tmap.set(");
			file.writeString(typeID + ", ");
			file.writeString(map.get(typeID));
			file.writeString(");");
		}

file.writeString("
	}
}
");

		file.close();
	}
	
	public function genEnum(def:EnumDef) {}
	
	public function newModule(module:Module) {
		module.generateWith(this);
		return cast this;
	}
}

class HaxeMessagePacking extends MessagePacking
{
	override private function addPropertyPackerCall(path:String, pType:PType, bindable:Bool)
	{
		switch (pType) {
			case Tarray(_,_,_):
			default:
				if (bindable) path += ".value";
		}
		
		switch (pType)
		{
			case Tbool(val):				a('b += o.packBool(');			a(path); ac(")".code);
			case Tinteger(min,max,stride):	a('b += o.packInt(');			a(path); ac(")".code);
			case Tdecimal(min,max,stride):	a('b += o.packDouble(');		a(path); ac(")".code);
			case Tstring:					a('b += o.packString(');		a(path); ac(")".code);
			case Tdate:						a('b += o.packDate(');			a(path); ac(")".code);
			case Tdatetime:					a('b += o.packDateTime(');		a(path); ac(")".code);
			case Tinterval:					a('b += o.packDateInterval(');	a(path); ac(")".code);
			case Turi, Turl:				a('b += o.packURI(');			a(path); ac(")".code);
			case TuniqueID:					a('b += o.packObjectId(');		a(path); ac(")".code);
			case Temail:					a('b += o.packEMail(');			a(path); ac(")".code);
			case Tcolor:					a('b += o.packRGBA(');			a(path); ac(")".code);
			case TfileRef:					a('b += o.packFileRef(');		a(path); ac(")".code);
		
			case Tdef(ptypedef): switch (ptypedef) {
				case Tclass(def):	a('b += ('); a(path); a(".notNull()? "); a(path); a(".messagePack(o) : o.packNil())");
				case Tenum(def):
				if (def.catchAll != null) {
					a('{\n\t\t\t\tvar v = '); a(def.fullName); a("_utils.toValue("); a(path); a(");\n\t\t\t\tb += v.isSet()? o.packInt(v) : o.packString("); a(def.fullName); a("_utils.toString("); a(path); a("));\n\t\t\t}");
				} else {
					a('b += o.packInt('); a(def.fullName); a("_utils.toValue("); a(path); a("))");
				}
			}
			case Tarray(type, min, max):
				a("{");
				a("\n\t\t\t\tvar a = "); a(path); ac(";".code);
				a("\n\t\t\t\tb += o.packArrayHeader(a.length);");
				a("\n\t\t\t\tfor (i in 0 ... a.length) "); addPropertyPackerCall("a.getItemAt(i)", type, false); ac(";".code);
				a("\n\t\t\t}");
				
			case TenumConverter(_):	throw "Not implemented";	
			case TclassRef(_):		throw "Not implemented";
			
		}
	}
	
	override private function definePackerFunction()
	{
		fieldIndexOffset = new IntHash();
		if (!def.isMixin) {
			a("\n\toverride private function _fieldOffset(typeID:Int) return switch(typeID) {");
			genFieldOffsetCases(def);
			a("\n\t}");
		}
		a("\n");
		
		a("\n\t@:keep static public function msgpack_packVO(o : haxe.io.BytesOutput, obj : "); if (def.isMixin){ a("I"); a(def.name); } else { a("I"); a(def.name); a("VO"); } a(", propertyBits : Int, prependMsgpackType : Bool = false) : Int\n\t{");
		
		a("\n\t\tAssert.that(o != null && obj != null);");
		a("\n\t\t");
		
		a("\n\t\tvar b /* bytes written */ : Int;");
		a("\n\t\tif (prependMsgpackType) {");
		
		a("\n\t\t\tif ("); a_not0("propertyBits"); a(") b = o.packValueObject();");
		a("\n\t\t\telse return o.packNil();");
		a("\n\t\t}");
		
		a("\n\t\telse b = 0;");
		a("\n\t\t");
	}
	
	
	override private function defineUnPackerFunction()
	{
		var name = def.isMixin ? def.name : def.name + "VO";
		a("\n\t@:keep static public function msgpack_unpackVO(reader : Reader, obj : "); a("I"); a(name); a(", propertyBytes : Int) : Void\n\t{");
		a("\n\t\tAssert.that(reader != null && obj != null);");
		a("\n\t\tvar bits:Int, fieldOffset:Int = (untyped obj)._fieldOffset(TYPE_ID);");
	}
	

	override private function a_unpackProperty(p:Property, bit:Int)
	{
		Assert.that(!p.isTransient());
		/*
		 1. check if property has getter -> indicating wether we should create instantiate a value even when there's nothing in the messagepack
		 2. if property has getter, only fill the property when it has a value
		 3. if property doesn't have a getter and isn't transient or a simple-value, always create a value for it
		 */
		
		var propertyCheck 	= "(bits & 0x" + StringTools.hex(1 << bit, 2) + ").not0()";
		var hasWrapper 		= p.isArray() || p.isBindable();	// indicating wether there's an object or list wrapped around a value/values
	//	a("\ttrace('"); a(p.name); a("');\n");
	//	a("\ttry {\n");
		a("\t\t");
	//	if (!hasWrapper || p.shouldHaveGetter()) {	// 1
			a("if ("); a(propertyCheck); a(")\t\t");
	//	}
		
	/*	if (!p.isArray() && p.isBindable()) {
			a("(cast(obj."); a(p.name); a(", primevc.core.Bindable<"); a(HaxeUtil.haxeType(p.type, true)); a(">).value = ");
		}
		else
		{*/
		a((def.isMixin || p.shouldHaveSetter()) ? "(untyped obj)." : "obj."); a(p.name); a(" = ");
	//	}
		
		if (hasWrapper) {
			a('new '); a(HaxeUtil.haxeType(p.type, true, p.isBindable(), false, p.isTransient())); a('( '); //a(propertyCheck); a(" ? ");
		}
/*		else {
			a("((untyped obj)."); a(p.name);
			if (!p.isBindable()) a('(');
			
//			a("(untyped obj).set"); code.addCapitalized(p.name); a("(");
			if (p.isBindable() && Util.isSingleValue(p.type)) {
				a(".value");
			}
//			if (p.isBindable()) {
//				a("new primevc.core.RevertableBindable<"); a(HaxeUtil.haxeType(p.type, true)); a(">(");
//			}
			a(" = ");
		}
*/		
		a("reader.");
		switch (p.type) {
			case Tarray(innerT,_,_):	a("readMsgPackArray("); a(p.name.toUpperCase()); a(", "); a(HaxeUtil.haxeType(innerT));
			case Tcolor:				a("readMsgPackValue("); a(p.name.toUpperCase()); a(", "); a(HaxeUtil.haxeType(p.type)); a("Type");
			default:					a("readMsgPackValue("); a(p.name.toUpperCase()); a(", "); a(HaxeUtil.haxeType(p.type));
		}

		if (hasWrapper)
			a(")");
		a(");\n");

	//	a("\t} catch (e:Dynamic) { trace(e); throw 'property: "); a(p.name); a(" in ' + obj + '; error: '+e; }\n");
	}
}

class Haxe implements CodeGenerator
{
	static var haxeModules : List<Haxe>;
	
	static public function reinitialize() { 
		haxeModules = new List();
	}
	static var initialize = reinitialize();
	
	public static function generate()
	{
		Module.root.generateWith(new Haxe(Module.root));
		
		for (m in Module.pkgRoots) {
			neko.Lib.print("\t - " + m.fullName + "\n");
			var map = new HaxeTypeMap();
			m.generateWith(map);
			map.write(m);
		}
	}
	
	var mod		: Module;
	var code	: StringBuf;
	var dir		: String;
	
	public function new(module:Module)
	{
		this.mod = module;
		this.dir = module.mkdir();
		haxeModules.add(this);
	}
	
	public function newModule(m:Module) : CodeGenerator
	{
		var h = new Haxe(m);
		return h;
	}
	
	function addPackage(def:TypeDefinition)
	{
		var a = this.code.add;
		if (def.module.name.length > 0) {
			a("package "); a(def.module.fullName); a(";\n");
			a(" import primevc.core.traits.IMessagePackable;\n");
			a(" import primevc.core.traits.IEditEnabledValueObject;\n");
		}
	}
	
	private function genClassStart(def:ClassDef, immutable:Bool)
	{
		this.code = new StringBuf();
		var a = this.code.add;
		addPackage(def);
		
		if (immutable)
			a("\n@:keep interface I");
		else {
			a(" import primevc.tools.valueobjects.ObjectChangeSet;\n");
			a(" import primevc.tools.valueobjects.ValueObjectBase;\n");
			a(" import primevc.tools.valueobjects.xmlmap.XMLString;\n");
			a(" import primevc.utils.msgpack.Reader;\n");
			a(" import primevc.utils.IfUtil;\n");
			a("  using primevc.utils.IfUtil;\n");
			a("  using primevc.utils.TypeUtil;\n");
			a("  using primevc.utils.NumberUtil;\n");
			a("  using primevc.utils.msgpack.Format;\n");
			a("\n\n");
			a("/**\n * @creation-date\t"); a(Date.now().toString()); a("\n */\n");
			a("class ");
		}
		
		a(def.name);
	}
	
	private function genClassProperties(def:ClassDef, immutable:Bool) //, editable:Bool)
	{
		genClassStart(def, immutable); if (!def.isMixin) a("VO");
		
		if (!immutable) {
			if (def.superClass != null) {
				a(" extends "); a(def.superClass.fullName); a("VO,");
			}
			else
				a(" extends ValueObjectBase,");
			
			a(" implements "); a(def.module.fullName); a(".I"); a(def.name); a("VO,");
			a(" implements primevc.core.traits.IEditableValueObject");
			if (!def.isMixin && def.superClass == null) {
				a(", implements primevc.core.traits.IClonable < " + def.name + "VO >");
				a(", implements primevc.core.traits.IDuplicatable < " + def.name + "VO >");
			}
		}
		else
		{	
			for (t in def.supertypes) {
				a(" implements "); addFullName(t, true); a(",");
			}
			
			a(" implements IEditEnabledValueObject, implements IMessagePackable");
		}
		
		a("\n{\n");
		
		for (i in 0 ... def.propertiesSorted.length)
		{
			var p = def.propertiesSorted[i];
			if (Util.isDefinedInSuperClassOf(def, p)) continue;

			openPlatformCode(p);
			if (immutable) {
				genGetter(p, true);
			} else {
				genGetter(p, false);
				if (p.shouldHaveSetter())
					genSetter(p);
				
				if (!p.isTransient() && (p.isBindable() || p.isArray()))
					genChangeWatcher(p);
				
				a("\n");
			}
			closePlatformCode(p);
		}
		
		if (immutable) {
			// Write immutable interface to disk
			a("}\n");
			write("I" + def.name + (def.isMixin? "" : "VO"));
		}
	}
	
	public function genClass(def:ClassDef) : Void
	{
#if debug
		trace(def.fullName);
#end
		
		// Interfaces
		genClassProperties(def, true);
		
		// VO Class
		if (!def.isMixin)
		{
			genClassProperties(def, false);
			
			var magic = getMagicGenerator(def);
			magic.inject(code);
			
			genClassConstructor(def, def.superClass != null, magic);
			genDispose(def);
			genValidation(def.superClass != null);
		//	genXMLMapFunctions(def, magic);
			genEditFunctions(def);
		}
		else {
			// mixin
			genClassStart(def, false);
			a("\n{");
		}
		
		genClassMetaData(def);
		genSerialization(def);
//		genFieldSetter(def);
		
		if (!def.isMixin) {
			genCloneFunction(def);
			genDuplicateFunction(def);
			genInjectValuesFunction(def);
		}
		
		for (p in def.property) if (p.hasOption(unique)) {
			genToStringFunction(def, p);
			break;
		}
	//	if (def.implementsType( UniqueIDTrait.type ))
	//		genToStringFunction(def);
		
		// Close class }
		code.add("}\n");
		
		write(def.name + (def.isMixin? "" : "VO"));
	}
	
	
	
	static var dummyMagic = new MagicClassGenerator();
	
	private function getMagicGenerator(def:ClassDef) : MagicClassGenerator
	{
		if (Std.is(def, NamedSetDef)) return new NamedSetDefGenerator(def);
		
		return dummyMagic;
	}
	
	inline function a(str:String) code.add(str)
	inline function addLineComment (str:String)
	{
		openComment();
		addCommentLine(str);
		closeComment();
	}
	inline function openComment ()					  a("\t/**\n")
	inline function closeComment ()					  a("\t */\n")
	inline function addCommentLine (str:String)		{ a("\t * "); a(str); a("\n"); }
	inline function addComment (str:String)
	{
		openComment();
		var lines = str.split("\n");
		for (line in lines)
			addCommentLine( StringTools.trim(line) );
		closeComment();
	}

	
	
	private function genClassMetaData(def:ClassDef)
	{
		a("\n\tstatic inline public var TYPE_ID = ");
		a(def.index + ";\n\t");
		for (p in def.propertiesSorted) if (!p.hasOption(transient)) {
		 	a("\n\tstatic inline public var "); a(p.name.toUpperCase()); a(" = "); a("0x" + StringTools.hex(p.definedIn.index << 8 | p.index, 4)); a("; // "); a(p.definedIn.name);
		}
		a("\n\t\n\t");

		//generate static propertyIdToString method
		a("\n\t@:keep static public function propertyIdToString (id:Int) : String");
		a("\n\t{");
		a("\n\t\treturn switch(id) {");
		for (p in def.propertiesSorted) if (!p.hasOption(transient)) {
			a("\n\t\t\tcase "); a(p.name.toUpperCase()); a(": '"); a(p.name); a("';");
		}
		a("\n\t\t}");
		a("\n\t}");
		a("\n\n");

		if (!def.isMixin)
		{
			//generate getPropertyById method
			addLineComment("Method is used for undo/redo to get a reference to the changed property");
			a("\n\toverride public function getPropertyById (id:Int) : Dynamic");
			a("\n\t{");
			a("\n\t\treturn untyped switch(id) {");
			for (p in def.propertiesSorted) if (!p.hasOption(transient)) {
				a("\n\t\t\tcase "); a(p.name.toUpperCase()); a(": "); a("this."); a(p.name); a(";");
			}
			a("\n\t\t}");
			a("\n\t}");
			a("\n\n");

			//generate setPropertyById method
			addLineComment("Method is used for undo/redo to update the reference of the changed property");
			a("\n\toverride public function setPropertyById (id:Int, v:Dynamic) : Void");
			a("\n\t{");
			a("\n\t\tswitch(id) {");
			for (p in def.propertiesSorted) if (!p.hasOption(transient)) {
				a("\n\t\t\tcase "); a(p.name.toUpperCase()); a(": "); 
				if (p.isBindable() && !p.isArray()) {
					a("this."); a(p.name); a(".value = v;");
				} else {
					a("this."); a(p.name); a(" = v;");
				}
			}
			a("\n\t\t}");
			a("\n\t}");
			a("\n\t\n\t");
		}
	}
	
	private function addFullName(t:TypeDefinition, interfaceT = false)
	{
		Util.addFullName(code, t, interfaceT);
	}
	
	private function genSerialization(def:ClassDef)
	{	
		if (!def.isMixin)
		{
			a("\n\t"); if (def.superClass != null) a("override "); a("public function messagePack(o : haxe.io.BytesOutput) : Int\n\t{");
			a("\n\t\treturn msgpack_packVO(o, this, _propertiesSet, true);");
			a("\n\t}");
			a("\n");
		}
		
		new HaxeMessagePacking(code, def).genSerialization();
	}
	
	static function bitmask(numBits:Int, offset:Int=0)
	{
		var mask = 0;
		for (bit in 0 ... numBits) {
			mask |= 1 << (bit + offset);
		}
		return "0x" + StringTools.hex(mask, 4);
	}
	
	private inline function ac(char:Int) code.addChar(char)
	
	/** Returns true when no if statement was added  (Property is always set) */
	private function addIfPropertyIsSetExpr(propPrefix:String, p:Property, expr:String)
	{
		return addIfVarIsSetExpr(propPrefix + p.name, p.type, p.isBindable(), expr);
	}
	
	private function addIfVarIsSetExpr(path:String, ptype:PType, bindable:Bool, expr:String)
	{	
		var nullCheck = path + ".notNull()";
		
		switch (ptype) {
			case Tarray(_,_,_), Turi, Turl:
			
			case Tinteger(_,_,_), Tdecimal(_,_,_), Tbool(_):
				if (!bindable) nullCheck = null; // Simple types can't be null...
			
			default:
				if (bindable) {
					path = path + ".value";
					nullCheck += " && " + path + ".notNull()";
				}
		}
		
		var extraChecks = extraNullCheck(path, ptype);
		
		if (nullCheck != null || extraChecks != null)
		{
			a("if (");
			if (nullCheck != null) a(nullCheck);
			if (extraChecks != null) { if (nullCheck != null) a(" && "); a(extraChecks); }
			a(") ");
		}
		
	 	a(expr);
		a(";"); // a(p.name);
		
		return nullCheck == null && extraChecks == null;
	}
	
	private function extraNullCheck(path:String, ptype:PType) return switch(ptype)
	{
		case Tstring, Temail:
			path + ".length.not0()";

		case Tarray(_,_,_):
			path + ".length.not0()";

		case Turi, Turl, TfileRef:
			path + ".isSet";
		
		case Tinteger(_,_,_), Tdecimal(_,_,_):
			path + ".isSet()";
		
		case Tdef(_):
			if (Util.isEnum(ptype)) null
			else "!" + path + ".isEmpty()";
		
		case Tbool(_), TuniqueID, Tinterval, TenumConverter(_), Tdate, Tdatetime, Tcolor, TclassRef(_):
			null;
	}
	
	private function addIfValueNotNullExpr(valuePath:String, ptype:PType, expr:String)
	{
		var extraChecks = extraNullCheck(valuePath, ptype);
		
		if (extraChecks != null) {
			a("if ("); a(extraChecks); a(") "); a(expr); a(";");
		}
	}


	private function getAllEmptyChecks (p:Property, path:String, bindable:Bool) : String
	{
		if (p.hasOption(unique))
			path = "this."+p.name;
		
		var isNull = HaxeUtil.isNullableOnEveryPlatform(p.type, bindable) ? path+".notNull()" : null;
		var extra = extraNullCheck(path, p.type);

		return 	 if (extra != null && isNull != null)	isNull + " && " + extra;
			else if (extra != null) 					extra;
			else if (isNull != null) 					isNull;
			else null;
	}
	
	private function addEmbedMessagePackTypeBitmask(i:Int) {
		a(StringTools.hex(1 << (i + 8), 4));
	}
	
	private function addEmbedMessagePackCall(i:Int, t:ClassDef, numEmbedTypes:Int, bitsOffset:Int)
	{
		a("\n\t\t\t");
		if (numEmbedTypes > 1) {
			a("if ((i & 0x"); addEmbedMessagePackTypeBitmask(i); a(").not0()) ");
		}
		a("b += "); a(t.fullName); if (!t.isMixin) a("VO"); a(".msgpack_packVO(o, this, "); a(bitsOffset + ");");
	}
	
	private function genValidation(genOverride:Bool = false)
	{
		return;
		
	/*	a("\n#if (VO_Write || !VO_Read)");
		a("\n\t"); if (genOverride) a("override "); a("public function isValid():Bool\n\t{\n ");
		a("\t\treturn true;\n");
		a("\t}\n#end\n");*/
	}
	
	private function hexBitflag(propertyIndex:Int) {
		return "0x" + StringTools.hex(1 << propertyIndex, 2);
	}
	
	private function genEditFunctions(def:ClassDef)
	{
		genEditableVOFunctionCalls(def, "beginEdit", false);
		genEditableVOFunctionCalls(def, "commitBindables", false);
		genEditableVOFunctionCalls(def, "cancelEdit");
		
		// addChanges()
		a("\n\toverride private function addChanges(changeSet:ObjectChangeSet)\n\t{");
		a("\n\t\tif (_changedFlags.not0())\n\t\t{");
		if (def.superClass != null)
			a("\n\t\t\tsuper.addChanges(changeSet);");
		
		var props = def.propertiesSorted;
		for (p in props)	if (!p.isTransient() && !Util.isDefinedInSuperClassOf(def, p))
		{	
			a("\n\t\t\t");
			if (p.isArray() && p.isBindable())	a("changeSet.addListChanges(");
			else if (p.isBindable())			a("changeSet.addBindableChange(");
			else								a("changeSet.addChange(");
			
			a(p.name.toUpperCase()); a(", _changedFlags & "); a(hexBitflag(p.bitIndex())); a(", "); 
			
			if (!p.isArray() && p.isBindable()) {
				a(p.name); a(".shadowValue, "); a(p.name); a(".value");
			}
			else if (p.shouldHaveGetter()) {
				a("(untyped this)."); a(p.name);
			} else
				a(p.name);
			
			a(");");
		}
		a("\n\t\t}\n\t}\n");
	}
	
	private function addAsClass(ptype:PType) switch (ptype) {
		case Tdef(innerT): switch (innerT) {
			case Tclass(cl):	a(".as("); addFullName(cl, false); a(")");
			case Tenum(_):
		}
		default:
	}
	
	
	private function genCloneFunction(def:ClassDef)
	{
		var a = code.add;
		
		var returnType	= def.superClass == null ? def.name : def.getRootSuperClass().fullName;
		returnType		= /*"I" + */returnType + "VO";
		
		openFunctionDeclaration( def, "clone", false, returnType, false);
		a("\t\tvar inst = new "+def.name + "VO(");
		
		var first = true;
		for (p in def.propertiesSorted)
		{
			a("\n\t\t\t");
			if (p.isPlatformSpecific()) {
				openPlatformCode(p, false); a(" ");
			}
			
			if (first) {
				first = false;
			}
			else a(", ");
			
			if (p.isArray()) {
				a("null"); // handle array cloning seperately
			} else {
				if (HaxeUtil.isNullableOnEveryPlatform(p.type, p.isBindable())) { // && !p.hasOption(transient)) {
					a("this."); a(p.name); a(".isNull()? null : ");
				}
				
				a("this."); a(p.name);
				if (p.isBindable())			a(".value");
				addAsClass(p.type);
				
				if (p.hasClonableType() && !p.hasOption(transient))
				{
					a(".clone()");
					addAsClass(p.type);
				}
			}
			
			if (p.isPlatformSpecific()) {
				a(" ");
				closePlatformCode(p);
			}
		}
		a("\n\t\t);\n");
		
		var first = true;
		for (p in def.propertiesSorted) if (p.isArray())
		{
			a("\n\t\tinst."); a(p.name); a(" = cast this."); a(p.name); a(".clone();");
			first = false;
		}
		if (!first)
			a("\n\t\tinst._changedFlags = 0;\n");
		
		if (def.superClass == null)
			a("\t\treturn inst;\n");
		else
			a("\t\treturn cast inst;\n");
		
		closeFunctionDeclaration( def, "clone");
	}
	
	
	private function genDuplicateFunction(def:ClassDef)
	{
		var a = code.add;
		
		var returnType	= def.superClass == null ? def.name : def.getRootSuperClass().fullName;
		returnType		= /*"I" + */returnType + "VO";
		
		openFunctionDeclaration( def, "duplicate", false, returnType, false);
		a("\t\tvar inst = new "+def.name + "VO(");
		
		var first = true;
		for (p in def.propertiesSorted)
		{
			a("\n\t\t\t");
			if (p.isPlatformSpecific()) {
				openPlatformCode(p, false); a(" ");
			}

			if (first) {
				first = false;
			}
			else a(", ");
			
			if (p.isArray()) {
				a("null"); // handle array cloning seperately
			} else if (p.hasOption(unique)) {
			//	a("null"); // never duplicate id's
				a(HaxeUtil.getConstructorInitializer(p.type, true));
			} else {
				if (HaxeUtil.isNullableOnEveryPlatform(p.type, p.isBindable())) { // && !p.hasOption(transient)) {
					a("this."); a(p.name); a(".isNull()? null : ");
				}
				
				a("this."); a(p.name);
				if (p.isBindable())			a(".value");
				addAsClass(p.type);
				
				if (p.hasClonableType() && !p.hasOption(transient))
				{
					a(".duplicate()");
					addAsClass(p.type);
				}
			}

			if (p.isPlatformSpecific()) {
				a(" ");
				closePlatformCode(p);
			}
		}
		a("\t\t);\n");
		
		var first = true;
		for (p in def.propertiesSorted) if (p.isArray())
		{
			a("\n\t\tinst."); a(p.name); a(" = cast this."); a(p.name); a(".duplicate();");
			first = false;
		}
		if (!first)
			a("\n\t\tinst._changedFlags = 0;\n");
		
		if (def.superClass == null)
			a("\t\treturn inst;\n");
		else
			a("\t\treturn cast inst;\n");
		
		closeFunctionDeclaration( def, "clone");
	}
	
	
	private function genInjectValuesFunction(def:ClassDef)
	{
		var a = code.add;

		var paramType		= /*"I" + */ (def.superClass == null ? def.name : def.getRootSuperClass().fullName) + "VO";
		var realType		= /*"I" + */ def.name + "VO";
		var functionName	= "injectValues";
		
		//open function declaration
		a("\n\t");
		a("@:keep ");
		if (def.superClass != null)
			a("override ");
		
		a("public function "); a(functionName); a(" (vo:"); a(paramType); a(") : Void");
		a("\n\t{\n");
		
		//call super method
		if (def.superClass != null) {
			a("\t\tsuper."); a(functionName); a("(vo);\n");
		}
		
		if (paramType != realType) {
			a("\t\tvar vo = vo.as("); a(realType); a(");\n");
		} else {
			a("\t\t"); a("_propertiesSet = vo._propertiesSet;\n");
		}
		
		//inject properties
		for (p in def.propertiesSorted) if (!p.isTransient() && !Util.isDefinedInSuperClassOf(def, p))
		{
			a("\t\t");
			if (p.shouldHaveGetter() && HaxeUtil.isNullableOnEveryPlatform(p.type, p.isBindable())) {
				a("if ((untyped vo)."); a(p.name); a(".notNull())\t");
			}
			a(p.shouldHaveSetter() ? "(untyped this)." : "this.");
			
			if (p.isArray()) {
				a(p.name); a(".inject( "); a("vo."); a(p.name); a(".list );");
			} else {
				var name = p.isBindable() ? p.name + ".value" : p.name;
				a(name); a(" = "); a("vo."); a(name); a(";");
			}
			a("\n");
		}
		closeFunctionDeclaration( def, "injectValues");
	}
	
	
	
	
	/**
	 * Method will add an toString method that will return an unique-id for
	 * the vo instance. Method only exists in debug mode
	 */
	private function genToStringFunction (def:ClassDef, idProp:Property)
	{
	/*	var a = code.add;
		a("\n\n#if debug");
		openFunctionDeclaration( def, "toString", true, "String", false);
		a("\t\treturn '[ "+def.name+"VO :: '+ " + idProp.name + " + ' ]';\n");
		closeFunctionDeclaration( def, "toString");
		a("#end\n");*/
	}
	
	
	private function openFunctionDeclaration (def:ClassDef, functionName, forceOverride = false, returnType:String = "Void", makeSuperCall = true, isPublic = true, comment:String = null)
	{
		a("\n\n\t");
	//	a("@:keep ");
		if (comment != null)
			addLineComment(comment);
		
		if (forceOverride || def.superClass != null)
			a("override ");

		a(isPublic? "public " : "private "); a("function "); a(functionName); a("()");
		
		if (returnType != null && returnType != "")
			a(" : " + returnType);
		
		a("\n\t{\n");
		
		if ((forceOverride || def.superClass != null) && makeSuperCall)
			callSuperFunction(def, functionName);
	}
	
	
	private function callSuperFunction (def:ClassDef, functionName)
	{
		a("\t\tsuper."); a(functionName); a("();\n");
	}
	
	
	private function closeFunctionDeclaration (def:ClassDef, functionName)
	{
		code.add("\t}\n\n");
	}
	
	
	private function generateFunctionCall (p:Property, functionName:String)
	{
		code.add("\t\tthis.");
		code.add(p.name);
		code.add(".");
		code.add(functionName);
		code.add("();\n");
	}

	
	function write(name:String)
	{
		var filename = dir +"/"+ name + ".hx";
		
		var file = neko.io.File.write(filename, false);
		file.writeString(code.toString());
		file.close();
	}
	
	
	private function genDispose(def:ClassDef)
	{	
		openFunctionDeclaration( def, "dispose", true, "Void", false );
		
		for (p in def.property) if (!Util.isDefinedInSuperClassOf(def, p) && p.isDisposable() && !p.hasOption(mongo_reference))
		{
			var hasSetter  = p.shouldHaveSetter();
			var hasGetter  = p.shouldHaveGetter();
			var hasWrapper = hasGetter || p.isTransient();

			var setName = hasSetter ? "(untyped this)." + p.name : p.name;
			var getName = hasGetter ? "(untyped this)." + p.name : p.name;

			a("\t\t");
			if (hasWrapper)	{ a("if (IfUtil.notNull("); a(getName); a("))\t\t{ "); }
			a(p.name); a(".dispose(); "); a(setName); a(" = null;");
			if (hasWrapper)
				a(" }");
			a("\n");
		}
		
		//do supercall after the local properties are disposed
		callSuperFunction(def, "dispose");
		closeFunctionDeclaration( def, "dispose");
	}
	
	
	private function genEditableVOFunctionCalls(def:ClassDef, functionName, superCallBefore:Bool = true)
	{
		var functionCalls = new List<Property>();
		
		for (p in def.property)
			if (!Util.isDefinedInSuperClassOf(def, p) && p.isBindable() && !p.hasOption(transient))
				functionCalls.add(p);
		
		var callFunctionName = functionName == "commitBindables"? "commitEdit" : functionName;
		var isCommitBindables = functionName == "commitBindables";
		
		if (functionCalls.length > 0)
		{
			openFunctionDeclaration( def, functionName, true, "Void", false, !isCommitBindables );
			for (p in functionCalls)
				generateFunctionCall( p, callFunctionName );
			
		//	if (def.superClass != null && !superCallBefore)
			callSuperFunction(def, functionName);
			
			closeFunctionDeclaration( def, functionName);
		}
	}
	
	private function genClassConstructor(def:ClassDef, genSuperCall:Bool = false, magic)
	{
		a("\n\n\tpublic function new(");
		for (i in 0 ... def.propertiesSorted.length)
		{
			var p = def.propertiesSorted[i];
			if (p.isPlatformSpecific()) {
				a("\n"); openPlatformCode(p, false); a("\t");
			} else {
				a("\n\t/* " + i + "  */\t\t");
			}
			// Add comma first to prevent platform specific property clashes
			if (i != 0) a(", ");

			a("?"); a(p.name); a("_ : "); a(HaxeUtil.haxeType(p.type, null, null, true, p.hasOption(transient)));
			var init = HaxeUtil.getConstructorInitializer(p.type, true);
			if (init != null) {
			 	a(" = "); a(init);
			}

			if (p.isPlatformSpecific())
				closePlatformCode(p, false);
			
		//	a("\t\t\t\t//"+i);
		}
		a("\n\t\t)\n\t{\n");


		//
		// fill class memberes
		//
		for (p in def.propertiesSorted) if (!Util.isDefinedInSuperClassOf(def, p))
		{
			openPlatformCode(p);
			var hasGetter 		= p.shouldHaveGetter();
			var hasSetter 		= p.shouldHaveSetter();
			var localProp 		= p.name + "_";
			var classProp 		= (hasSetter ? "(untyped this)." : "this.") + p.name; 

			var val 			=  hasGetter ? localProp : HaxeUtil.getConstructorCall(p.type, p.isBindable(), localProp, p.isTransient());
			var addNullCheck	= (hasGetter && HaxeUtil.isNullableOnEveryPlatform(p.type, p.isBindable())) || p.isDecimal();

			if (addNullCheck) 	{ a("\t\tif ("); a(localProp); a(".notNull())"); }
			a("\t\t"); a(classProp); a(" = "); a(val); a(";\n");
			closePlatformCode(p);
		}
		
		magic.constructor(code);

		if (genSuperCall)
		{
			a("\t\tsuper(");
			var first = true;
			var sorted = def.propertiesSorted;
			for (p in sorted) if (Util.isDefinedInSuperClassOf(def, p))
			{
				openPlatformCode(p, false);
				if (first) first = false; else a(", ");
				a(p.name); a("_");
				closePlatformCode(p, false);
			}
			a(");\n");
		}
		else
			a("\t\tinit();\n");
		

		//
		// find out and define which properties are set
		//

		var setProperties 		= new Array<String>();
		var shouldGenerateInit 	= false;

		for (p in def.propertiesSorted) if (!Util.isDefinedInSuperClassOf(def, p))
		{
			if (!p.isTransient()) {
				var localProp 	= p.name + "_";
				var flag 		= hexBitflag(p.bitIndex());
				var checks 		= getAllEmptyChecks( p, localProp, false );

				if (checks != null)		setProperties.push( "(" + flag + " * (" + checks + ").boolCalc())" );
				else 					setProperties.push( flag );
			}

			if (!shouldGenerateInit && (p.isBindable() || p.isArray()))
				shouldGenerateInit = true;
		}
		if (setProperties.length > 0) {
			a("\t\t_propertiesSet"); a(genSuperCall ? " |= " : " = "); a(setProperties.join(" | ")); a(";\n");
		}

		a("#if debug\tAssert.equal(_changedFlags, 0); #end\n");
	//	a("\t\t_changedFlags = 0;\n");
		a("\t}\n");

		if (shouldGenerateInit)
			genInit(def);
	}


	function genInit (def:ClassDef)
	{
		a("\n\n\toverride private function init()\n\t{\n");
		a("\t\tsuper.init();\n");

		for (p in def.propertiesSorted) if (!Util.isDefinedInSuperClassOf(def, p) && (!Util.isSingleValue(p.type) || (p.isBindable() || p.isArray())))
		{
			var isArray 		= p.isArray();
			var isSingleValue	= Util.isSingleValue(p.type);
			var flagName 		= p.name.toUpperCase();
			var hasGetter 		= p.shouldHaveGetter();
			
		//	if (p.isTransient() || p.isMixin()) {	//FIXME: Temporary.. isMixin won't detect classes that are used as mixins but are not defined as mixins.. To solve this, move all the unpack code to Reader
			if (!hasGetter || p.isBindable() || p.isArray()) {
				a("\t\t"); a("if ("); a(p.name); a(".isNull())\t"); 
				if (p.shouldHaveSetter())
					a("(untyped this).");	
				
				a(p.name); a(" = "); a(HaxeUtil.getConstructorCall(p.type, p.isBindable(), HaxeUtil.getConstructorInitializer(p.type, true), p.isTransient())); a(";\n");
			}

			if (!p.isTransient())
			{
				if (isArray || p.isBindable()) {
					// listen to changes of the array or bindable
					a("\t\t"); a(p.name); a(".change."); a(isArray ? "observe" : "bind"); a("(this, "); a(p.name); a("Changed);\n");
				}

				// listen to changes of value-objects within the array or bindable
				if (!isSingleValue)
				{
					if (isArray) {
						a("\t\t"); a(p.name); a(".setChangeHandler(objectChangedHandler("); a(flagName); a("));\n");
					}
					else
					{	// it's a bindable or vo
						var val = p.isBindable() ? p.name + ".value" : p.name;
						a("\t");
						if (hasGetter) {
							a("\tif (IfUtil.notNull((untyped this)."); a(val); a("))");
						} else if (p.isBindable()) {
							a("\tif ("); a(val); a(".notNull())");
						}
						a("\tValueObjectBase.addChangeListener( "); a(val); a(", this, objectChangedHandler("); a(flagName); a("));\n");
					}
				}
			}
		}

		a("\t}\n");
	}

	
	function genGetter(p:Property, immutable:Bool)
	{
		var type = HaxeUtil.haxeType(p.type, true, p.isBindable(), false, p.isTransient());

		if (p.description != null)
			addComment(p.description);
		
		a("\tpublic var "); a(p.name);


		var genGetterFn = p.shouldHaveGetter();
		var genSetterFn = !immutable && p.shouldHaveSetter();
		var transient   = !immutable && p.isTransient();
		
		if 		(genGetterFn)		{ a("\t(get"); code.addCapitalized(p.name); }
		else						{ a("\t(default"); }
		
		if 		(genSetterFn)		{ a(", set");  code.addCapitalized(p.name); }
		else if (transient)			{ a(", default"); }
		else						{ a(", null"); }
		
		a(") : "); a(type); a(";\n");

		// optional hasProperty()
		if (p.isOptional()) {
			if (immutable)		{ a("\tpublic function has"); code.addCapitalized(p.name); a(" (): Bool;\n"); }
			else				{ a("\tpublic inline function has"); code.addCapitalized(p.name); a(" (): Bool { return has("); a(p.name.toUpperCase()); a("); }\n"); }
		}

		if (!immutable && genGetterFn) {
			a("\tprivate function get"); code.addCapitalized(p.name); a(" () { return this."); a(p.name); a(".notNull()? this."); a(p.name); a(" : this."); a(p.name); a(" = ");
			a(HaxeUtil.getConstructorCall(p.type, p.isBindable(), HaxeUtil.getConstructorInitializer(p.type), transient)); a(";");
			a(" }\n\n");
		}
	}
	
	function genSetter(p:Property)
	{
		Assert.that(!p.isArray());
		Assert.that(!p.isBindable());

		var typeName			= HaxeUtil.haxeType(p.type, true, false, false, p.hasOption(transient));
		var hasGetter			= p.shouldHaveGetter();
		var name				= hasGetter ? "(untyped this)." + p.name : p.name;
		var isSingleValue 		= Util.isSingleValue(p.type);
		
		
		a("\tpublic function set"); code.addCapitalized(p.name); a("(newV:"); a(HaxeUtil.haxeType(p.type, true, p.isBindable(), false, p.hasOption(transient))); a(")\n\t{\n");
		
	//	a("\t\treturn if (v == "); a(name); a(") v;\n");
	//	a("\t\telse\n\t\t{\n\t\t\tif (isEditable()) _changedFlags |= "); a(hexBitflag(p.bitIndex())); a(";\n");
		
		a("\t\tvar oldV = "); a(name); a(";\n");
		a("\t\tif (newV != oldV)\n");
		a("\t\t{\n");
		
		if (!isSingleValue)
		{
			a("\t\t\tif (oldV.notNull()) {\n");
			a("\t\t\t\tValueObjectBase.removeChangeListener( oldV, this );\n");
			a("\t\t\t}\n");

			a("\t\t\tif (newV.notNull()) {\n");
			a("\t\t\t\tValueObjectBase.addChangeListener( newV, this, objectChangedHandler("); a(p.name.toUpperCase()); a("));\n");
			a("\t\t\t\tif (newV.isEmpty()) "); addPropChangeFlagUnsetter(p.bitIndex()); a(" else _propertiesSet |= "); a(hexBitflag(p.bitIndex())); a(";\n");
			a("\n\t\t\t}");
			a("\n\t\t\telse "); addPropChangeFlagUnsetter(p.bitIndex());
		}
		else {
			a("\t\t\t");
			addPropChangeFlagSetter(p.bitIndex(), "newV" + ((!p.isArray() && p.isBindable())? ".value" : ""), p.type, false);
		}
		
		a("\n\t\t\t");
		a("\n\t\t\t_changedFlags |= "); a(hexBitflag(p.bitIndex())); a(";");
		a("\n\t\t\tthis."); a(p.name); a(" = newV;\n");
		a("\t\t}\n");
		a("\t\treturn newV;\n");
		a("\t}\n\n");
	}


	private function genChangeWatcher (p:Property)
	{
		Assert.that(p.isArray() || p.isBindable());
		var isSingleValue 	= Util.isSingleValue(p.type);
		var isArray 		= p.isArray();

		a("\tprivate function "); a(p.name); a("Changed(");
		
		if (!isArray) {
			var typeName = HaxeUtil.haxeType(p.type, true, false, false, p.hasOption(transient));
			a("newV : "); a(typeName); a(", oldV : "); a(typeName);
		}
		a(") : Void {\n\t\t");
		a("_changedFlags |= "); a(hexBitflag(p.bitIndex())); a(";\n\t\t");
		
		if (p.isBindable() && !isArray && !isSingleValue) {
			a("if (oldV.notNull())		ValueObjectBase.removeChangeListener( oldV, this );\n\t\t");
			a("if (newV.notNull())		ValueObjectBase.addChangeListener( newV, this, objectChangedHandler("); a(p.name.toUpperCase()); a("));\n\t\t");
		}

		if (isArray)
		{
			a("if ("); a(p.name); a(".length.not0()) _propertiesSet |= "); a(hexBitflag(p.bitIndex())); a("; else "); addPropChangeFlagUnsetter(p.bitIndex());
		}
		else
			addPropChangeFlagSetter(p.bitIndex(), "newV", p.type, false);
		a("\n\t}\n\n");
	}

	
	private function addPropChangeFlagSetter(bit, path, ptype, bindable)
	{
		var ifAdded = !addIfVarIsSetExpr(path, ptype, bindable, "_propertiesSet |= " + hexBitflag(bit));
		
		if (ifAdded) {
			a(" else "); addPropChangeFlagUnsetter(bit);
		}
		
		return ifAdded;
	}
	
	private function addPropChangeFlagUnsetter(bit) {
		a("_propertiesSet &= 0x" + StringTools.hex(-1 ^ (1 << bit)) + ";");
	}
	
	
	public function genEnum(def:EnumDef) : Void
	{
		this.code = new StringBuf();
		var a = this.code.add;
		
		if (def.module.name.length > 0) {
			a("package "); a(def.module.fullName); a(";\n");
		}
		a("enum "); a(def.name); a("\n{\n");
		
		for (e in def.enumerations)
		{
			a("\t"); a(e.name);
			if (e.type == Tstring)
				a("(s:String)");
			a(";\n");
		}
		
		a("}");
		
		write(def.name);
		
		// -----------
		// Utils class
		// -----------
		this.code = new StringBuf();
		a = this.code.add;
		
		if (def.module.name.length > 0) {
			a("package "); a(def.module.fullName); a(";\n");
		}
		a("class "); a(def.utilClassName); a("\n{\n");
		
		// toValue
		a("\tstatic public function toValue(e:"); a(def.fullName); a(") : Int\n\t{\n\t\t");
		a("Assert.that(e != null); return switch (e) {");
		for (e in def.enumerations)
		{
			a("\n\t\t  case "); a(e.name);
			if (e.type != null) {
				a("(_): primevc.types.Number.INT_NOT_SET");
			}
			else {
				a(': ' + e.intValue);
			}
		 	a(";");
		}
		a("\n\t\t}\n\t}\n");
		// fromValue
		a("\t@:keep static public function fromValue(i:Int) : "); a(def.fullName); a("\n\t{\n\t\t");
		a("return switch (i) {");
		for (e in def.enumerations)
		{
			a("\n\t\t  case ");
			a(e.intValue + ': ');
			a(def.fullName); a("."); a(e.name);
			if (e.type != null) a("(null)");
			a(";");
		}
		a("\n\t\t}\n\t}\n");
		
		
		// other conversions
		for (conv in def.conversions)
		{
			//to method
			a("\tstatic public function "); a(conv.name); a("(e:"); a(def.fullName); a(") : String\n\t{\n\t\t");
			a("return e == null? '"); Std.string(conv.defaultValue); a("' : switch (e) {");
			for (e in conv.enums) {
				a("\n\t\t  case "); a(e.name);
				if (e.type != null) {
					a("(v): Std.string(v);");
				}
				else {
					a(': "');  a(e.conversions.get(conv.name)); a('";');
				}
			}
			var requiresDefaultCase = conv.enums.length < Lambda.count({iterator: def.enumerations.keys});
			if (requiresDefaultCase)
				a("\n\t\t  default: toString(e);");
			
			a("\n\t\t}\n\t}\n");
			
			//from method
			a("\tstatic public function from"); a(conv.name.substr(2)); a("(str:String) : "); a(def.fullName); a("\n\t{\n\t\t");
			a("return switch (str) {");
			for (e in conv.enums) if (e != def.catchAll)
			{
				a('\n\t\t  case "'); a(e.conversions.get(conv.name)); a('": '); a(def.fullName); a("."); a(e.name); a(";");
			} else {
				a("\n\t\t  default: if (str != null) "); a(def.fullName); a("."); a(e.name); a('(str);');
			}
			if (requiresDefaultCase)
				a("\n\t\t  default: fromString(str);");
			
			a("\n\t\t}\n\t}\n");
		}
		
		a("}");
		write(def.utilClassName);
	}
	
	public function toCode() : String
	{
		return code.toString();
	}
	
	private function genXMLMapFunctions(def:BaseTypeDefinition, magic)
	{
		var mapToHaxe = new HaxeXMLMap(code, def, magic);
	//	a("#if (VO_Write || !VO_Read)\n");
	/*	To XML generates incorrect code.
		toXML() should be split into 2 functions:
		- set values to a given node
		- create a childnode in the given node (and call the set function?)
	*/
	//	TODO: Fix toXML generation
	//	mapToHaxe.genToXML();
	//	a("\n#end\n");
		a("#if (VO_Read || !VO_Write)\n");
		mapToHaxe.genFromXML();
		a("\n#end\n");
	}



	private inline function openPlatformCode(p:Property, useNewLine:Bool = true)
	{
		if (p.isPlatformSpecific()) {
			a("#if ("+p.platforms.join(" || ")+")");
			if (useNewLine)
				a("\n");
		}
	}


	private inline function closePlatformCode (p:Property, useNewLine:Bool = true)
	{
		if (p.isPlatformSpecific()) {
			a("#end");
			if (useNewLine)
				a("\n");
		}
	}
}

private class CodeBufferer
{
	var code:StringBuf;
	
	private inline function a(s:String) {
		code.add(s);
	}
}

private class MagicClassGenerator extends CodeBufferer
{
	public function new() {}
	
	public function fromXML(code:StringBuf) {}
	public function inject(code:StringBuf) {}
	public function constructor(code:StringBuf) {}
}

private class NamedSetDefGenerator extends MagicClassGenerator
{
	var ns:NamedSetDef;
	var otherProp:String;
	
	public function new(def:ClassDef) {
		super();
		this.ns = cast def;
		
		otherProp = "other" + ns.itemName;
	}
	
	override public function constructor(code:StringBuf)
	{
		this.code = code;
		
		a("\t\n\t\tthis."); a(otherProp);
		a(" = new List();\n");
	}
	
	override public function inject(code:StringBuf)
	{
		this.code = code;
		
		// Properties
		a("\t public var "); a(otherProp); a(" (default, null) : List<"); a(HaxeUtil.haxeType(ns.baseType, true)); a(">;\n\n");
		
		// add()
		a("\tpublic function add(item:"); a(HaxeUtil.haxeType(ns.baseType));
		a(")\n\t{\n\t\t");
		
		
		if (ns.keys.length > 0) // No keys, no way to map...
		{
			var firstProp = true;
			for (p in ns.property)
			{
				if (firstProp) firstProp = false;
				else a("\t\telse ");
			
				a("if (");
				var firstKey = true;
				for (k in ns.keys)
				{
					if (!firstKey) { a(" && "); }
					else firstKey = false;
				
					a(HaxeUtil.compareValueCode("item."+k.path, k.prop.type, ns.getValueByPath(p.defaultValue, k.path)));
				}
				a(") this."); a(p.name); a(" = item;\n");
			}
			a("\t\telse ");
		}
		
		a("this."); a(otherProp); a(".add(item);\n\t}\n");
		
		
		// Iterator
		a("\tpublic inline function iterator() : Iterator<"); a(HaxeUtil.haxeType(ns.baseType, true));
		a(">\n\t{\n");
		
		var casenum = -1;
		a("
		var i = 0, self = this, other = "); a("this."); a(otherProp); a(".iterator();
		return {
			next: function() return switch (i++) {");
		for (p in ns.property) {
			a("\n\t\t\t\tcase " + (++casenum)); a(": self."); a(p.name); a(";");
		}
		
		a("
				default: other.next();
			},
			hasNext: function() return i < "); a(Std.string(casenum)); a(" || other.hasNext()
		}\n");
		
		a("\t}\n");
	}
	
	override public function fromXML(code:StringBuf)
	{
		this.code = code;
		
		if (ns.keys.length == 0) return; // No keys, no way to map...
		
		a("\t\tfor (child in node.elements()) {\n");
		
		a("\t\t\tvar tmp");
		var c = HaxeUtil.getConstructorCall(ns.baseType, false, HaxeUtil.getConstructorInitializer(ns.baseType));
		if (c != null) { a(" = "); a(c); a(";"); }
		else throw "non constructable type? " + ns;
		
		a("\n\t\t\ttmp.setFromXML(child);");
		a("\n\t\t\tthis.add(tmp);");
		a("\n\t\t}\n"); // end for
	}
}

private class HaxeUtil
{
	public static function getConstructorCall(ptype:PType, bindable:Bool, initializer:String, transient:Bool = false)
	{
		switch (ptype) {
			case Tarray(type, _,_):		return "new " + HaxeUtil.haxeType( ptype, true, bindable, false, transient ) + "("+ initializer +")";
			case TuniqueID:				initializer = initializer + ' == null? primevc.types.ObjectId.make() : ' + initializer;
			default:
		}
		
		return 	if (bindable)			"new "+ HaxeUtil.haxeType(ptype, true, bindable, false, transient) +"("+ initializer +")";
				else					initializer;
	}
	
	public static function getConstructorInitializer(ptype:PType, constOnly = false) return switch (ptype)
	{
		case Tstring:				"''";
		case Tbool(val):			val? "true" : "false";
		case Tarray(type, _,_):		constOnly? "null" : "[]";
		
//		case Turi, TfileRef, Tinterval:
//			"new " + HaxeUtil.haxeType(ptype) + "()";
		
		case Tdef(type):
			if (constOnly) null else getClassConstructorCall(Util.unpackPTypedef(type));
		
		case Tinteger(_,_,_):		'primevc.types.Number.INT_NOT_SET';
//		case Tdecimal(_,_,_):		'primevc.types.Number.FLOAT_NOT_SET';
		case Temail:				"''";
		
		case Turi, Turl, TfileRef, Tinterval, Tdecimal(_,_,_), TuniqueID, TenumConverter(_), Tdate, Tdatetime, Tcolor, TclassRef(_):
			null;
	}
		
	public static function getClassConstructorCall(tdef:TypeDefinition)
	{
		// Dont 'new' enums.
		return if (!Std.is(tdef, EnumDef))
			"new " + tdef.fullName + "VO()";
		  else null;
	}
	
	public static function isNullableOnEveryPlatform(ptype:PType, bindable:Bool) return switch (ptype)
	{
		case Tbool(_), Tinteger(_,_,_), Tdecimal(_,_,_), Tcolor:
			bindable;
		case Tstring,Tdate,Tdatetime,Tinterval,Turi,Turl,TuniqueID,Temail,TfileRef, Tdef(_), TenumConverter(_), Tarray(_,_,_), TclassRef(_):
			true;
	}
	
/*	- Useless effort?
	
	public static function getDynamicConversionCall(ptype:PType, bindable:Bool) return switch (ptype)
	{
		case Tbool(val):				'ConvertTo.bool(';
		case Tinteger(min,max,stride):	'ConvertTo.int(';
		case Tdecimal(min,max,stride):	'ConvertTo.float(';
		case Tstring:					'ConvertTo.string(';
		case Tdate:						'ConvertTo.date(';
		case Tdatetime:					'ConvertTo.date(';
		case Tinterval:					'ConvertTo.dateInterval(';
		case Turi:						bindable? 'ConvertTo.bindableURI(' : 'ConvertTo.uri(';
		case TuniqueID:					'ConvertTo.uniqueID(';
		case Temail:					'ConvertTo.eMail(';
		case Tcolor:					'ConvertTo.rgba(';
		case TfileRef:					'ConvertTo.fileRef(';
		
		case Tdef(ptypedef):			switch (ptypedef) {
			case Tclass		(def):		'ConvertTo.valueobject('+ def.fullName +', ';
			case Tenum		(def):		'ConvertTo.enum('+ def.fullName +', ';
		}
		case TenumConverter(enums):		'ConvertTo.string(';
		
		case Tarray(type, min, max):
			if (bindable)
		 		'ConvertTo.revertableArrayList('+ haxeType(type, true, false) +', ';
			else
			 	'ConvertTo.fastArray('+ haxeType(type, true, false) +', ';
	}
*/	
	public static function haxeType(ptype:PType, ?immutableInterface:Bool = false, ?bindable:Bool = false, ?constructorArg = false, ?transient = false)
	{
		var type = switch (ptype)
		{
			case Tbool(val):				'Bool';
			case Tinteger(min,max,stride):	'Int';
			case Tdecimal(min,max,stride):	'Float';
			case Tstring:					'String';
			case Tdate:						'Date';
			case Tdatetime:					'Date';
			case Tinterval:					'primevc.types.DateInterval';
			case Turi:						'primevc.types.URI';
			case Turl: 						'primevc.types.URL';
			case TuniqueID:					'primevc.types.ObjectId';
			case Temail:					'primevc.types.EMail';
			case Tcolor:					'primevc.types.RGBA';
			case TfileRef:					'primevc.types.FileRef';
			case TclassRef(classPath):		classPath;
			
			case Tdef(ptypedef):			switch (ptypedef) {
				case Tclass		(def): (immutableInterface? def.module.fullName + ".I" + def.name : def.fullName) + "VO";
				case Tenum		(def): def.fullName;
			}
			case TenumConverter(enums):		'String';
			
			case Tarray(type, min, max):
				return if (!constructorArg)
				{
					'primevc.core.collections.' +
					 	(Util.isSingleValue(type)
							? (bindable? 'RevertableArrayList<'   : 'ArrayList<')
							: (bindable? 'RevertableVOArrayList<' : 'VOArrayList<'))
						+ haxeType(type, true)  + '>';
				}
				else
				 	'primevc.utils.FastArray<'+ haxeType(type, true) +'>';
		}
		
		if (bindable)
		{
			// Check for Bindable
			type = transient ? "primevc.core.Bindable<"+type+">" : "primevc.core.RevertableBindable<"+type+">";
		}
		
		return type;
	}
	
	public static function haxeValueTypeEnum(ptype:PType) return "ValueType." + switch (ptype)
	{
		case Tbool(val):				'TBool';
		case Tinteger(min,max,stride):	'TInt';
		case Tdecimal(min,max,stride):	'TFloat';
		case Tstring:					'TClass(String)';
		case Tdate:						'TClass(Date)';
		case Tdatetime:					'TClass(Date)';
		case Tinterval:					'TClass(primevc.types.DateInterval)';
		case Turi:						'TClass(primevc.types.URI)';
		case Turl:						'TClass(primevc.types.URL)';
		case TuniqueID:					'TClass(primevc.types.ObjectId)';
		case Temail:					'TClass(primevc.types.EMail)';
		case Tcolor:					'TClass(primevc.types.RGBA)';
		case TfileRef:					'TClass(primevc.types.FileRef)';
		case TclassRef(className):		'TClass('+className+')';
		
		case Tdef(ptypedef):			switch (ptypedef) {
			case Tclass		(def): "TClass("+ def.module.fullName + "VO)";
			case Tenum		(def): "TEnum(" + def.fullName + ")";
		}
		case TenumConverter(enums):		'TClass(String)';
		
		case Tarray(type, min, max):
		 	'TClass(primevc.utils.FastArray<'+ haxeType(type, true) +'>)';
	}
	
	public static function compareValueCode(path:String, type:PType, val:Dynamic) : String
	{
		return switch (type)
		{
			case Tbool(_), Tinteger(_,_,_), Tdecimal(_,_,_):
				path +" == "+ Std.string(val);
			
			case Tstring:
				path +" == "+ '"'+ val +'"';
			
			case Turi, Turl, TfileRef:
				path +'.string == "'+ Std.string(val) +'"';
			
			case Tdate, Tdatetime, TuniqueID, Temail:
				haxeType(type) + '.fromString("' + Std.string(val) + '")';
			
			case Tcolor:
				if (Std.is(val, String))
					haxeType(type) + '.fromString("' + Std.string(val) + '")';
				else
					StringTools.hex(val, 6);
			
			case TenumConverter(enums):
			//	this.
			//	'String';
				throw "Unsupported value literal";
			
			case Tdef(_), Tinterval, Tarray(_,_,_), TclassRef(_):
				throw "Unsupported value literal: "+type;
		}
		/*
		return if (Std.is(val, String)) val; else switch (Type.typeof(val))
		{
			case TNull, TInt, TFloat, TBool:
				Std.string(val);
			
			case TEnum(e):		Std.string(val);
			case TClass(c):		Std.string(val);
			
			case TUnknown, TObject, TFunction:
				throw "Unsupported value";
		}
		*/
	}
}

private class HaxeXMLMap extends CodeBufferer
{
	static inline var xmlstring = "XMLString.";
	
	var def:BaseTypeDefinition;
	var map:XMLMapping;
	var magic:MagicClassGenerator;
	
	public function new (code:StringBuf, def:BaseTypeDefinition, magic:MagicClassGenerator)
	{
		this.code	= code;
		this.def	= def;
		this.map	= def.getOptionDef(XMLMapping);
		this.magic	= magic;
	}
	
	public function genFromXML()
	{
		var a = code.add;
		// From
		a("\n\tpublic static function fromXML(node:Xml) : "+def.fullName+"VO\n\t{\n");
		
		if (map != null && map.root != null)
			gen_fromXMLValueToTypeMapping(map.root, "node", 1);
		
		a("\t\tvar o = "); a(HaxeUtil.getClassConstructorCall(def)); a(";\n");
		a("\t\to.setFromXML(node);\n");
		
		a("\t\treturn o;\n");
		a("\t}\n");
		
		a("\n\t");
	//	a('@:keep ');
		var has_super = add_override();
		a("public function setFromXML(node:Xml) : Void\n\t{\n");
		
		if (has_super) a("\t\tsuper.setFromXML(node);\n");
		
		if (map != null && map.root != null)
			gen_fromXMLNode(map.root, "node", 1);
		
		magic.fromXML(code);
		a("\t}\n");
	}
	
	function addTabs(num:Int)
	{
		for (i in 0 ... num+1) code.addChar('\t'.code);
	}
	
	function gen_fromXMLNode(node:XMLMapNode, nodeVarname:String, identLevel:Int) : Bool
	{
		var code_added = false;
		
		// This node's stuff
		code_added = gen_fromNodeAttr(node, nodeVarname, identLevel) || code_added;
		
		for (c in node.children) if (isMultiChildXMLMap(c.value)) {
			code_added = true;
			addTabs(identLevel); gen_fromXMLConversion(c, c.value, nodeVarname, null);
		}
		if (node.value != null && node.valuePath != null && !isMultiChildXMLMap(node.value)) {
			code_added = gen_fromNodeValue(node, nodeVarname, identLevel) || code_added;
		}
		
		// Children
		return doWithChildren(node, nodeVarname, identLevel, gen_fromXMLNode) || code_added;
	}
	
	static function isMultiChildXMLMap(value)
	{
		return value != null && switch (value)
		{
			default: false;
			case XM_children(_,_): true;
		}
	}
	
	static function isChildXMLMap(value)
	{
		return value != null && switch (value)
		{
			default: false;
			case XM_children(_,_), XM_child(_,_): true;
		}
	}
	
	function doWithChildren(node:XMLMapNode, nodeVarname:String, identLevel:Int, dg:XMLMapNode->String->Int -> Bool)
	{
		var code_added = false;
		var childNodeVarname = node.nodeName + "_child";
		
		if (node.children.length == 1)
		{
			if (node == node.mapping.root || isChildXMLMap(node.children[0].value)) {
				// childNode()
				childNodeVarname = nodeVarname;
			}
			else {
				// child hierachy
				addTabs(identLevel); a("/* hier */ var "); a(childNodeVarname); a(" = "); a(nodeVarname); a(".firstElement();\n");
			}
			
			if (node.children[0] == null) throw node;
			code_added = dg(node.children[0], childNodeVarname, identLevel+1);
		}
		else if (node.children.length > 1)
		{
			var oldbuf = this.code;
			this.code = new StringBuf();
			
			addTabs(identLevel); a("\n");
			addTabs(identLevel); a("for ("); a(childNodeVarname); a(" in "); a(nodeVarname); a(".elements()) switch("); a(childNodeVarname); a(".nodeName)\n");
			addTabs(identLevel); a("{\n");
			
			for (c in node.children) if (!isMultiChildXMLMap(c.value))
			{
				var oldbuf2 = this.code;
				var caseBuf = this.code = new StringBuf();
				
				var case_added = dg(c, childNodeVarname, identLevel+2);
				this.code = oldbuf2;
				
				if (case_added)
				{
					code_added = true;
					addTabs(identLevel+1);
					a("case \""); a(c.nodeName); a('":\n');
					a(caseBuf.toString());
					addTabs(identLevel+1);
					a("\n");
				}
			}
			
			if (code_added) {
				addTabs(identLevel); a("}\n");
				oldbuf.add(code.toString());
			}
			
			this.code = oldbuf;
		}
		
		return code_added;
	}
	
	function gen_fromXMLValueToTypeMapping(node:XMLMapNode, nodeVarname:String, identLevel:Int) : Bool
	{
		var code_added = false;
		
		// Handle value->type mappings first
		if (node.attributes != null) for (attr in node.attributes.keys()) switch (node.attributes.get(attr))
		{
			case XM_typeMap(map):
				var a = code.add;
				addTabs(identLevel); a("switch ("); a(nodeVarname); a('.get("'); a(attr); a('")'); a(") {\n");
				
				for (k in map.keys()) {
					code_added = true;
					addTabs(identLevel + 1); a('case "'); a(k); a('": return '); a(map.get(k).fullName); a("VO.fromXML("); a(nodeVarname); a(");\n");
				}
				
				addTabs(identLevel); a("}\n");
			
			default:
		}
		
	//	return code_added;
		return doWithChildren(node, nodeVarname, identLevel, gen_fromXMLValueToTypeMapping) || code_added;
	}
	
	function gen_fromNodeValue(node:XMLMapNode, nodeVarname:String, identLevel:Int)
	{
		if (node.valuePath == null) switch (node.value)
		{
			case XM_empty:
				return false;
			case XM_String(v):
				throw "how?!";
				code.addChar('"'.code); a(v); code.addChar('"'.code);
				return true;
			
			default:
				throw node.nodeName + " has no valuepath ->" + node;
		}
		
		var oldbuf = this.code;
		this.code = new StringBuf();
		addTabs(identLevel);
		if (!isChildXMLMap(node.value)) {
			a("if ("); a(nodeVarname); a(".firstChild() != null) ");
		}
		
		var code_added = gen_fromXMLConversion(node, node.value, nodeVarname, ".firstChild().nodeValue");
		if (code_added)
			oldbuf.add(code.toString());
		
		this.code = oldbuf;
		
		return code_added;
	}
	
	function gen_fromNodeAttr(node:XMLMapNode, nodeVarname:String, identLevel:Int) : Bool
	{
		if (node == null || node.attributes == null) return false;
		
		var code_added = false;
		
		// Handle value conversions
		var attr;
		for (attrName in node.attributes.keys()) switch (attr = node.attributes.get(attrName))
		{
			case XM_typeMap(map):
				// Ignore typemaps in this pass
			case XM_String(str):
				// Ignore strings which arent mapped to any property
			
			default:
				addTabs(identLevel);
				gen_fromXMLConversion(node, attr, nodeVarname, ".get(\""+attrName+"\")");
				code_added = true;
		}
		
		return code_added;
	}
	
	function gen_setObjProperty(path:String, bindable:Bool)
	{
		if (path.indexOf(".") > 0) code.add("untyped ");
		code.add("this.");
		code.add(path);
		if (bindable) code.add(".value");
		code.add(" = ");
	} 
	
	function gen_fromXMLConversion(node:XMLMapNode, xtype:XMLMapValue, nodeVarname:String, nodeValueAccess:String, ?propPrefix="") : Bool
	{
		var a = this.code.add;
		var source = nodeVarname + nodeValueAccess;
		
		switch(xtype)
		{
			case XM_String(v):
				code.addChar('"'.code); code.add(v); code.addChar('"'.code);
			
			case XM_binding(path,prop):
				if (Util.isSingleValue(prop.type)) {
					a(getFromXMLConversionFunctionCall(prop.type, source, path, prop.isBindable()));
					code.add(";\n");
				}
				else if (!Util.isPTypeBuiltin(prop.type)) {
					a("this.");
					a(path);
					if (prop.isBindable()) a(".value");
					a(".setFromXML("); a(nodeVarname); a(');\n');
				}
				else {
					throw "impossible ?";
				}
				
			case XM_toInt		(path,prop):
				gen_setObjProperty(path, prop.isBindable());
				code.add(propPrefix);
				code.add(getFromIntConversion(source, prop.type));
				code.add(";\n");
			
			case XM_not			(val): 
				gen_fromXMLConversion(node, val, nodeVarname, nodeValueAccess, "!");
			
			case XM_children	(path, from), XM_child(path, from):
				if (!Util.isPTypeBuiltin(from.type)) {
				//	a(Util.unpackPTypedef(Util.getPTypedef(from.type)).fullName);
					var fullTypeName = switch(from.type) {
						case Tdef(t): Util.unpackPTypedef(t).fullName;
						default: throw "impossible";
					}
					var varname = path.split(".").join("_") + "_vo";
					a("var "); a(varname); a(" : "); a(fullTypeName); a("VO = cast this."); a(path); a(";  ");
					a(varname); a(".setFromXML("); a(nodeVarname); a(");\n");
				}
				else if(Util.isPTypeIterable(from.type)) {
					a("for (x in "); a(nodeVarname); a(".elements()){ this."); a(path); a(".push(");
					a(Util.unpackPTypedef(Util.getPTypedef(from.type)).fullName); a("VO.fromXML(x)); }\n");
				}
				else
					throw "impossible";
			
			case XM_join		(p,v,sep):
				gen_setObjProperty(node.valuePath, false);
				a(nodeVarname); a(".firstChild().nodeValue.split(\""); a(sep); a("\");\n");
				
			case XM_format		(path, prop, format):
				code.add(getFromXMLConversionFunctionCall(prop.type, source, path, prop.isBindable()));
				code.add(";\n");
			
			
			case XM_concat		(values):
				throw "Little hard to implement";
				// tmp = node.get(); for v in values .. switch (v)
				// case XM_String(s): startPos = s.length;
				// case XM_binding(...):  tmp.substr(startPos, tmp.indexOf(values[i+1].length ?? ))
				trace("whut?");
				trace(code.toString());
			
			
			case XM_typeMap		(_):		throw "impossible";
			
			case XM_empty:
				return false;
		}
		
		return true;
	}
	
	private function add_override()
	{
		if (Std.is(def, ClassDef) && cast(def, ClassDef).superClass != null) {
			code.add("override ");
			return true;
		}
		return false;
	}
	
	// ------
	// To XML
	// ------
	
	public function genToXML()
	{
		var a = code.add;
		
		// To
		a("\n\t"); add_override(); a("public function toXML(parent:Xml) : Void\n\t{");
		
		if (map != null && map.root != null)
		 for (i in 0 ... map.root.children.length)
		{
			var childNode = map.root.children[i];
			var nodeVarname = 'node'+i+'__'+ childNode.nodeName;
			
			a("\t\t\n\t\t// <"); a(childNode.nodeName); a(">\n");
			a("\t\tvar "); a(nodeVarname); a(' = Xml.createElement("'); a(childNode.nodeName); a('");\n');
			a("\t\tparent.addChild("); a(nodeVarname); a(");\n");
			genNode_toAttr(nodeVarname, childNode, 2);
			
			if (childNode.children.length == 0)
			{
				if (childNode.value != null && childNode.value != XM_empty) {
					addTabs(2);
					genToXMLNodeConversion(nodeVarname, childNode.value);
					a("\n");
				}
			}
			else for (ci in 0 ... childNode.children.length)
			{
				var subChild = childNode.children[ci];
				genChildNode_toXML(nodeVarname, 'node'+i, subChild, ci, 2);
			}
		}
		
		a("\t}\n");
	}
	
	function genNode_toAttr(varname:String, child:XMLMapNode, identLevel:Int)
	{
		if (child == null || child.attributes == null) return;
		
		var a = this.code.add;
		for (attr in child.attributes.keys())
		{
			addTabs(identLevel);
			a(varname);
			a('.set("');
			a(attr);
			a('", ');
			genToXMLValueConversion(varname, child.attributes.get(attr));
			a(");\n");
		}
	}
	
	function genChildNode_toXML(parentVarname:String, varnamePrefix:String, child:XMLMapNode, childNum:Int, identLevel:Int)
	{
		var a = this.code.add;
		var varnameSuffix = '_child'+childNum+'__'+ child.nodeName;
		var varname = varnamePrefix + varnameSuffix;
		addTabs(identLevel);
		a("\n");
		addTabs(identLevel); a("var "); a(varname); a(' = Xml.createElement("'); a(child.nodeName); a('");\n');
		genNode_toAttr(varname, child, identLevel);
		
		if (child.children.length == 0)
		{
			if (child.value != null && child.value != XM_empty) {
				addTabs(identLevel);
				genToXMLNodeConversion(varname, child.value); //a(child.nodeName); a('");\n');
				a("\n");
			}
		}
		else for(si in 0 ... child.children.length)
			genChildNode_toXML(varname, varnamePrefix, child.children[si], si, identLevel+1);
		
		addTabs(identLevel); a(parentVarname); a(".addChild("); a(varname); a(");\n");
	}
	
	function genToXMLValueConversion(varname:String, v:XMLMapValue, ?propPrefix="")
	{
		switch(v)
		{	
			case XM_String		(v):
				code.addChar('"'.code); code.add(v); code.addChar('"'.code);
			
			case XM_typeMap		(map):
				var a = code.add;
				var arr = Lambda.array( { iterator: map.keys } );
				arr.reverse();
				for (val in arr) {
					a("Type.getClass(this) == "); a(map.get(val).fullName); a('? "'); a(val); a('" : ');
				}
				a('""');
			
			case XM_binding		(path,prop):
				if (!Util.isSingleValue(prop.type)) {
					throw "impossible: "+varname+" = "+v;
				}
				else {
					code.add(getToXMLConversionFunctionCall(prop.type, propPrefix+"this."+path, "this."+path));
				}
			
			case XM_toInt		(p,v):
				code.add(xmlstring);
				code.add("fromInt( ");
				code.add(getToIntConversion(propPrefix+"this."+p, v.type));
				code.add(" )");
			
			case XM_not			(v): 
				genToXMLValueConversion(varname, v, "!");
			
			case XM_format		(path,prop,format):
				var f = getToXMLConversionFunction(prop.type, path, path);
				code.add(f.fun);
				code.add("(");
				code.add(f.path);
				code.add(",\"");
				code.add(format);
				code.add("\")");
			
			case XM_join		(path,val,sep):
				var a = code.add;
				switch (val.type) {
					default:
						throw "Only array joins are supported.";
					
					case Tarray(elemT, min, max):
						switch(elemT) {
							default:
								throw "Only Array<String> joins are supported.";
							case Tstring:
								
						}
						a("this."); a(path); a(".join(\""); a(sep); a("\")");
				}
			
			case XM_concat		(values):
				for (i in 0 ... values.length) {
					genToXMLValueConversion(varname, values[i], propPrefix);
					if (i <= values.length - 2) code.add(" + ");
				}
				
				trace(code.toString());
				//throw "not implemented";
			
			case XM_children	(_,_), XM_child(_,_):	throw "impossible";
			case XM_empty:
		}
	}
	
	function genToXMLNodeConversion(varname:String, v:XMLMapValue)
	{
		var a = code.add;
		
		if (XMLMap.isSingleValue(v)) {
			a(varname);
			a(".addChild(Xml.createPCData( ");
			genToXMLValueConversion(varname, v);
			a(" ));");
		}
		else switch(v)
		{
			case XM_binding(path,prop):
				if (!Util.isPTypeBuiltin(prop.type)) {
					a(path);
					a(".toXML("); a(varname); a(");");
				}
				else throw "wut?";
			
			case XM_children	(path,from), XM_child(path,from):
				if (!Util.isPTypeBuiltin(from.type)) {
					a(path); a(".toXML("); a(varname); a(");\n");
				}
				else if(Util.isPTypeIterable(from.type)) {
					a("for (x in this."); a(path); a(") x.toXML("); a(varname); a(");\n");
				}
				else
					throw "impossible";
			
			case XM_toInt		(_,_)	: throw "not implemented";
			case XM_not			(_)		: throw "not implemented";
			case XM_join		(_,_,_)	: throw "not implemented";
			case XM_format		(_,_,_)	: throw "not implemented";
			case XM_concat		(_)		: throw "not implemented";
			case XM_String		(_)		: throw "not implemented";
			case XM_typeMap		(_)		: throw "not implemented";
			
			case XM_empty:
		}
	}
	
	function getToIntConversion(path:String, ptype:PType) : String
	{
		return switch (ptype)
		{
			case Tbool(val):				"("+path+")? 1:0";
			case Tinteger(min,max,stride):	path;
			case Tdecimal(min,max,stride):	"Std.int("+path+")";
			case Tstring:					"Std.int("+path+")";
			case Tcolor:					"Std.int("+path+")";
			case Tdate:						path+".getTime()";
			case Tdatetime:					path+".getTime()";
			
			case Turi:						throw "impossible Turi";
			case Turl:						throw "impossible Turl";
			case TfileRef:					throw "impossible TfileRef";
			case Tinterval:					throw "impossible Tinterval";
			case TuniqueID:					throw "impossible Tuniq";
			case Temail:					throw "impossible Temail";
			case Tdef(ptypedef):			throw "impossible int conversion Tdef: "+ptypedef;
			
			case TclassRef(className):		throw "not implemented "+ptype;
			case TenumConverter(enums):		throw "not implemented "+ptype;
			case Tarray(type, min, max):	throw "not implemented "+ptype;
		}
	}
	
	function getFromIntConversion(path:String, ptype:PType) : String
	{
		return switch (ptype)
		{
			case Tbool(val):				xmlstring+"toBool("+path+", "+val+")";
			case Tinteger(min,max,stride):	path;
			case Tdecimal(min,max,stride):	path;
			case Tstring:					path;
			case Tcolor:					path;
			case Tdate:						"Std.parseFloat("+path+")";
			case Tdatetime:					"Std.parseFloat("+path+")";
			
			case Tinterval:					throw "impossible";
			case Turi:						throw "impossible";
			case Turl:						throw "impossible";
			case TfileRef:					throw "impossible";
			case TuniqueID:					throw "impossible";
			case Temail:					throw "impossible";
			case Tdef(ptypedef):			throw "impossible";
			
			case TenumConverter(enums):		throw "not implemented";
			case Tarray(type, min, max):	throw "not implemented";
			case TclassRef(className):			throw "not implemented";
		}
	}
	
	
	function getToXMLConversionFunction(ptype:PType, path:String, property:String)
	{
		// to xml <---> from type
		return getXMLConversionFunction(ptype, path, "from", property);
	}
	
	function getXMLConversionFunction(ptype:PType, path:String, prefix:String, property:String) : {fun:String, path:String, property:String, setProp:Bool}
	{
		var setProp = true;
		
		var fun = switch (ptype)
		{
			case Tbool(val):				path += ", " + val; xmlstring+prefix+'Bool';
			case Tinteger(min,max,stride):	xmlstring+prefix+'Int';
			case Tdecimal(min,max,stride):	xmlstring+prefix+'Float';
			case Tstring, TfileRef:			if (prefix == "to") xmlstring+prefix+'String'; else null;
			case Turi:						if (prefix == "to") { setProp = false; "this." + property + ".parse"; } else xmlstring+prefix+'URI';
			case Turl:						if (prefix == "to") { setProp = false; "this." + property + ".parse"; } else xmlstring+prefix+'URL';
			case TuniqueID:					xmlstring+prefix+'ObjectId';
			case Temail:					xmlstring+prefix+'EMail';
			case Tcolor:					xmlstring+prefix+'Color';
			case Tdate:						xmlstring+prefix+'Date';
			case Tdatetime:					xmlstring+prefix+'Date';
			
			case Tdef(ptypedef):			
				switch (ptypedef) {
					case Tenum(e):
						prefix = prefix == "to" /* to Enum -> from String */? "from" : "to";
						e.utilClassPath +"."+ prefix+"String";
					
					case Tclass(_):
						setProp = false;
						if (prefix == "to")
							path + ".setFromXML(" + property + ")";
						else
							path + ".toXML(" + prefix + ")";
				}
			
			case TenumConverter(prop):
				var convFunction = (prefix == "to" /* to Enum -> from String */)? "from"+prop.name.substr(2) : prop.name;
				var p = path.split(".");
				p.pop(); // remove the converter 'property'
				if (prefix == "from") path = p.join(".");
				property = property.substr(0, property.lastIndexOf("."));
				
				/*'return'*/ prop.definition.utilClassPath+"."+convFunction;
			
			case Tinterval:					throw "Interval not supported as XML value";
			case TclassRef(_):				throw "Dynamic class-type not supported as XML value";
			case Tarray(type, min, max):	"function(arr){Lambda.iter(arr, "+getXMLConversionFunction(type, "arr", prefix, property).fun+");}";
		}
		
		return { fun: fun, path: path, setProp: setProp, property: property };
	}
	
	function getToXMLConversionFunctionCall(ptype:PType, path:String, property:String) : String {
		var f = getToXMLConversionFunction(ptype, path, property);
		return getXMLConversionFunctionCall(f.fun, f.path);
	}
	
	function getFromXMLConversionFunctionCall(ptype:PType, path:String, property:String, bindable) : String {
		// from xml String <---> to type
		var f = getXMLConversionFunction(ptype, path, "to", property);
		if (f.setProp) gen_setObjProperty(f.property, bindable);
		return getXMLConversionFunctionCall(f.fun, f.path);
	}
	
	function getXMLConversionFunctionCall(conv:String, path:String) : String
	{
		if (conv != null)
		{
			var buf = new StringBuf();
			var a = buf.add;
			
			a(conv);
			a("( ");
			a(path);
			a(" )");
			return buf.toString();
		}
		else
			return path;
	}
}
