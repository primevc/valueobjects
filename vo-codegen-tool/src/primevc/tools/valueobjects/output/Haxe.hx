package primevc.tools.valueobjects.output;
 import primevc.tools.valueobjects.VODefinition;
  using primevc.tools.valueobjects.output.Util;

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
			a(" import primevc.tools.valueobjects.xmlmap.XMLString;\n");
		}
	}
	
	private function genClassProperties(def:ClassDef, immutable:Bool) //, editable:Bool)
	{
		this.code = new StringBuf();
		var a = this.code.add;
		addPackage(def);
		
		if (immutable)
			a("\ninterface I"); 
		else {
			a("  using primevc.utils.IfUtil;\n");
			a("  using primevc.utils.msgpack.Format;\n");
			a("\nclass ");
		}
		
		a(def.name); a("VO");
		
//		var i = def.supertypes.length;
		if (def.superClass != null)
		{
			if (immutable) {
				a(" implements "); a(def.superClass.module.fullName); a(".I"); a(def.superClass.name); a("VO,");
			} else {
				a(" extends "); a(def.superClass.fullName); a("VO,");
			}
			
//			if (--i > 1) code.addChar(','.code);
		}
		
		if (immutable)
			a(" implements primevc.core.traits.IEditEnabledValueObject, implements primevc.core.traits.IValueObject");
		else {
			a(" implements "); a(def.module.fullName); a(".I"); a(def.name); a("VO,");
			a(" implements primevc.core.traits.IEditableValueObject");
		}
		
	/*	for (t in def.supertypes) if (t != def.superClass) {
			a(" implements "); a(t.fullName);
			if (--i > 0) code.addChar(','.code);
		}
	*/	
		a("\n{\n");
		
		for (p in def.propertiesSorted) if (!Util.isDefinedInSuperClassOf(def, p)) {
			
			if (immutable) {
				genGetter(p, true);
			} else {
				genGetter(p, false);
			//	genSetter(p, def.fullName);
			}
		}
		
		if (immutable) {
			// Write immutable interface to disk
			a("}\n");
			write("I" + def.name + "VO");
		}
	}
	
	public function genClass(def:ClassDef) : Void
	{
		if (def.isMixin) return;
		
		// Interfaces
		genClassProperties(def, true);
		
		// Class properties
		genClassProperties(def, false);
		
		var magic = getMagicGenerator(def);
		magic.inject(code);
		
		// Meta data
		genClassMetaData(def);
		
		// Constructor
		genClassConstructor(def, def.superClass != null, magic);
		
		// Dispose
		genDispose(def);
		
		// Validation
		genValidation(def.superClass != null);
		
		// Serialization
		genSerialization(def);
		
		// XMLMapping
		genXMLMapFunctions(def, magic);
		
		genEditFunctions(def);
		
		// Close class }
		code.add("}\n");
		
		write(def.name + "VO");
	}
	
	
	
	static var dummyMagic = new MagicClassGenerator();
	
	private function getMagicGenerator(def:ClassDef) : MagicClassGenerator
	{
		if (Std.is(def, NamedSetDef)) return new NamedSetDefGenerator(def);
		
		return dummyMagic;
	}
	
	function a(str:String) code.add(str)
	
	
	private function genClassMetaData(def:ClassDef)
	{
		a("\n\tstatic inline public var TypeID = ");
		a(def.index + ";\n\t");
		for (p in def.propertiesSorted) {
		 	a("\n\tstatic inline public var P_"); a(p.name); a(" = "); a("0x" + StringTools.hex(p.definedIn.index << 8 | p.index, 4)); a("; // "); a(p.definedIn.name);
		}
		a("\n\t");
	}
	
	private function genMsgpackMaxSetPropertyCheck(def:ClassDef)
	{
		if (def.numPropertiesDefined == 0) return; // no props
		
		a("\n\tstatic public function msgpack_maxNonEmptyPropertyID(obj : "); a(def.name); a("VO) : Int\n\t{");
		a("\n		return ");
		var p, i = def.propertiesSorted.length, noIf = true;
		while (i-->0)
		{
			p = def.propertiesSorted[i];
			if (p.definedIn != def) continue;

			noIf = addIfPropertyIsSetExpr("obj.", p, Std.string(p.index));
			if (noIf) i = 0;
			break;
		}
		while (i-->0) {
			p = def.propertiesSorted[i];
			if (p.definedIn != def) continue;

			a("\n\t\t  else ");
			noIf = addIfPropertyIsSetExpr("obj.", p, Std.string(p.index));
			if (noIf) i = 0;
		}
		if (!noIf) {
			a("\n\t\t  else -1;");
		}
		a("\n\t}");
	}
	
	private function genSerialization(def:ClassDef)
	{
		// Count bits/properties for this 'def
		var lastProp = null;
		var thisProps = new IntHash<Property>();
		var totalPropsToPack = def.numPropertiesDefined;
		for (p in def.propertiesDefined) lastProp = p;
		
		var totalProps = totalPropsToPack;
		
		
		genMsgpackMaxSetPropertyCheck(def);
		var hasSuper = def.superClass != null;
		
		a("\n\t"); if (hasSuper) a("override "); a("public function messagePack(o : BytesOutput) : Int\n\t{");
		a("\n\t\treturn msgpack_packVO(o, this, true);");
		a("\n\t}");
		a("\n");
		
		a("\n\tstatic public function msgpack_packVO(o : BytesOutput, obj : "); a(def.name); a("VO, prependMsgpackType : Bool = false) : Int\n\t{");
		a("\n		Assert.that(o != null && obj != null);");
		a("\n		var ");
		if (lastProp != null)
			a("maxID = msgpack_maxNonEmptyPropertyID(obj)");
		
		var processEmbeddedTypes = (def.superClass == null && def.supertypes.length > 0) || (hasSuper && def.supertypes.length > 1);
		
		if (processEmbeddedTypes) {
			if (lastProp != null) a(", ");
			a("i = obj.msgpack_interfacesToPack()");
		}
		
		if (hasSuper) {
			if (lastProp != null || processEmbeddedTypes) a(",\n\t\t\t");
			a("superPacker = obj.msgpack_superClassPacker();");
		}
		else
			a(";");
		a("\n\t\t");
		a("\n\t\tvar b /* bytes written */ : Int;");
		a("\n\t\tif (prependMsgpackType) {");
		
		a("\n\t\t\tif (");
		 	if (hasSuper) {
				a("!superPacker.notNull()");
				if (lastProp != null || processEmbeddedTypes) a(" && ");
			}
		 	if (processEmbeddedTypes) a(lastProp != null? "i + " : "i.not0()");
		 	if (lastProp != null) a("maxID == -1");
		a(") return o.packNil();");
		
		a("\n\t\t\telse b = o.packValueObject();");
		a("\n\t\t}");
		a("\n\t\telse b = 0;");
		
		a("\n\t\t");
		a("\n\t\tb += o.packValueObjectHeader(TypeID, ");
		
		if (processEmbeddedTypes) a(def.supertypes.length == (def.superClass == null? 1 : 2)? "i" : "(i & 0xFF)"); 
		else if (def.superClass == null) a("0");
		
		if (hasSuper) {
			if (def.supertypes.length > 1) a(" + ");
			a("(superPacker != null).boolCalc()");
		}
		if (lastProp != null)
			a(", maxID == -1? 0 : 1 + (maxID >> 3));");
		else
			a(", 0);");
		
		a("\n\t\t");
		
		if (hasSuper)
			a("\n\t\tif (superPacker.notnull()) superPacker(o, this);");
		
		
		if (processEmbeddedTypes)
		{
			a("\n\t\tif (i.not0())");
			a("\n\t\t{");
		
			// Pack mixins/embedded/interface types
			var i = 0;
			if (hasSuper) {
				for (t in def.supertypes) if (!t.implementedBy.exists(def.superClass.fullName))
					addEmbedMessagePackCall(i++, t, def.supertypes.length - 1);
			}
			else {
				for (t in def.supertypes)
					addEmbedMessagePackCall(i++, t, def.supertypes.length);
			}
		
			if (totalProps > 1) a("\n\t\t\ti = 0;");
			a("\n\t\t}");
		}
		
		if (totalPropsToPack == 1 && lastProp.index < 8)
		{
			// Single Property optimization
			var p = lastProp;
			
			a("\n\t\tif (maxID == P_"); a(p.name); a(") {");
			a("\n\t\t\to.writeByte(0x"); a(StringTools.hex(1 << p.index, 2)); a("); ++b;");
			a("\n\t\t\t"); addPropertyPackerCall("obj." + p.name, p.type, p.isBindable()); ac(";".code);
			a("\n\t\t}");
			a("\n\t\telse Assert.that(maxID == -1);");
		}
		else if (lastProp != null) // at least one Property to pack
		{
			a("\n\t\tif (maxID == -1) return b;");
			a("\n\t\t");
			
			if (!processEmbeddedTypes && totalPropsToPack > 1) a("\n\t\tvar i = 0;");
			
			var bit = 0;
			for (i in 0 ... lastProp.index + 1)
			{
				var p = thisProps.get(i);
				++bit;
				
				if (p == null)
				{
					if (thisProps.exists(i+1))
					{
						if (bit >= 8) while (bit >= 8)
						{
							     if (bit >= 32) { a("\n\t\to.writeInt31(0); b += 4;");	bit -= 32; }
							else if (bit >= 24) { a("\n\t\to.writeInt24(0); b += 3;");	bit -= 24; }
							else if (bit >= 16) { a("\n\t\to.writeInt16(0); b += 2;");	bit -= 16; }
							else if (bit >=  8) { a("\n\t\to.writeByte(0); ++b;");		bit -=  8; }
						}
					}
				}
				else
				{
					if (totalPropsToPack == 1 && lastProp.index % 8 == 0)
					{
						--totalPropsToPack;
					
						if (bit <= 8 && p == lastProp)
						{
							a("\n\t\t");
							// Single last Property: optimize if + packing
							a("\n\t\tAssert.that(P_"); a(lastProp.name); a(" == maxID);");
							a("\n\t\to.writeByte(" + (1 << (bit-1))); a("); ++b;\n\t\t");
							addPropertyPackerCall("obj." + p.name, p.type, p.isBindable()); ac(";".code);
							
							break;
						}
						else throw "huh?";
					}
					else
					{
						--totalPropsToPack;
						a("\n\t\t"); addIfPropertyIsSetExpr("obj.", p, "i |= 0x" + StringTools.hex(1 << (bit-1), 2));
					
						//TODO: Add basic circular reference check ?  Maybe cache all object references and throw on circle?
					
						if (bit == 8 || p == lastProp)
						{
							a("\n\t\to.writeByte(i); ++b;");
							a("\n\t\t");
							if (totalProps <= 8)	a("\n\t\tAssert.that(i.not0());");
							else					a("\n\t\tif (i.not0())");
							a("\n\t\t{");
						
							for (b in 0 ... bit) {
								var p = thisProps.get(i+1 - bit + b);
								if (p == null) continue;
								a("\n\t\t	if ((i & 0x"); a(StringTools.hex(1 << b, 2)); a(").not0()) "); 
								addPropertyPackerCall("obj." + p.name, p.type, p.isBindable()); ac(";".code);
							}
						
							a("\n\t\t}");
							if (totalPropsToPack > 0) {
								a("\n\t\tif (maxID <= P_"); a(p.name);  a(") return b;");
								a("\n\t\t");
							}
							
							bit = 0;
						}
	//					else throw "wtf?";
					}
				}
			}
		}
		a("\n\t\t");
		a("\n\t\treturn b;");
		a("\n\t}\n");
		
		if (processEmbeddedTypes)
		{
			a("\n\t");
			if (hasSuper) a("override "); a("private function msgpack_interfacesToPack() : Int\n\t{");
			
			if (def.superClass == null? def.supertypes.length == 1 : def.supertypes.length == 2) {
				var t = def.supertypes.first();
				a("\n\t\treturn ("); a(t.fullName); a(".msgpack_maxNonEmptyPropertyID(this) != -1).boolCalc();");
			}
			else {
				a("\n\t\tvar i = 0, n = 0;");
				var i = 0;
				for (t in def.supertypes) {
					a("\n\t\tif ("); a(t.fullName); a(".msgpack_maxNonEmptyPropertyID(this) != -1) { ++n; i |= 0x"); addEmbedMessagePackTypeBitmask(i++); a("; }");
				}
				a("\n\t\treturn i |= n;");
			}
			a("\n\t}\n");
		}
		
		if (hasSuper)
		{
			a("\n\t");
			if (def.superClass.superClass != null) a("override ");
			a("private function msgpack_superClassPacker() : Output -> IValueObject -> Int\n\t{\n\t\t\t");
			a("var sc = "); a(def.superClass.fullName); a(";\n\t\t\t");
			a("return if (sc.msgpack_maxNonEmptyPropertyID(this) != -1 || super.msgpack_interfacesToPack().not0()) cast sc.msgpack_packVO;\n\t\t\t");
			a("     else "); if (def.superClass.superClass != null) a("super.superClassPacker();"); else a("null;");
			a("\n\t}\n");
		}
	}
	
	private inline function ac(char:Int) code.addChar(char)
	
	private function addPropertyPackerCall(path:String, pType:PType, bindable:Bool) switch (pType)
	{
		case Tbool(val):				a('b += o.packBool(');			a(path); ac(")".code);
		case Tinteger(min,max,stride):	a('b += o.packInt(');			a(path); ac(")".code);
		case Tdecimal(min,max,stride):	a('b += o.packDouble(');		a(path); ac(")".code);
		case Tstring:					a('b += o.packString(');		a(path); ac(")".code);
		case Tdate:						a('b += o.packDate(');			a(path); ac(")".code);
		case Tdatetime:					a('b += o.packDateTime(');		a(path); ac(")".code);
		case Tinterval:					a('b += o.packDateInterval(');	a(path); ac(")".code);
		case Turi:						a('b += o.packURI(');			a(path); ac(")".code);
		case TuniqueID:					a('b += o.packUniqueID(');		a(path); ac(")".code);
		case Temail:					a('b += o.packEMail(');			a(path); ac(")".code);
		case Tcolor:					a('b += o.packRGBA(');			a(path); ac(")".code);
		case TfileRef:					a('b += o.packFileRef(');		a(path); ac(")".code);
		
		case Tdef(ptypedef): switch (ptypedef) {
			case Tclass(def):	a('b += ('); a(path); a(".notNull()? "); a(path); a(".messagePack(o) : o.packNil())");
			case Tenum(def):	a('b += o.packInt('); a(def.fullName); a("_utils.toValue("); a(path); a(")");
		}
		case Tarray(type, min, max):
			a("{");
			a("\n\t\t\t\tvar a = "); a(path); ac(";".code);
			a("\n\t\t\t\tb += o.packArrayHeader(a.length);");
			a("\n\t\t\t\tfor (i in 0 ... a.length) "); addPropertyPackerCall("a.get(i)", type, false); ac(";".code);
			a("\n\t\t\t}");
			

		case TenumConverter(_), Tbinding(_), TlinkedList:	throw "Not implemented";
	
	}
	
	/** Returns true when no if statement was added  (Property is always set) */
	private function addIfPropertyIsSetExpr(propPrefix:String, p:Property, expr:String)
	{
		var path, nullCheck;
		
		if (p.isBindable())
		{
			switch (p.type) {
				case Tarray(_,_,_), Turi:
					path = propPrefix + p.name;
				default:
					path = propPrefix + p.name + ".value";
			}
			nullCheck = propPrefix + p.name + ".notNull() && " + path + ".notNull()";
		}
		else {
			path = propPrefix + p.name;
			nullCheck = path + ".notNull()";
		}
		
		var extraChecks = null;
		
		switch (p.type)
		{
			case Tstring, Temail:
				extraChecks = path + ".length.not0()";
	
			case Tarray(_,_,_):
				extraChecks = path + ".length.not0()";
	
			case Turi, TfileRef, Tinteger(_,_,_), Tdecimal(_,_,_):
				extraChecks = path + ".isSet()";
			
			case Tbool(_):
				nullCheck = null; // no way to figure out if the value was changed...
			
			case Tdef(type):
				extraChecks = "!" + path + ".isEmpty()";
			
			case TuniqueID, Tinterval, TlinkedList, TenumConverter(_), Tdate, Tdatetime, Tcolor, Tbinding(_):
		}
		
		if (nullCheck != null) {
			a("if ("); a(nullCheck);
			if (extraChecks != null) { a(" && "); a(extraChecks); }
			a(") ");
		}
		
	 	a(expr);
		a(";"); // a(p.name);
		
		return nullCheck == null;
	}
	
	private function addEmbedMessagePackTypeBitmask(i:Int) {
		a(StringTools.hex(1 << (i + 8), 4));
	}
	
	private function addEmbedMessagePackCall(i:Int, t:TypeDefinition, numEmbedTypes:Int)
	{
		a("\n\t\t\t");
		if (numEmbedTypes > 1) {
			a("if ((i & 0x"); addEmbedMessagePackTypeBitmask(i); a(").not0()) ");
		}
		a("b += "); a(t.fullName); a("VO.msgpack_packVO(o, this);");
	}
	
	private function genValidation(genOverride:Bool = false)
	{
		return;
		
		a("\n#if (VO_Write || !VO_Read)");
		a("\n\t"); if (genOverride) a("override "); a("public function isValid():Bool\n\t{\n ");
		a("\t\treturn true;\n");
		a("\t}\n#end\n");
	}
	
	
	private function genEditFunctions(def:ClassDef)
	{
		genEditableVOFunctionCalls(def, "beginEdit");
		genEditableVOFunctionCalls(def, "commitEdit");
		genEditableVOFunctionCalls(def, "cancelEdit");
	}
	
	
	private function openFunctionDeclaration (def:ClassDef, functionName)
	{
	/*	if (def.superClass != null)
		{
			// Check if this function should be overridden
			var doOverride = false;
			for (p in def.property) if (p.isBindable() && !Util.isDefinedInSuperClassOf(def, p)) {
				doOverride = true;
				break;
			}

			if (!doOverride) return; // no need to override
		}
*/		
		a("\n\t");
		if (def.superClass != null) a("override ");

		a("public function "); a(functionName); a("() : Void\n\t{\n");

		if (def.superClass != null) {
			a("\t\tsuper."); a(functionName); a("();\n");
		}
	}
	
	
	private function closeFunctionDeclaration (def:ClassDef, functionName)
	{
		code.add("\t}\n");
	}
	
	
	private function generateFunctionCall (p:Property, functionName:String)
	{
		code.add("\t\tthis.");
		code.add(p.name);
		code.add(".");
		code.add(functionName);
		code.add("();\n");
	}
	
	
	private function genDispose (def:ClassDef)
	{	
		openFunctionDeclaration( def, "dispose");
		
		for (p in def.property)
			if (!Util.isDefinedInSuperClassOf(def, p) && p.isDisposable())
				generateFunctionCall( p, "dispose" );
		
		closeFunctionDeclaration( def, "dispose");
	}
	
	
	private function genEditableVOFunctionCalls(def:ClassDef, functionName)
	{
		openFunctionDeclaration( def, functionName);
		
		for (p in def.property)
			if (!Util.isDefinedInSuperClassOf(def, p) && (p.isBindable() || p.isArray()))
				generateFunctionCall( p, functionName );
		
		closeFunctionDeclaration( def, functionName);
	}
	
	private function genClassConstructor(def:ClassDef, genSuperCall:Bool = false, magic)
	{
		a("\n\tpublic function new()\n\t{\n");
		if (genSuperCall)
			a("\t\tsuper();\n");
		
		var a = code.add;
		var setProp = function(p){
			a("\t\tthis."); a(p.name); a(" = ");
		}
		
		for (p in def.property) if (!Util.isDefinedInSuperClassOf(def, p)) {
			var c = HaxeUtil.getConstructorCall(p.type, p.isBindable());
			if (c != null) {
				setProp(p);
				a(c); a("\n");
			}
		}
		
		magic.constructor(code);
		a("\t}\n");
	}
	
	function write(name:String)
	{
		var filename = dir +"/"+ name + ".hx";
		
		var file = neko.io.File.write(filename, false);
		file.writeString(code.toString());
		file.close();
	}
	
	function genGetter(p:Property, immutable:Bool)
	{
		a("\tpublic var "); a(p.name); 
		if (immutable) {
			a("\t(default,null");
		} else {
			a("\t(default,default");
//			a("(default,set"); code.addCapitalized(p.name);
		}
		a(") : "); a(HaxeUtil.haxeType(p.type, true, p.isBindable())); a(";\n");
	}
/*	
	function genSetter(p:Property, fullName:String)
	{
		var a = code.add;
		
		a("\t#if (VO_Write || !VO_Read)\n");
		a("\tinline private function set"); code.addCapitalized(p.name); a("(v:"); a(HaxeUtil.haxeType(p.type)); a(") {\n");
		a("\t\tthis."); a(p.name); a(" = v;\n");
		a("\t\treturn this;\n");
		a("\t}\n");
		a("\t#end\n\n");
	}
*/	
	
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
		
		for (conv in def.conversions)
		{
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
		code.add("#if (VO_Write || !VO_Read)\n");
	/*	To XML generates incorrect code.
		toXML() should be split into 2 functions:
		- set values to a given node
		- create a childnode in the given node (and call the set function?)
	*/
	//	TODO: Fix toXML generation
	//	mapToHaxe.genToXML();
		code.add("\n#end\n#if (VO_Read || !VO_Write)\n");
		mapToHaxe.genFromXML();
		this.code.add("\n#end\n");
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
	public function new();
	
	public function fromXML(code:StringBuf);
	public function inject(code:StringBuf);
	public function constructor(code:StringBuf);
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
		var c = HaxeUtil.getConstructorCall(ns.baseType, false);
		if (c != null) { a(" = "); a(c); }
		else throw "non constructable type? " + ns;
		
		a("\n\t\t\ttmp.setFromXML(child);");
		a("\n\t\t\tthis.add(tmp);");
		a("\n\t\t}\n"); // end for
	}
}

private class HaxeUtil
{
	public static function getConstructorCall(ptype:PType, bindable:Bool)
	{
		var code = switch (ptype)
		{
			case Tstring:				"''";
			case Tbool(val):			val? "true" : "false";
			case Tarray(type, _,_):		("new primevc.core.collections.RevertableArrayList < " + HaxeUtil.haxeType( type, true ) + " >()");
			
			case Turi, TfileRef, Tinterval:
				"new " + HaxeUtil.haxeType(ptype) + "()";
			
			case Tdef(type):
				getClassConstructorCall(Util.unpackPTypedef(type));
			
			case Tinteger(_,_,_):		'primevc.types.Number.INT_NOT_SET';
			case Tdecimal(_,_,_):		'primevc.types.Number.FLOAT_NOT_SET';
			case TuniqueID, Temail:		"''";
			
			case TlinkedList, TenumConverter(_), Tdate, Tdatetime, Tcolor, Tbinding(_):
				null;
		}
		
		return bindable? "new primevc.core.RevertableBindable("+ code +");" : if (code == null) null else code + ";";
	}
	
	public static function getClassConstructorCall(tdef:TypeDefinition)
	{
		// Dont 'new' enums.
		return if (!Std.is(tdef, EnumDef))
			"new " + tdef.fullName + "VO()";
		  else null;
	}
	
	public static function haxeType(ptype:PType, ?immutableInterface:Bool = false, ?bindable:Bool = false)
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
			case Turi:						bindable? 'primevc.types.BindableURI' : 'primevc.types.URI';
			case TuniqueID:					'primevc.types.UniqueID';
			case Temail:					'primevc.types.EMail';
			case Tcolor:					'primevc.types.RGBA';
			case TfileRef:					'primevc.types.FileRef';
			
			case Tdef(ptypedef):			switch (ptypedef) {
				case Tclass		(def): (immutableInterface? def.module.fullName + ".I" + def.name : def.fullName) + "VO";
				case Tenum		(def): def.fullName;
			}
			case Tbinding(p):				haxeType(p.type);
			case TenumConverter(enums):		'String';
			
			case TlinkedList:				'';
			case Tarray(type, min, max):	'primevc.core.collections.RevertableArrayList<'+ haxeType(type, true) +'>';
		}
		
		if (bindable)
		{
			// Check for Bindable
			type = "primevc.core.RevertableBindable<"+type+">";
		}
		
		return type;
	}
	
	public static function compareValueCode(path:String, type:PType, val:Dynamic) : String
	{
		return switch (type)
		{
			case Tbool(_), Tinteger(_,_,_), Tdecimal(_,_,_):
				path +" == "+ Std.string(val);
			
			case Tstring:
				path +" == "+ '"'+ val +'"';
			
			case Turi, TfileRef:
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
			
			case TlinkedList, Tdef(_), Tbinding(_), Tinterval, Tarray(_,_,_):
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
			case Tbinding(p):				getToIntConversion(path, p.type);
			case Tdate:						path+".getTime()";
			case Tdatetime:					path+".getTime()";
			
			case Turi:						throw "impossible Turi";
			case TfileRef:					throw "impossible TfileRef";
			case Tinterval:					throw "impossible Tinterval";
			case TuniqueID:					throw "impossible Tuniq";
			case Temail:					throw "impossible Temail";
			case Tdef(ptypedef):			throw "impossible int conversion Tdef: "+ptypedef;
			
			case TenumConverter(enums):		throw "not implemented";
			case TlinkedList:				throw "not implemented";
			case Tarray(type, min, max):	throw "not implemented";
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
			case Tbinding(p):				getFromIntConversion(path, p.type);
			
			case Tinterval:					throw "impossible";
			case Turi:						throw "impossible";
			case TfileRef:					throw "impossible";
			case TuniqueID:					throw "impossible";
			case Temail:					throw "impossible";
			case Tdef(ptypedef):			throw "impossible";
			
			case TenumConverter(enums):		throw "not implemented";
			case TlinkedList:				throw "not implemented";
			case Tarray(type, min, max):	throw "not implemented";
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
			case TuniqueID:					xmlstring+prefix+'UniqueID';
			case Temail:					xmlstring+prefix+'EMail';
			case Tcolor:					xmlstring+prefix+'Color';
			case Tdate:						xmlstring+prefix+'Date';
			case Tdatetime:					xmlstring+prefix+'Date';
			
			case Tbinding(p):
				var f = getXMLConversionFunction(p.type, path, prefix, property);
				path = f.path;
				setProp = f.setProp;
				/*'return'*/ f.fun;
				
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
			case TlinkedList:				throw "linkedlist not implemented";
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
