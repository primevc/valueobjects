package primevc.tools.valueobjects.output;
 import primevc.tools.valueobjects.VODefinition;
  using primevc.tools.valueobjects.output.Util;

class HaxeTypeMap implements CodeGenerator
{
	var map : IntHash<String>;
	
	public function new() {
		map = new IntHash();
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
	
	public function genEnum(def:EnumDef);
	
	public function newModule(module:Module) {
		module.generateWith(this);
		return cast this;
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
			a("\ninterface I");
		else {
			a(" import primevc.tools.valueobjects.ValueObjectBase;\n");
			a(" import primevc.tools.valueobjects.xmlmap.XMLString;\n");
			a(" import primevc.utils.msgpack.Reader;\n");
			a("  using primevc.utils.IfUtil;\n");
			a("  using primevc.utils.TypeUtil;\n");
			a("  using primevc.utils.NumberUtil;\n");
			a("  using primevc.utils.msgpack.Format;\n");
			a("\nclass ");
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
			if (!def.isMixin && def.superClass == null) a(", implements primevc.core.traits.IClonable < I" + def.name + "VO >");
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
			
			if (immutable) {
				genGetter(p, true);
			} else {
				genGetter(p, false);
				genSetter(i, p, def.fullName);
			}
		}
		
		if (immutable) {
			// Write immutable interface to disk
			a("}\n");
			write("I" + def.name + (def.isMixin? "" : "VO"));
		}
	}
	
	public function genClass(def:ClassDef) : Void
	{
		trace(def.fullName);
		
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
			genXMLMapFunctions(def, magic);
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
		
		if (!def.isMixin)
			genCloneFunction(def);
		
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
	
	function a(str:String) code.add(str)
	
	
	private function genClassMetaData(def:ClassDef)
	{
		a("\n\tstatic inline public var TYPE_ID = ");
		a(def.index + ";\n\t");
		for (p in def.propertiesSorted) {
		 	a("\n\tstatic inline public var "); a(p.name.toUpperCase()); a(" = "); a("0x" + StringTools.hex(p.definedIn.index << 8 | p.index, 4)); a("; // "); a(p.definedIn.name);
		}
		a("\n\t");
	}
	
/*	private function genMsgpackMaxSetPropertyCheck(def:ClassDef)
	{
		if (def.numPropertiesDefined == 0) return; // no props
		
		a("\n\tstatic "); if (def.numPropertiesDefined < 4) a("inline "); a("public function msgpack_maxNonEmptyPropertyID(obj : "); a(def.name); a("VO) : Int\n\t{");
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
*/	
	private function addIfMixinBitsSet(t:BaseTypeDefinition, offset:Int, expr:String)
	{
		a("\n			if ((propertyBits & ");
		if (t.propertiesSorted.length == 1)
			a("1");
		else {
			a(interfacePropertiesBitmask(t, offset));
		}
		a(" /* "); a(t.name); a(" */).not0()) "); a(expr);
	}
	
	private function addFullName(t:TypeDefinition, interfaceT = false)
	{
		if (interfaceT) {
			a(t.module.fullName); a(".I"); a(t.name);
		}
		else a(t.fullName);
		
		if (Std.is(t,ClassDef) && !cast(t, ClassDef).isMixin) a("VO");
	}

/*	
	private function genFieldSetter(def:ClassDef)
	{
		a("\n\tstatic public function setByIndex(obj : "); if (def.isMixin){ a("I"); a(def.name); } else { a("I"); a(def.name); a("VO"); } a(", propertyIndex:Int, value:Dynamic) : Void\n\t{");
		a("\n		Assert.that(obj != null);");
		a("\n		switch(propertyIndex) {");
		for (p in def.propertiesDefined) {
			a("\n			case "); a(p.index + ":"); a("obj."); a(p.name); a(" = "); a(HaxeUtil.getDynamicConversionCall(p.type, p.isBindable())); a("value);");
		}
		a("\n		}");
		a("\n\t}\n");
	}
*/	
	private function genDeSerialization(def:ClassDef, lastProp:Property)
	{
		if (lastProp == null) return;
		
		a("\n\tstatic public function msgpack_unpackVO(reader : Reader, obj : "); if (def.isMixin){ a("I"); a(def.name); } else { a("I"); a(def.name); a("VO"); } a(", propertyBytes : Int, converter : ValueConverter) : Void\n\t{");
		a("\n		Assert.that(reader != null && obj != null);");
		a("\n		var input = reader.input, bits:Int;");
		
		var bit = 8;
		
		for (i in 0 ... lastProp.index + 1)
		{
			if (bit == 8) {
				a("\n\t\n\t\tif (propertyBytes-- == 0) return;");
				a("\n\t\n\t\tbits = input.readByte();");
				bit = 0;
			}
			
			var p = def.propertiesDefined.get(i);
			if (p == null) {
				++bit;
				continue;
			}
			
			a("\n\t\tif ((bits & 0x"); a(StringTools.hex(1 << bit, 2)); a(").not0()) ");
			
			if (p.isArray())
			{
				a("(untyped obj).set"); code.addCapitalized(p.name); a("(");
				a( Util.isSingleValue(p.type)
				 	? p.isBindable()? "new primevc.core.collections.RevertableArrayList(" : "new primevc.core.collections.ArrayList("
					: 'new ' + HaxeUtil.haxeType(p.type, true) + '('
				);
			}
			else {
				a("((untyped obj).");
				a(p.name);
				if (p.isBindable() && Util.isSingleValue(p.type)) a(".value");
				a(" = ");
			}
			
			a("reader.");
			switch (p.type)
			{
				case Tarray(innerT,_,_):
					a("readMsgPackArray(");
					a(p.name.toUpperCase()); a(", "); a(HaxeUtil.haxeType(innerT)); a(")));");
				
				default:
					a("readMsgPackValue("); a(p.name.toUpperCase()); a(", "); a(HaxeUtil.haxeType(p.type)); a("));");
			}
			
			++bit;
		}
		
		a("\n\t\t\n\t\tif (propertyBytes.not0()) reader.discardRemainingVOProperties(propertyBytes);");
		
		a("\n\t}");
	}
	
	private function genSerialization(def:ClassDef)
	{	
		// Count bits/properties for this 'def
		var lastProp:Property = null;
		var totalPropsToPack = def.numPropertiesDefined;
		for (p in def.propertiesDefined) if (lastProp == null || lastProp.index < p.index) lastProp = p;
		
		genDeSerialization(def, lastProp);
		
		var totalProps = totalPropsToPack;
		
		var hasSuper = def.superClass != null;
		
		if (!def.isMixin)
		{
			a("\n\t"); if (hasSuper) a("override "); a("public function messagePack(o : haxe.io.BytesOutput) : Int\n\t{");
			a("\n\t\treturn msgpack_packVO(o, this, _propertiesSet, true);");
			a("\n\t}");
			a("\n");
		}
		
		a("\n\tstatic public function msgpack_packVO(o : haxe.io.BytesOutput, obj : "); if (def.isMixin){ a("I"); a(def.name); } else { a("I"); a(def.name); a("VO"); } a(", propertyBits : Int, prependMsgpackType : Bool = false) : Int\n\t{");
		
		a("\n		Assert.that(o != null && obj != null);");
		a("\n		");
		
		a("\n\t\tvar b /* bytes written */ : Int;");
		a("\n\t\tif (prependMsgpackType) {");
		
		a("\n\t\t\tif (propertyBits.not0()) b = o.packValueObject();");
		a("\n\t\t\telse return o.packNil();");
		a("\n\t\t}");
		
		a("\n\t\telse b = 0;");
		a("\n\t\t");
		
		
		var mixinBits = 0;
		var hasMixins = def.supertypes.length > 0;
		
		if (hasMixins)
		{	
			for (t in def.supertypes) mixinBits += t.propertiesSorted.length;
			
			if (mixinBits > 1)
			{
				a("\n\t\tvar mixin = 0;");
				a("\n\t\tvar mixBits = propertyBits & "); a(bitmask(mixinBits)); a(";");
				var offset = 0;
				
				if (def.supertypes.length == 1) {
					a("\n\t\tif (mixBits.not0()) ++mixin; // "); a(def.supertypes.first().fullName);
				}
				else for (t in def.supertypes) {
					addIfMixinBitsSet(t, offset, "++mixin;");
					offset += t.propertiesSorted.length;
				}
				a("\n");
			}
			else {
				// single mixin with 1 field optimization (like ObjectId)
				a("\n\t\tvar mixin = propertyBits & 1; // Single field mixin: "); a(def.supertypes.first().fullName);
			}
			
			a("\n\t\tpropertyBits >>>= "); a(mixinBits + ";");
		}
		
		a("\n\t\tb += o.packValueObjectHeader(TYPE_ID, ");
		
		if (hasMixins) a("mixin");
		else if (!hasSuper) a("0");
		
		if (lastProp != null)
		{
			if (def.numPropertiesDefined == 1)
				a(", propertyBits);");
			else {
				a(", propertyBits.not0()? "); a(def.numPropertiesDefined > 8? "ValueObjectBase.bytesUsedInInt(propertyBits)" : "1"); a(" : 0);");
			}
		}
		else
			a(", 0);");
		
		a("\n\t\t");
		
		if (hasMixins)
		{
			if (lastProp != null)	a("\n\t\tif (mixin.not0())");
			else					a("\n\t\tAssert.that(mixin.not0());");
			
			a("\n\t\t{");
			
			if (def.supertypes.length == 1)
			{
				var t = def.supertypes.first();
				a("\n\t\t\tb += "); addFullName(t); a(".msgpack_packVO(o, obj, ");
				a(mixinBits == 1? "1" : "mixBits");
				a(", false);");
			}
			else for (t in def.supertypes)
			{	
				a("\n			mixin = mixBits & ");
				if (t.propertiesSorted.length == 1)
					a("1");
				else {
					a(interfacePropertiesBitmask(t, 0));
				}
				a(" /* "); a(t.name); a(" */;");
				
				a("\n			if (mixin.not0()) "); a("b += "); a(t.fullName); if (Std.is(t,ClassDef) && !cast(t, ClassDef).isMixin) a("VO"); a(".msgpack_packVO(o, obj, mixin, false);");
				
				if (t != def.supertypes.last()) {
					a("\n\t\t\tmixBits >>>= "); a(t.propertiesSorted.length+";");
					a("\n\t\t\t");
				}
			}
			
			a("\n\t\t}");
			a("\n\t\t");
		}
		
		if (totalPropsToPack == 1 && lastProp.index < 8)
		{
			// Single Property optimization
			var p = lastProp;
			
			a("\n\t\tif (propertyBits.not0()) {");
			a("\n\t\t\to.writeByte(0x"); a(StringTools.hex(1 << p.index, 2)); a("); ++b;");
			a("\n\t\t\t"); addPropertyPackerCall("obj." + p.name, p.type, p.isBindable()); ac(";".code);
			a("\n\t\t}");
		}
		else if (lastProp != null) // at least one Property to pack
		{
			a("\n\t\tif (!propertyBits.not0()) return b;");
			a("\n\t\t");
			
			var bit = 0, mask = 0;
			for (i in 0 ... lastProp.index + 1)
			{
				var p = def.propertiesDefined.get(i);
				++bit;
				
				if (p == null)
				{
					if (def.propertiesDefined.exists(i+1))
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
					--totalPropsToPack;
					
					if (totalPropsToPack == 0 && lastProp.index % 8 == 0)
					{
						if (bit <= 8 && p == lastProp)
						{
							a("\n\t\t");
							var bitMask = (1 << (bit-1));
							// Single last Property: optimize if + packing
							a("\n\t\tAssert.that(propertyBits & "); a(bitMask + " != 0);");
							a("\n\t\to.writeByte(" + bitMask); a("); ++b;\n\t\t");
							addPropertyPackerCall("obj." + p.name, p.type, p.isBindable()); ac(";".code);
							
							break;
						}
						else throw "huh?";
					}
					else
					{
					//	a("\n\t\t"); addIfPropertyIsSetExpr("obj.", p, "i |= 0x" + StringTools.hex(1 << (bit-1), 2));
						mask |= 1 << (bit-1);
						
						//TODO: Add basic circular reference check ?  Maybe cache all object references and throw on circle?
					
						if (bit == 8 || p == lastProp)
						{
							a("\n\t\t");
							
							if (def.numPropertiesDefined > 8 && def.numPropertiesDefined - i > 1) {
								a("\n\t\to.writeByte(propertyBits & 0x"); a(StringTools.hex(mask, 2));
							}
							else {
								a("\n\t\to.writeByte(propertyBits");
							}
							a("); ++b;");
							
							a("\n\t\t");
							if (totalProps <= 8)	a("\n\t\tAssert.that(propertyBits.not0());");
							else					a("\n\t\tif (propertyBits.not0())");
							a("\n\t\t{");
						
							for (b in 0 ... bit) {
								var p = def.propertiesDefined.get(i+1 - bit + b);
								if (p == null) continue;
								a("\n\t\t	if ((propertyBits & 0x"); a(StringTools.hex(1 << b, 2)); a(").not0()) ");
								addPropertyPackerCall("obj." + p.name, p.type, p.isBindable()); ac(";".code);
							}
						
							a("\n\t\t}");
							if (totalPropsToPack > 0) {
								a("\n\t\tif (!(propertyBits >>>= 8).not0()) return b;");
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
	}
	
	static function bitmask(numBits:Int, offset:Int=0)
	{
		var mask = 0;
		for (bit in 0 ... numBits) {
			mask |= 1 << (bit + offset);
		}
		return "0x" + StringTools.hex(mask, 4);
	}
	
	private static function interfacePropertiesBitmask(t:BaseTypeDefinition, offset:Int)
	{
		return bitmask(t.propertiesSorted.length, offset);
	}
	
	private inline function ac(char:Int) code.addChar(char)
	
	private function addPropertyPackerCall(path:String, pType:PType, bindable:Bool)
	{
		if (bindable) path += ".value";
		
		switch (pType)
		{
			case Tbool(val):				a('b += o.packBool(');			a(path); ac(")".code);
			case Tinteger(min,max,stride):	a('b += o.packInt(');			a(path); ac(")".code);
			case Tdecimal(min,max,stride):	a('b += o.packDouble(');		a(path); ac(")".code);
			case Tstring:					a('b += o.packString(');		a(path); ac(")".code);
			case Tdate:						a('b += o.packDate(');			a(path); ac(")".code);
			case Tdatetime:					a('b += o.packDateTime(');		a(path); ac(")".code);
			case Tinterval:					a('b += o.packDateInterval(');	a(path); ac(")".code);
			case Turi:						a('b += o.packURI(');			a(path); ac(")".code);
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
	
		}
	}
	
	/** Returns true when no if statement was added  (Property is always set) */
	private function addIfPropertyIsSetExpr(propPrefix:String, p:Property, expr:String)
	{
		return addIfVarIsSetExpr(propPrefix + p.name, p.type, p.isBindable(), expr);
	}
	
	private function addIfVarIsSetExpr(path:String, ptype:PType, bindable:Bool, expr:String)
	{	
		var nullCheck = path + ".notNull()";
		if (bindable)
			path = path + ".value";
		
		switch (ptype) {
			case Tarray(_,_,_), Turi:
			
			case Tinteger(_,_,_), Tdecimal(_,_,_), Tbool(_):
				if (!bindable) nullCheck = null; // Simple types can't be null...
			
			default:
				if (bindable) nullCheck += " && " + path + ".notNull()";
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

		case Turi, TfileRef:
			path + ".isSet";
		
		case Tinteger(_,_,_), Tdecimal(_,_,_):
			path + ".isSet()";
		
		case Tdef(_):
			if (Util.isEnum(ptype)) null
			else "!" + path + ".isEmpty()";
		
		case Tbool(_), TuniqueID, Tinterval, TenumConverter(_), Tdate, Tdatetime, Tcolor:
			null;
	}
	
	private function addIfValueNotNullExpr(valuePath:String, ptype:PType, expr:String)
	{
		var extraChecks = extraNullCheck(valuePath, ptype);
		
		if (extraChecks != null) {
			a("if ("); a(extraChecks); a(") "); a(expr); a(";");
		}
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
		
		a("\n#if (VO_Write || !VO_Read)");
		a("\n\t"); if (genOverride) a("override "); a("public function isValid():Bool\n\t{\n ");
		a("\t\treturn true;\n");
		a("\t}\n#end\n");
	}
	
	private function hexBitflag(propertyIndex:Int) {
		return "0x" + StringTools.hex(1 << propertyIndex, 2);
	}
	
	private function genEditFunctions(def:ClassDef)
	{
		genEditableVOFunctionCalls(def, "beginEdit");
		genEditableVOFunctionCalls(def, "commitBindables");
		genEditableVOFunctionCalls(def, "cancelEdit");
		
		// addChanges()
		a("\n\toverride private function addChanges(changeSet:ObjectChangeSet)\n\t{");
		a("\n\t\tif (_changedFlags.not0())\n\t\t{");
		if (def.superClass != null)
			a("\n\t\t\tsuper.addChanges(changeSet);");
		
		for (i in 0 ... def.propertiesSorted.length)
		{
			var p = def.propertiesSorted[i];
			if (Util.isDefinedInSuperClassOf(def, p)) continue;
			
			if (p.isArray() && p.isBindable())	a("\n\t\t\tchangeSet.addListChanges(");
			else if (p.isBindable())			a("\n\t\t\tchangeSet.addBindableChange(");
			else								a("\n\t\t\tchangeSet.addChange(");
			
			a(p.name.toUpperCase()); a(", _changedFlags & "); a(hexBitflag(i)); a(", "); a(p.name); a(");");
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
		
		var returnType	= def.superClass == null ? def.name : def.getRootSuperClass().name;
		returnType		= "I" + returnType + "VO";
		
		openFunctionDeclaration( def, "clone", false, returnType, false);
		a("\t\tvar inst = new "+def.name + "VO(\n");
		
		var first = true;
		for (p in def.propertiesSorted)
		{
			if (first) {
				first = false;
				a("\t\t\t");
			}
			else a(",\n\t\t\t");
			
			if (p.isArray()) {
				a("null"); // handle array cloning seperately
			} else {
				a("this."); a(p.name);
				if (p.isBindable())			a(".value");
				
				addAsClass(p.type);
				if (p.hasClonableType())	a(".clone()");
				addAsClass(p.type);
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
	
	
	private function openFunctionDeclaration (def:ClassDef, functionName, forceOverride = false, returnType:String = "Void", makeSuperCall = true, isPublic = true)
	{
		a("\n\t");
		if (forceOverride || def.superClass != null) a("override ");

		a(isPublic? "public " : "private "); a("function "); a(functionName); a("()");
		
		if (returnType != null && returnType != "")
			a(" : " + returnType);
		
		a("\n\t{\n");
		
		if ((forceOverride || def.superClass != null) && makeSuperCall) {
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
	
	
	private function genDispose(def:ClassDef)
	{	
		openFunctionDeclaration( def, "dispose", true );
		
		for (p in def.property) if (!Util.isDefinedInSuperClassOf(def, p) && p.isDisposable()) {
			a("\t\tif (this."); a(p.name); a(".notNull())\t\t{ "); a(p.name); a(".dispose(); "); a(p.name); a(" = null; }\n");
		}
		
		closeFunctionDeclaration( def, "dispose");
	}
	
	
	private function genEditableVOFunctionCalls(def:ClassDef, functionName)
	{
		var functionCalls = new List<Property>();
		
		for (p in def.property)
			if (!Util.isDefinedInSuperClassOf(def, p) && p.isBindable())
				functionCalls.add(p);
		
		var callFunctionName = functionName == "commitBindables"? "commitEdit" : functionName;
		var isCommitBindables = functionName == "commitBindables";
		
		if (functionCalls.length > 0)
		{
			openFunctionDeclaration( def, functionName, true, "Void", def.superClass != null, !isCommitBindables );
			for (p in functionCalls)
				generateFunctionCall( p, callFunctionName );
			
			closeFunctionDeclaration( def, functionName);
		}
	}
	
	private function genClassConstructor(def:ClassDef, genSuperCall:Bool = false, magic)
	{
		a("\n\tpublic function new(");
		for (i in 0 ... def.propertiesSorted.length) {
			var p = def.propertiesSorted[i];
			a("?"); a(p.name); a("_ : "); a(HaxeUtil.haxeType(p.type, null, null, true));
			var init = HaxeUtil.getConstructorInitializer(p.type, true);
			if (init != null) {
			 	a(" = "); a(init);
			}
			if (i + 1 != def.propertiesSorted.length) a(", ");
		}
		a(")\n\t{\n");
		
		if (genSuperCall)
		{
			a("\t\tsuper(");
			var first = true;
			for (i in 0 ... def.propertiesSorted.length)
			{
				var p = def.propertiesSorted[i];
				if (Util.isDefinedInSuperClassOf(def, p)) {
					if (first) first = false; else a(", ");
					a(p.name); a("_");
				}
			}
			a(");\n");
		}
		else
			a("\t\tsuper();\n");
		
		for (p in def.propertiesSorted) if (!Util.isDefinedInSuperClassOf(def, p))
		{
			if (!Util.isPTypeBuiltin(p.type) && !Util.isEnum(p.type)) {
				if (p.isBindable())		{ a("\t\tthis."); a(p.name); a(".value"); a(" = "); a(p.name); a("_;"); }
				else					{ a("\t\tthis."); a(p.name); a(" = "); a(p.name); a("_;"); }
			}
			else switch (p.type) {
				case Tdecimal(_,_,_):
					a("\t\tif ("); a(p.name); a("_.notNull()) this."); a(p.name); a(" = "); a(p.name); a("_;");
				
				default:
					var c = HaxeUtil.getConstructorCall(p.type, p.isBindable(), p.name + "_");
					if (c != null) {
						a("\t\tthis."); a(p.name); a(" = ");
						a(c);
					}
			}
		 	a("\n");
		}
		
		magic.constructor(code);
		a("\t\t_changedFlags = 0;\n\t}\n");
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
		
		if (Util.isPTypeBuiltin(p.type) || Util.isEnum(p.type))
			a("\t(default");
		else {
			a("\t(get"); code.addCapitalized(p.name);
		}
		
		if (immutable) {
			a(",null");
		} else {
			a(",set"); code.addCapitalized(p.name);
		}
		a(") : "); a(HaxeUtil.haxeType(p.type, true, p.isBindable())); a(";\n");
		
		if (!immutable && !Util.isPTypeBuiltin(p.type) && !Util.isEnum(p.type)) {
			a("\tprivate function get"); code.addCapitalized(p.name); a("() { return this."); a(p.name); a(".notNull()? this."); a(p.name); a(" : this."); a(p.name); a(" = ");
			a(HaxeUtil.getConstructorCall(p.type, p.isBindable(), HaxeUtil.getConstructorInitializer(p.type)));
			a(" }\n");
		}
	}
	
	function genSetter(i:Int, p:Property, fullName:String)
	{
		var typeName = null;
		var listChangeHandler = false;
		if (p.isBindable() || p.isArray())
		{
			typeName = switch (p.type) {
				case Tarray(innerType, _, _):
					listChangeHandler = true;
					"primevc.core.collections.ListChange<" + HaxeUtil.haxeType(innerType, true, false) + ">";
				
				default:
					HaxeUtil.haxeType(p.type, false, false);
			}
		}
		
		
		a("\tprivate function set"); code.addCapitalized(p.name); a("(v:"); a(HaxeUtil.haxeType(p.type, true, p.isBindable())); a(")\n\t{\n");
		
		a("\t\treturn if (v == "); if (Util.isEnum(p.type) || Util.isPTypeBuiltin(p.type)) a("this."); else a("(untyped this)."); a(p.name); a(") v;\n");
		a("\t\telse\n\t\t{\n\t\t\t_changedFlags |= "); a(hexBitflag(i)); a(";\n");
		
		if (listChangeHandler || p.isBindable() || !Util.isSingleValue(p.type))
		{
			a("\t\t\tif (this."); a(p.name); a(".notNull()) this."); a(p.name); if(!p.isArray()) a(".as(ValueObjectBase)"); a(".change.unbind(this);\n");
			a("\t\t\tif (v.notNull()) {\n\t\t\t\tv.");
			
			if (p.isArray() || p.isBindable())
			{
				a("change.");
				a(listChangeHandler? "observe(this, " : "bind(this, "); a(p.name); a("Changed);\n\t");
				if (listChangeHandler && !Util.isSingleValue(p.type)) {
					a("\t\t\tv.setChangeHandler(objectChangedHandler("); a(p.name.toUpperCase()); a("));\n\t");
				}
			}
			else if (!Util.isSingleValue(p.type)) {
				a("as(ValueObjectBase).change.bind(this, objectChangedHandler("); a(p.name.toUpperCase()); a("));\n\t");
			}
		}
		a("\t\t\t");
		var ifExprAdded = addPropChangeFlagSetter(i, "v" + (p.isBindable()? ".value" : ""), p.type, false);
		if (listChangeHandler || p.isBindable() || !Util.isSingleValue(p.type)) {
			a("\n\t\t\t}");
			if (ifExprAdded) {
				a("\n\t\t\t");
				addPropChangeFlagUnsetter(i);
			}
		}
		
		a("\n\t\t\t");
		a("\n\t\t\tthis."); a(p.name); a(" = v;\n");
		a("\t\t}\n");
		a("\t}\n");
		
		if (p.isBindable() || p.isArray())
		{
			a("\tprivate function "); a(p.name); a("Changed(");
			
			if (!listChangeHandler) {
				a("value : "); a(typeName); a(", old : "); a(typeName);
			}
			a(") : Void {\n\t\t_changedFlags |= "); a(hexBitflag(i));
			a(";\n\t\t");
			
			if (listChangeHandler)
			{
				a("if ("); a(p.name); a(".length.not0()) _propertiesSet |= "); a(hexBitflag(i)); a("; "); addPropChangeFlagUnsetter(i);
			}
			else
				addPropChangeFlagSetter(i, "value", p.type, false);
			a("\n\t}\n\n");
		}
	}
	
	private function addPropChangeFlagSetter(i, path, ptype, bindable)
	{
		var ifAdded = !addIfVarIsSetExpr(path, ptype, bindable, "_propertiesSet |= " + hexBitflag(i));
		
		if (ifAdded) {
			a(" "); addPropChangeFlagUnsetter(i);
		}
		
		return ifAdded;
	}
	
	private function addPropChangeFlagUnsetter(i) {
		a("else _propertiesSet &= 0x" + StringTools.hex(0xFFFFFF ^ (1 << i)) + ";");
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
		a("\tstatic public function fromValue(i:Int) : "); a(def.fullName); a("\n\t{\n\t\t");
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
		var c = HaxeUtil.getConstructorCall(ns.baseType, false, HaxeUtil.getConstructorInitializer(ns.baseType));
		if (c != null) { a(" = "); a(c); }
		else throw "non constructable type? " + ns;
		
		a("\n\t\t\ttmp.setFromXML(child);");
		a("\n\t\t\tthis.add(tmp);");
		a("\n\t\t}\n"); // end for
	}
}

private class HaxeUtil
{
	public static function getConstructorCall(ptype:PType, bindable:Bool, initializer:String)
	{
		var code = switch (ptype)
		{
			case Tarray(type, _,_):
				"new " + HaxeUtil.haxeType( ptype, true, bindable ) + "("+ initializer +")";
			
			case Tdef(_), Turi, TfileRef, Tinterval:
				initializer; //"new " + HaxeUtil.haxeType(ptype) + "("+ initializer +")";
			
			case Tinteger(_,_,_), Tdecimal(_,_,_), TuniqueID, Temail, Tstring, Tbool(_), TenumConverter(_), Tdate, Tdatetime, Tcolor:
				initializer;
		}
		
		return bindable? "new primevc.core.RevertableBindable("+ code +");" : if (code == null) null else code + ";";
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
		
		case Turi, TfileRef, Tinterval, Tdecimal(_,_,_), TuniqueID, TenumConverter(_), Tdate, Tdatetime, Tcolor:
			null;
	}
		
	public static function getClassConstructorCall(tdef:TypeDefinition)
	{
		// Dont 'new' enums.
		return if (!Std.is(tdef, EnumDef))
			"new " + tdef.fullName + "VO()";
		  else null;
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
	public static function haxeType(ptype:PType, ?immutableInterface:Bool = false, ?bindable:Bool = false, ?constructorArg = false)
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
			case TuniqueID:					'primevc.types.ObjectId';
			case Temail:					'primevc.types.EMail';
			case Tcolor:					'primevc.types.RGBA';
			case TfileRef:					'primevc.types.FileRef';
			
			case Tdef(ptypedef):			switch (ptypedef) {
				case Tclass		(def): (immutableInterface? def.module.fullName + ".I" + def.name : def.fullName) + "VO";
				case Tenum		(def): def.fullName;
			}
			case TenumConverter(enums):		'String';
			
			case Tarray(type, min, max):
				return
				if (!constructorArg) {
					if (Util.isSingleValue(type)) {
			 			(bindable
			 				? 'primevc.core.collections.RevertableArrayList<'
							: 'primevc.core.collections.ArrayList<') + haxeType(type, true) +'>';
					}
					else
			 			'primevc.core.collections.VOArrayList<'+ haxeType(type, true) +'>';
				}
				else
				 	'primevc.utils.FastArray<'+ haxeType(type, true) +'>';
		}
		
		if (bindable)
		{
			// Check for Bindable
			type = "primevc.core.RevertableBindable<"+type+">";
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
		case TuniqueID:					'TClass(primevc.types.ObjectId)';
		case Temail:					'TClass(primevc.types.EMail)';
		case Tcolor:					'TClass(primevc.types.RGBA)';
		case TfileRef:					'TClass(primevc.types.FileRef)';
		
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
			
			case Tdef(_), Tinterval, Tarray(_,_,_):
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
			case Tdate:						path+".getTime()";
			case Tdatetime:					path+".getTime()";
			
			case Turi:						throw "impossible Turi";
			case TfileRef:					throw "impossible TfileRef";
			case Tinterval:					throw "impossible Tinterval";
			case TuniqueID:					throw "impossible Tuniq";
			case Temail:					throw "impossible Temail";
			case Tdef(ptypedef):			throw "impossible int conversion Tdef: "+ptypedef;
			
			case TenumConverter(enums):		throw "not implemented";
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
			
			case Tinterval:					throw "impossible";
			case Turi:						throw "impossible";
			case TfileRef:					throw "impossible";
			case TuniqueID:					throw "impossible";
			case Temail:					throw "impossible";
			case Tdef(ptypedef):			throw "impossible";
			
			case TenumConverter(enums):		throw "not implemented";
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
