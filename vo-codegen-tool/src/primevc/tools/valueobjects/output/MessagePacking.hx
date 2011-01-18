package primevc.tools.valueobjects.output;
 import primevc.tools.valueobjects.VODefinition;

/**
 * Helper functions to generate VO message packing functions
 * 
 * @author Danny Wilson
 * @creation-date jan 04, 2011
 */
class MessagePacking
{
	var code : StringBuf;
	var def  : ClassDef;
	
	var lastProp			: Property;
	var totalPropsToPack	: Int;
	var totalProps			: Int;
	
	private inline function ac(char:Int)   code.addChar(char)
	private inline function a (str:String) code.add(str)
	
	public function new(code:StringBuf, def:ClassDef)
	{
		this.code = code;
		this.def  = def;
	}
	
	private function definePackerFunction()		Assert.abstract()
	private function defineUnPackerFunction()	Assert.abstract()
	
	private function addPropertyPackerCall(path:String, pType:PType, bindable:Bool)	Assert.abstract()
	private function a_unpackProperty(p:Property) Assert.abstract()
	
	private function expr_incrementMixinCount()		return "++mixin"
	private function expr_decrementPropertyBytes()	return "--propertyBytes;"
	
	private function a_writeByte(byte:String) {
		a("{ o.writeByte("); a(byte); a("); ++b; }");
	}
	
	private function a_return() a("return b;")
	
	private function a_not0(v:String) {
		a("("); a(v); a(").not0()");
	}
	
	private function a_is0(v:String) {
		a("!"); a_not0(v);
	}
	
	private function a_assert(v:String) {
		a("Assert.that("); a(v); a(");");
	}
	
	private function a_assertNot0(v:String) {
		a_assert("("+ v + ") != 0");
	}
	
	private function endPackerFunction() {
		a("\n\t\t");
		a("\n\t\t"); a_return();
		a("\n\t}\n");
	}
	
	private function addIfMixinBitsSet(t:BaseTypeDefinition, offset:Int, expr:String)
	{
		a("\n			if (");
		a_not0( "(propertyBits & " + (t.propertiesSorted.length == 1? "1" : interfacePropertiesBitmask(t, offset)) + " /* " + t.name + " */)");
		a(") "); a(expr);
	}
	
	private function addFullName(t:TypeDefinition, interfaceT = false)
	{
		Util.addFullName(code, t, interfaceT);
	}
	
	private function a_packVOHeaderCallStart() {
		a("\n\t\tb += o.packValueObjectHeader(TYPE_ID, ");
	}
	
	private function a_msgpackVOCallStart(t : TypeDefinition) {
		a("\n\t\t\tb += "); addFullName(t); a(".msgpack_packVO(o, obj, ");
	}
	
	
	public function genSerialization()
	{
		// Count bits/properties for this 'def
		totalPropsToPack = def.numPropertiesDefined;
		for (p in def.propertiesDefined) if (lastProp == null || lastProp.index < p.index) lastProp = p;
		
		genDeSerialization(lastProp);
		
		totalProps = totalPropsToPack;		
		var hasSuper = def.superClass != null;
		
		definePackerFunction();
		
		
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
					a("\n\t\tif ("); a_not0("mixBits"); a(") "); a(expr_incrementMixinCount()); a("; // "); a(def.supertypes.first().fullName);
				}
				else for (t in def.supertypes) {
					addIfMixinBitsSet(t, offset, expr_incrementMixinCount() + ";");
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
		
		a_packVOHeaderCallStart();
		if (hasMixins) a("mixin");
		else if (!hasSuper) a("0");
		
		if (lastProp != null)
		{
			a(", propertyBits);");
		}
		else
			a(", 0);");
		
		a("\n\t\t");
		
		if (totalPropsToPack == 1 && lastProp.index < 8)
		{
			// Single Property optimization
			var p = lastProp;
			
			a("\n\t\tif ("); a_not0("propertyBits"); a(") {");
			a("\n\t\t\t"); a_writeByte("0x" + StringTools.hex(1 << p.index, 2));
			a("\n\t\t\t"); addPropertyPackerCall("obj." + p.name, p.type, p.isBindable()); ac(";".code);
			a("\n\t\t}");
		}
		else if (lastProp != null) // at least one Property to pack
		{
			a("\n\t\tif ("); a_not0("propertyBits"); a(") {"); //a_return();
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
							else if (bit >=  8) { a("\n\t\t"); a_writeByte("0");		bit -=  8; }
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
							a("\n\t\t"); a_assertNot0("propertyBits & " + bitMask);
							a("\n\t\t"); a_writeByte(Std.string(bitMask));
							a("\n\t\t"); addPropertyPackerCall("obj." + p.name, p.type, p.isBindable()); ac(";".code);
							
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
							
							if (p.index < 8)
								a("\n\t\t"); 
							else {
								a("\n\t\tif ("); a_not0("propertyBits"); a(") ");
							}
							
							a_writeByte((def.numPropertiesDefined > 8 && def.numPropertiesDefined - i > 1)? "propertyBits & 0x" + StringTools.hex(mask, 2) : "propertyBits");
							a("\n\t\t");
							if (totalProps <= 8) {	a("\n\t\t"); a_assertNot0("propertyBits"); }
							else				 {	a("\n\t\tif ("); a_not0("(propertyBits & 0xFF)"); a(") { // open group"); }
							a("\n\t\t");
						
							for (b in 0 ... bit) {
								var p = def.propertiesDefined.get(i+1 - bit + b);
								if (p == null) continue;
								a("\n\t\t	if ("); a_not0("(propertyBits & 0x" + StringTools.hex(1 << b, 2) + ")"); a(") ");
								addPropertyPackerCall("obj." + p.name, p.type, p.isBindable()); ac(";".code);
							}
							
							if (totalProps > 8) {	a("\n\t\t} //end 8"); }
							a("\n\t\t");
							if (totalPropsToPack > 0) {
								a("\n\t\tpropertyBits >>>= 8;");
								a("\n\t\t");
							}
							
							bit = 0;
						}
	//					else throw "wtf?";
					}
				}
			}
			a("\n\t\t} // end properties");
		}
		
		if (hasMixins)
		{
			a("\n\t\t");
			if (lastProp != null) {	a("if ("); a_not0("mixin"); a(")"); }
			else				  {	a_assertNot0("mixin"); }
			
			a("\n\t\t{");
			
			if (def.supertypes.length == 1)
			{
				var t = def.supertypes.first();
				a_msgpackVOCallStart(t);
				a(mixinBits == 1? "1" : "mixBits");
				a(");");
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
				
				a("\n			if ("); a_not0("mixin"); a(") ");
				a_msgpackVOCallStart(t); // a("b += "); a(t.fullName); if (Std.is(t,ClassDef) && !cast(t, ClassDef).isMixin) a("VO"); a(".msgpack_packVO(o, obj, mixin);");
				a("mixin);");
				
				if (t != def.supertypes.last()) {
					a("\n\t\t\tmixBits >>>= "); a(t.propertiesSorted.length+";");
					a("\n\t\t\t");
				}
			}
			
			a("\n\t\t}");
			a("\n\t\t");
		}
		
		endPackerFunction();
	}
	
	private function genDeSerialization(lastProp:Property)
	{
		if (lastProp == null) return;
		
		defineUnPackerFunction();
		
		var bit = 8;
		
		for (i in 0 ... lastProp.index + 1)
		{
			if (bit == 8) {
				a("\n\t\n\t\tif ("); a_is0("propertyBytes"); a(") return;");
				a("\n\t\n\t\t"); a(expr_decrementPropertyBytes());
				a("\n\t\n\t\tbits = input.readByte();");
				bit = 0;
			}
			
			var p = def.propertiesDefined.get(i);
			if (p == null) {
				++bit;
				continue;
			}
			
			a("\n\t\tif ("); a_not0("bits & 0x" + StringTools.hex(1 << bit, 2)); a(") ");
			
			a_unpackProperty(p);
			
			++bit;
		}
		
		a("\n\t\t\n\t\tif ("); a_not0("propertyBytes"); a(") reader.discardRemainingVOProperties(propertyBytes);");
		
		a("\n\t}");
	}
	
	static function bitmask(numBits:Int, offset:Int=0)
	{
		var mask = 0;
		for (bit in 0 ... numBits) {
			mask |= 1 << (bit + offset);
		}
		return "0x" + StringTools.hex(mask, 6);
	}
	
	static function interfacePropertiesBitmask(t:BaseTypeDefinition, offset:Int)
	{
		return bitmask(t.propertiesSorted.length, offset);
	}
}
