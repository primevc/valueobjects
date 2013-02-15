package prime.tools.valueobjects.specs;
 import prime.tools.valueobjects.VODefinition;

class VODefinitionSpecs extends hxspec.Specification
{
	override function before() {
		
	}
	
	override function after() {
		
	}
	
	function Should_copy_implementation_from_interfaces()
	{
		var def = Module.declare("test");
		var klass = def._class("Klass", {_implements: UniqueID}, {test:string});
		
		klass.finalize();
		
		Calling(klass.findProperty("test")).should.not._return._null();
		Calling(klass.findProperty("id")).should.not._return._null();
		
		The(klass.supertypes).should.contain.value(UniqueIDTrait.type);
	}
	
	function Should_have_a_uniqueID_type()
	{
		Calling(UniqueIDTrait.type.findProperty("id")).should.not._return._null();
		
		var t = new MockBaseTypeDefinition();
		t.setMetadata({_implements: UniqueID});
		
		The(t.implementedTypes).should.contain.value(UniqueIDTrait.type);
	}
	
	function Should_implement_an_enumeration_type()
	{
		this.runner.add(new EnumDefSpecs());
		this.currentTest.done = true;
	}
}

private class EnumDefSpecs extends hxspec.Specification
{
	
	function Should_create_conversion_properties()
	{
		var e = new EnumDef("Test", Module.root.findModule("test"));
		e.setDefinition({
			PNG:			{desc: "PNG image",  toExt: "png", toMimetype: "image/png"},
			JPEG:			{desc: "JPEG image", toExt: "jpg"}
		});
		e.finalize();
		
		var png;
		Calling(png = e.enumerations.get("PNG")).should.not._return._null();
		
		var conversionKeys = Lambda.array({ iterator: png.conversions.keys });
		var conversionValues = Lambda.array(png.conversions);
		
		for (accessor in ["toExt", "toString", "toMimetype"])
			The(conversionKeys).should.contain.value(accessor);
		
		for (conversion in ["png", "image/png"])
			The(conversionValues).should.contain.value(conversion);
		
		Calling(e.findProperty("toExt")).should.not._return._null();
	}
}

class MockBaseTypeDefinition extends BaseTypeDefinition
{
	public var implementedTypes:Array<TypeDefinition>;
	
	public function new()
	{
		this.implementedTypes = [];
		super("Mocktype", Module.root.findModule("test"));
	}
	
	override public function implement(type:TypeDefinition) : Void
	{
		this.implementedTypes.push(type);
	}
	
}
