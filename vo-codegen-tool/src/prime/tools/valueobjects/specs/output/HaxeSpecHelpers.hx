package prime.tools.valueobjects.specs.output;
 import prime.tools.valueobjects.VODefinition;



// --------------
// Helper classes
// --------------

private class ModuleVisitor implements CodeGenerator
{
	public var imports (default, null) : List<String>;
	
	public function new() {
		imports = new List();
	}
	
	public function newModule	(m:Module)		: CodeGenerator {
		return this;
	}
	
	private function add(importName:String)
	{
		imports.add("import "+importName+";");
	}
	
	public function genClass	(def:ClassDef)	: Void	{ add(def.fullName); }
	public function genStruct	(def:StructDef)	: Void	{ add(def.fullName); }
	public function genI18N		(def:I18NDef)	: Void	{ add(def.fullName); }
	public function genEnum		(def:EnumDef)	: Void	{ add(def.fullName); }
}

private class HaxeProxy extends org.valueobjects.output.Haxe
{
	public var onWriteComplete : String->Void;
	
	override function write(name:String)
	{
		super.write(name);
		if (onWriteComplete != null) onWriteComplete(name);
	}
}

class CodegenSpecs extends hxspec.Specification
{
	var m:Module;
	var haxe:HaxeProxy;
	var haxe_code:String;
	
	var moduleVisitor : ModuleVisitor;
	
	override function before()
	{
		super.before();
		this.m = Module.declare("test");
		this.moduleVisitor = new ModuleVisitor();
		
		haxe = new HaxeProxy(m);
	}
	
	override function after()
	{
		m = null; haxe_code = null; haxe = null;
		Module.reinitialize();
		org.valueobjects.output.Haxe.reinitialize();
	}
	
	function run_codegen()
	{
		m.finalize();
		m.finalizeOptions();
		
		m.generateWith(haxe);
		haxe_code = haxe.toCode();
	}
	
	function haXe_code_should_compile()
	{
		m.generateWith(this.moduleVisitor);
		var f = neko.io.File.write("CompileTest.hx", false);
		var hxfile = this.moduleVisitor.imports.join("\n") + " class CompileTest {}";
		f.writeString(hxfile);
		f.close();
		
		var p = new neko.io.Process("haxe", ["--no-output", "-swf9", "compiletest.swf", "CompileTest"]);
		var e = p.exitCode();
		
		if (e > 0) {
			neko.FileSystem.deleteFile("CompileTest.hx");
			throw "\n\n --- haXe compile failed: \n"+ p.stderr.readAll().toString()+"\n\n"+ hxfile;
		}
		
		neko.FileSystem.deleteFile("CompileTest.hx");
	}
	
	function code(code:String)
	{
		run_codegen();
		
		m.generateWith(this.moduleVisitor);
		var f = neko.io.File.write("CompileTest.hx", false);
		var hxfile = this.moduleVisitor.imports.join("\n") + " class CompileTest { static var print = neko.Lib.print; static function main(){\n" + code + "\n} }";
		f.writeString(hxfile);
		f.close();
		
		var p = new neko.io.Process("haxe", ["-x", "CompileTest"]);
		var e = p.exitCode();
		
		if (e > 0) {
			neko.FileSystem.deleteFile("CompileTest.hx");
			throw "\n\n --- haXe compile failed: \n"+ p.stderr.readAll().toString()+"\n\n"+ hxfile;
		}
		
		if (neko.FileSystem.exists("CompileTest.n") )
			neko.FileSystem.deleteFile("CompileTest.n");
		
		neko.FileSystem.deleteFile("CompileTest.hx");
		
		return p.stdout.readAll().toString();
	}
}

class ClassGenSpecBase extends CodegenSpecs
{
	var c:ClassDef;
	
	override function after()
	{
		super.after();
		c = null;
	}
	
	function define_UIText()
	{	
		c = m._class("UIText", {}, {
			title:		[string],
			text:		[string]
		}, [xmlmap({
			testNode : attributes({
				title: '{title}',
				text:  '{text}'
			})
		 })]
		);
		
		c.finalize();
		
		haxe = new HaxeProxy(m);
		haxe.genClass(c);
		haxe_code = haxe.toCode();
	}
	
	static var all_types : Array<String> = [
		"Bool", "Int", "Float", "String", 
		"org.valueobjects.types.UniqueID",
		"org.valueobjects.types.URI",
		"org.valueobjects.types.EMail", 
		"org.valueobjects.types.Color",
		"org.valueobjects.types.Bitmap",
		"test.UIText",
		
		"Bool", "Bool", "Bool"
	];
	
	static var all_props : Array<String> = [
		"bool_val", "int_val", "float_val", "string_val",
		"org_valueobjects_types_uniqueid_val",
		"org_valueobjects_types_url_val",
		"org_valueobjects_types_email_val",
		"org_valueobjects_types_color_val",
		"org_valueobjects_types_bitmap_val",
		"test_uitext_val",
		
		"bool_val", "bool_val", "bool_val"
	];
	
	static var all_mapping_props : Array<String> = [
		"bool", "int", "float", "string",
		"uniqueid", "url", "email",
		"color", "bitmap",
		"uitext",
		
		"bool_as_int", "bool_not_as_int", "bool_not"
	];
	
	static var xmlPackage = "org.valueobjects.xmlmap.";
	
	static var all_mapping_props_conversions : Array<String> = [
		"XMLString.fromBool",
		"XMLString.fromInt",
		"XMLString.fromFloat",
		'',
		"XMLString.fromUniqueID",
		"XMLString.fromURI",
		"XMLString.fromEMail",
		"XMLString.fromColor",
		"XMLString.fromAsset",
		"NOT A CONVERSION",
		
		"XMLString.fromInt",
		"XMLString.fromInt",
		"XMLString.fromBool",
	];
	
	static var all_mapping = {
		bool:				'{bool_val}',
		bool_as_int:		'{(int)bool_val}',
		bool_not_as_int:		'{!(int)bool_val}',
		bool_not:			'{!bool_val}',
		int:				'{int_val}',
		float:				'{float_val}',
		string:				'{string_val}',
		url:				'{org_valueobjects_types_url_val}',
		uniqueid:			'{org_valueobjects_types_uniqueid_val}',
		email:				'{org_valueobjects_types_email_val}',
		uitext:				'{test_uitext_val}',
		color:				'{org_valueobjects_types_color_val}',
		bitmap:				'{org_valueobjects_types_bitmap_val}'
	};
	
	static var all_mapping_attr = {
		bool:				'{bool_val}',
		bool_as_int:		'{(int)bool_val}',
		bool_not_as_int:		'{!(int)bool_val}',
		bool_not:			'{!bool_val}',
		int:				'{int_val}',
		float:				'{float_val}',
		string:				'{string_val}',
		url:				'{org_valueobjects_types_url_val}',
		uniqueid:			'{org_valueobjects_types_uniqueid_val}',
		email:				'{org_valueobjects_types_email_val}',
		uitext_title:		'{test_uitext_val.title}',
		uitext_text:		'{test_uitext_val.text}',
		color:				'{org_valueobjects_types_color_val}',
		bitmap:				'{org_valueobjects_types_bitmap_val}'
	};
	
	function define_All()
	{
		define_UIText();
		
		c = m._class("All", {}, {
			bool_val:			[true],
			int_val:			[integer],
			float_val:			[decimal],
			string_val:			[string],
			org_valueobjects_types_url_val:				[URI],
			org_valueobjects_types_uniqueid_val:		[UniqueID],
			org_valueobjects_types_email_val:			[EMail],
			test_uitext_val:	[is("test.UIText")],
			org_valueobjects_types_color_val:			[color],
			org_valueobjects_types_bitmap_val:			[bitmap],
		}, [xmlmap({
			allNode:			all_mapping,
			allNodeAttributes:	attributes(all_mapping_attr),
			allNodeBoth:		[all_mapping, attributes(all_mapping_attr)]
		})]);
		
		c.finalize();
		c.finalizeOptions();
		run_codegen();
	}
	
	override function run_codegen()
	{
		super.run_codegen();
		haxe.genClass(c);
		haxe_code = haxe.toCode();
	}
	
	function instantiate_haxe_generator()
	{
		haxe = new HaxeProxy(m);
	}
	
	function define_EmptyTest(module:Module)
	{
		var cl = module._class("Test", {}, {});
		haxe = new HaxeProxy(Module.root);
		haxe.genClass(cl);
		haxe_code = haxe.toCode();	
	}
	
	function has_defined_type(type:String)
	{
		The(haxe_code).should.contain
			.text("public var "+ type.toLowerCase().split(".").join("_") +"_val")
			.then(":")
			.then(type+";");
	}
}
