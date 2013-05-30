package prime.tools.valueobjects.specs.output;
 import prime.tools.valueobjects.specs.output.HaxeSpecHelpers;
 import prime.tools.valueobjects.VODefinition;

class HaxeSpecs extends hxspec.Specification
{
	function Should_convert_class_definitions_to_code()
	{
		this.runner.add(new ClassDefSpecs());
		this.runner.add(new EnumDefSpecs());
		this.runner.add(new XMLMappingSpecs());
		this.currentTest.done = true;
	}
	
	/** Generate a .hx file which imports all enum util classes and makes them available with 'using'. **/
	function Should_generate_using_utility_import_module_for_all_enum_types()
	{
		
	}
}

private class EnumDefSpecs extends CodegenSpecs
{
	var e:EnumDef;
	
	function define_LinkTarget()
	{
		e = m._enum("LinkTarget", {}, {
			_blank:			{desc: "New window (_blank)"},
			_self:			{desc: "Same window (_self)"},
			target:			[string, {desc: "Another window target"}]
		});
		run_enum_codegen();
	}
	
	function Should_generate_definition()
	{
		define_LinkTarget();
		
		var Then = The;
		var haXe = haxe;
		
		haXe.onWriteComplete = function(name:String) if (name == "LinkTarget"){
			Then(haXe.toCode()).should.contain.text("enum LinkTarget");
		}
		
		run_enum_codegen();
	}
	
	function Should_generate_utils_class()
	{
		define_LinkTarget();
		
		var Then = The;
		var haXe = haxe;
		var code_was_generated = false;
		
		haXe.onWriteComplete = function(name:String) if (name == "LinkTarget_utils"){
			Then(haXe.toCode()).should.contain.text("class LinkTarget_utils");
			code_was_generated = true;
		}
		run_enum_codegen();
		
		The(code_was_generated).should.be._true();
	}
	
	function run_enum_codegen()
	{
		m.finalize();
		m.finalizeOptions();
		
		haxe.genEnum(e);
		haxe_code = haxe.toCode();
	}
}

private class ClassDefSpecs extends ClassGenSpecBase
{
	static var all_types = ClassGenSpecBase.all_types;
	static var all_props = ClassGenSpecBase.all_props;
	
	function Should_omit_package_when_module_lies_in_root()
	{
		define_EmptyTest(Module.root);
		The(haxe_code).should.not.contain.text("package");
	}
	
	function Should_generate_definition()
	{
		define_EmptyTest(Module.declare("test"));
		The(haxe_code).should.contain
			.text("package test;")
			.then("class Test\n{")
			.then("}");
	}
	
	function Should_generate_UIText_fields()
	{
		define_UIText();
		The(haxe_code).should.contain
			.text("public var title")
			.then(":")
			.then("String;");
	}
	
	function Should_generate_all_regular_data_fields()
	{
		define_All();
		for (t in all_types)
			has_defined_type(t);
	}
	
	function Should_generate_readonly_properties()
	{
		define_UIText();
		The(haxe_code).should.contain
			.text("public var title")
			.then("(default,null)")
			.then(":")
			.then("String;");
	}
	
	function Should_generate_setter_properties()
	{
		define_All();
		for (i in 0 ... all_types.length) {
			var t = all_types[i];
			The(haxe_code).should.contain
			.text("public function set"+t.substr(0,1).toUpperCase() + all_props[i].substr(1))
			.then("(v:"+t+") :")
			.then(c.fullName)
			.then("{")
			.then("this."+ all_props[i] +" = v;")
			.then("return this;")
			.then("}");
		}
	}
	
	function Should_generate_constructor()
	{
		define_UIText();
		var and = The(haxe_code).should.contain
			.text("public function new()")
			.then("{");
			
			and.then("title = '';");
			
			and.then("}\n");
	}
	
	function Should_set_default_values_in_constructor()
	{
		
	}

/*	
	function Should_generate_validate_function()
	{
		define_UIText();
		The(haxe_code).should.contain
			.text("public function isValid()").then("Bool")
			.then("{")
			.then("}\n");
	}
*/
	
	function Should_generate_fromXML_function()
	{
		define_UIText();
		The(haxe_code).should.contain
			.text("public static function fromXML(node:Xml)").then(":").then("test.UIText")
			.then("{")
			.then("}\n");
	}
	
	function Should_have_XMLMapping()
	{
		define_All();
		var map:XMLMapping = c.getOptionDef(XMLMapping);
		
		The(map).should.not.be._null();
		The(map.root).should.not.be._null();
		The(map.root.children).should.not.be._null();
	}
}

private class XMLMappingSpecs extends ClassGenSpecBase
{
	static var all_props						= ClassGenSpecBase.all_props;
	static var all_mapping						= ClassGenSpecBase.all_mapping;
	static var all_mapping_attr					= ClassGenSpecBase.all_mapping_attr;
	static var all_mapping_props				= ClassGenSpecBase.all_mapping_props;
	static var all_mapping_props_conversions	= ClassGenSpecBase.all_mapping_props_conversions;
	
	var map:XMLMapping;
	
	function Should_generate_toXML_code_from_mapping()
	{
		define_All();
		
		var and = The(haxe_code).should.contain
			.text("public function toXML(parent:Xml) : Void")
			.then("{")
			.then("var node0__allNode = Xml.createElement(\"allNode\");");
		
		var it_should_contain = and;
		for (i in 0 ... all_mapping_props.length)
		{
			var property = c.property.get(all_props[i]);
			if(property == null) throw (i);
			The(property).should.not.be._null();
			
			var nodeVarname:String = null;
			
//			if (i < 10) {
				for (ci in 0 ... map.root.children[0].children.length) if (map.root.children[0].children[ci].nodeName == all_mapping_props[i]) {
					nodeVarname = 'node0_child'+ci+'__'+all_mapping_props[i];
					break;
				}
//			}
//			else /* the bool conversions */
//				nodeVarname = 'node0_child_'+i+'__'+all_mapping_props[i];
			
				
			Var(nodeVarname).should.not.be._null();
			Var(all_mapping_props[i]).should.not.be._null();
			
			and = it_should_contain
				.then('var '+ nodeVarname +' = Xml.createElement("')
				.before('var', all_mapping_props[i])
				.before('var', '");');
			
			Var(all_props[i]).should.not.be._null();
			
			if (Std.string(property.type).indexOf("UIText") > 0) 
				and.then(all_props[i]+".toXML(").then(nodeVarname).then(')');
			else {
				Var(all_mapping_props_conversions[i]).should.not.be._null();
				
				and.then(nodeVarname+".addChild(Xml.createPCData")
				.before('node0', all_mapping_props_conversions[i])
				.before('node0', all_mapping_props[i].indexOf("_not_") > 0? "(!" : "(")
				.before('node0', all_props[i])
				.before('var', "node0__allNode.addChild("+nodeVarname+");");
			}
		}
		
		and.then("}\n");
		
		this.haXe_code_should_compile();
	}
	
	function Should_generate_fromXML_code_from_mapping()
	{
		define_All();
		
		var and = The(haxe_code).should.contain
			.text("public static function fromXML(node:Xml) : test.All")
			.then("{");
		
		var it_should_contain = and;
		for (i in 0 ... all_mapping_props.length)
		{
			var property = c.property.get(all_props[i]);
			if(property == null) throw (i);
			The(property).should.not.be._null();
			
			if (all_mapping_props[i] == "url") and = it_should_contain
				.then('case "'+ all_mapping_props[i] +'":')
				.before("case", all_props[i]+".parse(")
				.before("case", "allNode_child.firstChild().nodeValue");
			
			else if (all_mapping_props[i] != "uitext") and = it_should_contain
				.then('case "'+ all_mapping_props[i] +'":')
				.before("case", "."+all_props[i]+" = ")
				.before("case", "allNode_child.firstChild().nodeValue");
			
			Var(all_props[i]).should.not.be._null();
			
		}
		
		and.then("}\n");
	}
	
	function Should_generate_toXML_code_for_attributes_from_XMLMap()
	{
		define_All();
		
		var toXML_should_contain =
			The(haxe_code).should.contain.text("function toXML");
		
		toXML_should_contain
			.before('function', 'set("string", this.string_val);');
		
		toXML_should_contain
			.before('function', 'set("uitext_title", this.test_uitext_val.title);');
		
		toXML_should_contain
			.before('function', 'set("uitext_text", this.test_uitext_val.text);');
		
		toXML_should_contain
			.before('function', 'set("bool",')
			.before("set(",		"fromBool")
			.before(");",		"this.bool_val");
		
		toXML_should_contain
			.before('function', 'set("bool_as_int",')
			.before("set(",		"fromInt")
			.before(");",		"this.bool_val");
		
		toXML_should_contain
			.before('function', 'set("bool_not",')
			.before("set(",		"!this.bool_val");
		
		
	//	print(haxe_code);
	}
	
	function Should_generate_fromXML_code_for_attributes_from_XMLMap()
	{
		define_All();
		
		var fromXML_should_contain =
			The(haxe_code).should.contain.text("function setFromXML");
		
		fromXML_should_contain
			.before('function', '.string_val = _child.get("string");');
		
	}
	
	function Should_map_array_to_nodes()
	{
		c = m._class("TestBook", {}, {
			pages:	[arrayOf(".TestPage"), {
				min:  4,
				warn: "A mini-animation needs at least 4 pages"
			}],
		}, [xmlmap({
			minibook: {addChildren: ['pages']}
		})]);
		
		m._class("TestPage", {}, {
			num:		[integer],
		}, [xmlmap({
			page: attributes({
				num:	'{num}',
			})
		})]);
		
		run_codegen();
		
		The(c.findProperty("pages").parent.defaultXMLMap.root.children[0]).should.not.be._null();
		
		The(haxe_code).should.contain.text("function toXML")
			.then("for").then(" in ").then("pages")
			.then("toXML(");
		
		this.haXe_code_should_compile();
	}
	
	function Should_support_date_formatting()
	{
		c = m._class("DateTest", {}, {
			lastupdate:	[datetime]
		}, [xmlmap({
			minibook: attributes({
				lastupdate:	'{(format, "d/m/Y") lastupdate}'
			})
		})]);
		
		// org.valueobjects.Datetime.parse ("d/m/Y", string_value);
		// org.valueobjects.Datetime.format("d/m/Y", date_value);
		
	/*	var s:String;
		var lastPos = 0;
		s.substr(lastPos, lastPos = s.indexOf("/") + 1);
		s.substr(lastPos, lastPos = s.indexOf("/") + 1);
		s.substr(lastPos);
		
		var dateformat = ~/([0-9]+)\/([0-9]+)\/([0-9]+)/;
		dateformat.match("");
		var d = new Date(Std.parseInt(dateformat.matched(3)), 
	*/	
		run_codegen();
		
		The(haxe_code).should.contain.text("function toXML")
			.then("addChild")
			.then("fromDate(")
			;
	}
	
	function Should_support_joining_arrays_to_string_value()
	{
		c = m._class("JoinMe", {}, {
				keywords: [arrayOfType(Tstring)]
			}, 
			[xmlmap({ metakeys: '{(join ", ") keywords}' })]
		);
		
		run_codegen();
		
		The(haxe_code).should.contain.text("function toXML")
			.then("addChild")
			.then('keywords.join(", ")')
			;
		
		The(haxe_code).should.contain.text("function fromXML")
			.then("keywords = ")
			.then('.split(", ")')
			;
	}
	
	function Should_create_subclass_instances_when_attribute_matches()
	{
		c = m._class("Frame", {},
		{
			id: [string]
		}, [xmlmap({ link: attributes({
			type: [	'1', '.URLLinkFrame',
					'2', '.PageLinkFrame',
					'3', '.PageLinkFrame']
		})})]);
		
		m._class("LinkFrame", {_extends: ".Frame"}, {
			title: [string]
		},
		 [xmlmap({
			link: attributes({ id: '{id}', title: '{title}' })
		 })]
		);
		
		m._class("URLLinkFrame", {_extends: ".LinkFrame"}, {
			url:		[URI],
		},
		 [xmlmap({
			link: attributes({ type: '1', url:   '{url}' })
		 })]
		);
		
		m._class("PageLinkFrame", {_extends: ".LinkFrame"}, {
			pageID:		[string]
		},
		 [xmlmap({
			link: attributes({ type: '2', page_id: '{pageID}' })
		 })]
		);
		
		var xml1 = 'Xml.parse(\'<link type="1" id="i1" url="u1"/>\').firstElement()',
			xml2 = 'Xml.parse(\'<link type="2" id="i2" page_id="p2"/>\').firstElement()',
			xml3 = 'Xml.parse(\'<link type="3" id="i3" page_id="p3"/>\').firstElement()';
		
		Calling (code('
			var o1 = Frame.fromXML('+xml1+'),
				o2 = Frame.fromXML('+xml2+'),
				o3 = Frame.fromXML('+xml3+');
			
			untyped {
			print(Std.is(o1, URLLinkFrame)+"-"+Std.is(o2, PageLinkFrame)+"-"+Std.is(o3, PageLinkFrame)+", ");
			print("i1:"+o1.id +" i2:"+ o2.id +" i3:"+ o3.id +", ");
			print(o1.url.toString() +"-"+ o2.pageID +"-"+ o3.pageID);
			}
		'))
		.should._return.value('true-true-true, i1:i1 i2:i2 i3:i3, u1-p2-p3');
		
	/*	
		// Frame fromXML()
		
		//  ---
		//  static function fromXML(x:Xml)
		var x = Xml.createElement("test");
		
		// Switch op alle matchAttributes en voer bijbehorende fromXML factory uit
		switch (x.get("type")) {
			case '2','3':
				return PageLinkFrame.fromXML(x);
			case '1':
				return URLLinkFrame.fromXML(x);
			default:
				return setFromXml(x, new Frame());
		}
		
		// ---
		//  static function setFromXML(x:Xml, instance:Frame)
		
		// normale waardes setten:
		id = x.get("id");
		
		// ---
		// PageLinkFrame.fromXML(x:Xml)
		var friend : { setFromXml: function (x:Xml, i:Instance) } = Frame;
		friend.setFromXml(x);
		
		// normale waardes setten:
		// ...
	*/	
	//	run_codegen();
		
	//	this.haXe_code_should_compile();
		
	}
	
	// -------
	// Helpers
	// -------
	
	override function define_All()
	{
		super.define_All();
		initMap();
	}
	
	function initMap()
	{
		map = c.getOptionDef(XMLMapping);
		
		The(map).should.not.be._null();
		The(map.root).should.not.be._null();
		The(map.root.children).should.not.be._null();
	}
	
	function define_AttributeXMLMapped()
	{
		define_UIText();
		
		c = m._class("AttributeXMLMapped", {}, {
			string_val:			[string],
			test_uitext_val:	[is("test.UIText")],
		}, [xmlmap({
			nodeWithAttributes : attributes({
				string:	'{string_val}',
				title:	'{test_uitext_val.title}',
				text:	'{test_uitext_val.text}'
			})
		})]);
		
		c.finalize();
		c.finalizeOptions();
		
		instantiate_haxe_generator();
		haxe.genClass(c);
		haxe_code = haxe.toCode();
		
		
		initMap();
		
		The(map.root.children.length).should.equal(1);
		The(map.root.children[0].attributes).should.not.be._null();
	}
}
