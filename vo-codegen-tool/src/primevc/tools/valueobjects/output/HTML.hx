package primevc.tools.valueobjects.output;
 import primevc.tools.valueobjects.VODefinition;
  using primevc.utils.ArrayUtils;

class HTML implements CodeGenerator
{
	private static var codegen = new Hash<CodeGenerator>();
	private static var modules = new Hash<HTML>();
	
	public static function generate(file:String)
	{
		var html = new HTML();
		Module.root.generateWith(html);
		
		var modkeys = Lambda.array({iterator: modules.keys});
		modkeys.sortAlphabetically();
		
		var mods = [];
		for (m in modkeys) {
			var h = modules.get(m);
			if (!h.hasTypes) continue;
			
			h.finalize();
			mods.push(h);
		}
		
		var body = t_page.execute({modules: mods});
		
		neko.io.File.write(file, false).writeString(body);
	}
	
	private function new() {
		result  = new StringBuf();
		hasTypes = false;
	}
	
	private var moduleName : String;
	private var module : Module;
	private var hasTypes:Bool;
	private var html : String;
	private var result:StringBuf;
	
	private function finalize()
	{
		html = result.toString();
		result = null;
	}
	
	private static function typeLink(t:TypeDefinition) : String
	{
		return '<a href="#'+t.fullName+'">'+t.name+'</a>';
	}
	
	private static function typeHTML(pname:String, ptype:PType) : String
	{
		if (ptype == null) return 'NULL TYPE';
		
		var minmax = function(min:Float, max:Float):String return (min != null? ' <span>min = '+min+'</span>' : '') + (max != null? ' <span>max = '+max+'</span>' : '');
		
		return switch(ptype)
		{
			case Tdate:						'<b>date</b>';
			case Tdatetime:					'<b>datetime</b>';
			case Tinterval:					'<b>interval</b>';
			case Tinteger(min,max,stride):	'<b>integer</b>'+ minmax(min,max);
			case Tdecimal(min,max,stride):	'<b>decimal</b>'+ minmax(min,max);
			case Tbool(bool):				'<b>boolean</b><span title="default value">'+bool+'</span>';
			case TenumConverter(prop):		'<b>conversion </b>' + Lambda.map(prop.enums, function(e:Enumeration){ return "<span>"+e.name +" &harr; "+ e.conversions.get(prop.name)  +"</span>"; }).join(" ");
			case Tarray(etype, min, max):	'<b>array of </b>'+typeHTML(pname, etype)+' '+ minmax(min,max);
			
			case Tdef(ptypedef):			typeLink(Util.unpackPTypedef(ptypedef));
			case TclassRef(className):		'<b>'+className+'</b>';
			
			case Turi,
				TuniqueID,
				TfileRef,
				Tstring,
				Temail,
				Tcolor:
				return '<b>'+ Std.string(ptype).substr(1) +'</b>';
		}
	}
	
	private static function propertiesHTML(properties:Hash<Property>)
	{
		var keys = properties.keys;
		var propKeys = Lambda.array({iterator: keys});
		
		propKeys.sortAlphabetically();
		
		var props = [];
		for (k in propKeys) {
			var p = properties.get(k);
			props.push({
				type: typeHTML(p.name, p.type),
				name: p.name
			});
		}
		
		return t_properties.execute({
			property: props,
		});
	}
	
	private static function linkTypes(prefix:String, types:Iterable<TypeDefinition>)
	{
		var s = new StringBuf();
		s.add(prefix);
		
		s.add("<ul>");
		for (t in types) s.add('<li><a href="#'+t.fullName+'">'+ t.fullName +'</a></li>');
		s.add("</ul>");
		
		var str = s.toString();
		return str.length > prefix.length + (4 + 5)? str : '';
	}
	
	// ---
	// Visitor implementation
	// ---
	
	public function newModule	(m:Module)		: CodeGenerator
	{
		var html = new HTML();
		html.module = m;
		html.moduleName = m.fullName;
		html.result.add(
			t_mod.execute({fullName: m.fullName})
		);
		
		modules.set(m.fullName, html);
		
		return html;
	}
	
	private static function pathstr(to:Property) {
		return (to == null? '(UNBOUND!)' : (to.parent == null? 'UNKNOWN' : to.parent.fullName) +'::'+ to.name);
	}
	
	private function genXMLMapValueHTML(xmapval:XMLMapValue)
	{
		return switch(xmapval)
		{
			case XM_empty: '';
			case XM_String		(val):				val;
			case XM_not			(val):				'!'+genXMLMapValueHTML(val);
			
			case XM_binding		(path, to):			'<span class="binding">{'+path+'}</span>';
			case XM_toInt		(path, to):			'<span class="binding">{(int) '+path+'}</span>';
			case XM_join		(path, to, sep):	'<span class="binding">{(join \"'+sep+'\") '+path+'}</span>';
			case XM_format		(path, to, format):	'<span class="binding">{(format \"'+format+'\") '+path+'}</span>';
			case XM_children	(_, p):				genXMLMapNodeHTML((cast Util.unpackPTypedef(Util.getPTypedef(p.type))).defaultXMLMap.root);
			case XM_child		(_, p):				genXMLMapNodeHTML((cast Util.unpackPTypedef(Util.getPTypedef(p.type))).defaultXMLMap.root);
			
			case XM_typeMap(m):
				var s = new StringBuf();
				for (k in m.keys()) {
					s.add(', ');
					s.add(k);
					s.add(' =&gt; <span class="binding">');
					s.add(typeLink(m.get(k)));
					s.add('</span>');
				}
				s.toString().substr(2);
			
			case XM_concat(values):
				var s = new StringBuf();
				for (v in values) s.add(genXMLMapValueHTML(v));
				s.toString();
		}
	}
	
	private function genXMLMapNodeHTML(node:XMLMapNode, level = 0)
	{
		if (node == null) return '';
		
		var s = new StringBuf();
		
		// root node
		if (level == 0) {
			for (c in node.children) s.add(genXMLMapNodeHTML(c, 1));
			return s.toString();
		}
		
		if (level > 0) for (i in 0 ... level) s.add("&nbsp; ");
		
		s.add('&lt;'); s.add(node.nodeName);
		
		if (node.attributes != null) for (a in node.attributes.keys()) {
			s.add(' '); s.add(a); s.add('="');
			s.add(genXMLMapValueHTML(node.attributes.get(a)));
			s.add('"');
		}
		
		if (node.children.length == 0)
			s.add('/&gt;');
		else {
			s.add('&gt;\n<br/>');
			
			for (c in node.children)
				s.add(genXMLMapNodeHTML(c, level + 1));
			
			for (i in 0 ... level) s.add("&nbsp; ");
			s.add('&lt;/'); s.add(node.nodeName); s.add('&gt;');
		}
		
		s.add("<br />");
		return s.toString();
	}
	
	private function genBaseType	(def:BaseTypeDefinition, type:String)
	{
		var xmap:XMLMapping = def.getOptionDef(XMLMapping);
		var opt:StringBuf = new StringBuf();
		if (xmap != null) {
			opt.add("<br/>");
			opt.add(genXMLMapNodeHTML(xmap.root));
		}
		
		this.result.add(t_type.execute({
			type			: type,
			name			: def.name,
			fullName		: def.fullName,
			implemented		: linkTypes('Implemented by: ', cast def.implementedBy),
			supers 			: linkTypes('Supertypes ', cast def.supertypes),
			properties		: def.property == null? "" : propertiesHTML(def.property) + opt.toString()
		}));
	}
	
	public function genClass	(def:ClassDef)	: Void
	{
		hasTypes = true;
		genBaseType(def, "class");
	}
	
	public function genEnum	(def:EnumDef)	: Void
	{
		hasTypes = true;
		
		var xmap:XMLMapping = def.getOptionDef(XMLMapping);
		var opt:StringBuf = new StringBuf();
		if (xmap != null) {
			opt.add("<br/>");
			opt.add(genXMLMapNodeHTML(xmap.root));
		}
		
		this.result.add(t_type.execute({
			type			: "enum",
			name			: def.name,
			fullName		: def.fullName,
			implemented		: "", //linkTypes('Implemented by: ', cast def.implementedBy),
			supers 			: "", //linkTypes('Supertypes ', cast def.supertypes),
			properties		: propertiesHTML(def.property) + opt.toString()
		}));
	}
	
	
	// ---
	// HTML Templates
	// ---
	
	static var t_page = new haxe.Template('
<html>
 <head><title>ValueObject HTML</title></head>
 <style>
 	html,body { 
 		font: 10pt Monaco, "Lucida Console";
 		color:  #FFF;
		background: #010305;
	}
	ul {
		margin: 0 0 1em 0;
		padding: 0 0 0 1.28em;
	}
	h1,h2,h3 {
		color: #FFFFC0;
	}
	h1 {
		margin: 2em 0 0 0;
		font-size: 1.1em;
		background: #0B2F2F;
		padding: 0.2em;
	}
	h2 {
		font-size: 1.1em;
		margin: 2em 0 0.2em 0;
	}
	h3 {
		font-size: 1.1em;
	}
	table {
		border-top: 1px solid #808080;
		border-left: 2px solid #000;
		padding: 0;
	}
	td {
		padding: 0.2em 0.3em;
		border-right: 2px solid #000;
		border-bottom: 1px solid #606060;
		width: 15em;
		font-size: 9pt;
		background: #1A1D1D;
		text-align: right;
	}
	td + td { 
		text-align: left;
		background: #1D1D1D;
	}
	td span {
		display: block;
		float: left;
		padding: 0.1em 0.3em;
		margin: -0.3em 0.6em 0 -0.3em;
		border: 1px solid #252525;
		background: #333;
		font-size: 0.8em;
		color: #DADBEF;
	}
	a {
		color: #C0CDDD;
		text-decoration: none;
	}
	span, a, b { padding: 0 }
	td b {
		font-weight: normal;
		color: #50CF50;
	}
	td > a, span > a, td > a:visited {
		font-weight: normal;
		color: #A0F020;
	}
	
	a:hover, td > a:hover {
		color: #FFF;
		text-decoration: underline;
	}
	.nav { float: left; position: fixed; top: 10px; background: #1D1D1D; padding: 0.1em; }
	.nav li { padding: 0.1em }
	.nav a { display:block; text-decoration: none; color: #C0C0C0; }
	.nav a:hover { color: #fff; background: #00000B; }
	.module { margin: 10px 10px 10px 350px; }
	.binding { color: #989888; font-size: 0.9em; }
 </style>
 <body>
	<ul class="nav">
	::foreach modules::
		<li><a href="#::moduleName::">::moduleName::</a></li>
	::end::
	</ul>
	::foreach modules::
	<div class="module">
		::html::
	</div>
	::end::
 </body>
</html>
	');
	
	static var t_mod = new haxe.Template('
		<h1><a name="::fullName::">::fullName::</a></h1>
	');
	
	static var t_type = new haxe.Template('
		<h2><b>::type::</b> <a name="::fullName::">::name::</a></h2>
		<div class="supers">::supers::</div>
		<div class="implemented">::implemented::</div>
		::properties::
	');
	
	static var t_properties = new haxe.Template('
		<table cellspacing="0" cellpadding="0">
		::foreach property::
			<tr><td>::type::</td><td>::name::</td></tr>
		::end::
		</table>
	');
}
