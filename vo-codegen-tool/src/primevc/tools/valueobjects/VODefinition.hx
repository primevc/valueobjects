package primevc.tools.valueobjects;
 import primevc.utils.ArrayUtils;
 import Type;
  using primevc.utils.NumberUtil;

enum ModuleMember {
	MModule(m:Module);
	MType(type:PTypedef);
	MPending(type:PTypedef, meta:Dynamic, members:Dynamic, options:Array<ClassOption>);
	
	MUndefined(name:String, container:Module);
}

interface CodeGenerator
{
	public function newModule	(m:Module)		: CodeGenerator;
	
	public function genClass	(def:ClassDef)	: Void;
	public function genEnum		(def:EnumDef)	: Void;
}

class Module
{
	static public var pkgRoots	(default,null)	: List<Module>;
	
	static public var root		(default,null)	: Module;
	static public var traits	(default,null)	: Module;
	static public var types		(default,null)	: IntHash<TypeDefinition>;
	
	static public function declare(path:String, isPackageRoot = false):Module
	{
		return switch(Module.root.find(path)) {
			case MModule(m):		if (m.packageRoot = isPackageRoot) pkgRoots.add(m); m;
			case MType(t):			throw "Cannot redeclare type: '"+path+"' "+Std.string(t)+" as module";
			case MUndefined(n,m):	throw "Cannot declare modules that start in Uppercase";
			case MPending(a,b,c,d):	throw "Impossible error?";
		}
	}
	
	static public function reinitialize() {
		pkgRoots = new List();
		root     = new Module("", null);
		types    = new IntHash();
		traits   = declare("primevc.core.traits");
	}
	static var initialize = reinitialize();
	
	// ---
	
	public function mkdir() : String
	{
		var dir:String;
		var fullName = this.fullName;
		
		if (fullName.indexOf(".") > 1)
		{
			var buf = new StringBuf();
			
			dir = null;
			for (m in fullName.split(".")) {
				buf.add(m);
				dir = buf.toString();
				if (!neko.FileSystem.exists(dir)) try
					neko.FileSystem.createDirectory(dir)
					catch (e:Dynamic) throw "Failed creating directory: "+dir;
				
				buf.add("/");
				dir = buf.toString();
				
			}
			return dir;
		}
		
		if (fullName.length < 1) return "";
		
		dir = fullName;
		if (!neko.FileSystem.exists(dir))
			neko.FileSystem.createDirectory(dir);
		return dir;
	}
	
	public  var packageRoot (default,null)		: Bool;
	public  var name		(default,null)		: String;
	private var members		(default,null)		: Hash<ModuleMember>;
	public  var parent		(default,null)		: Module;
	public  var fullName	(getFullName,null)	: String;
	
	private function getFullName():String { return (parent == null || parent == Module.root ? name : parent.getFullName() + '.' + name); }
	
	private function new(name:String, parent:Module) {
		this.name = name;
		this.parent = parent;
		members = new Hash();
	}
	
	private function get(pkg:String)
	{
	//	trace("GET: "+pkg);
		var n = members.get(pkg);
		if (n == null) {
			if (Util.startsUpperCase(pkg)) return MUndefined(pkg, this);
			else members.set( pkg, n = MModule(new Module(pkg, this)) );
		}
		return n;
	}
	
	private function doFind(path:Array<String>, origPath:String) : ModuleMember
	{
		if (path.length == 0) return MModule(this);
		if (path.length == 1) return get(path[0]);
		
		return (path[0].length == 0)
			? parent.doFind( path.slice(path[1].length == 0 /* handles: "../.." */? 2:1), origPath )
			: switch ( get(path[0]) ) { 
				case MModule(m): m.doFind( path.slice(1), origPath );
				
				case MPending(a,b,c,d):	throw "Module["+this.fullName+"]::doFind("+path+") ended in a pending type: "+Std.string(a);
				case MUndefined(n,m):	throw "Module["+   m.fullName+"]::doFind("+path+") ended in undefined type: "+n;
				case MType(t):			throw "Module["+this.fullName+"]::doFind("+path+") ended with a type definition: "+Std.string(t);
			}
	}
	
	private function splitPath(path:String) {
		return StringTools.replace(path, '/', '').split('.');
	}
	
	//
	// -- Public API
	//
	public function generateWith(code:CodeGenerator)
	{
		finalize();
		finalizeOptions();
		
		var list = new List<Module>();
		
		for (member in this.members) switch(member) {
			case MModule(m):			list.add(m);
			case MType(t):			switch(t) {
				case Tclass (def):	code.genClass(def);
				case Tenum  (def):	code.genEnum(def);
			}
			case MPending(t,m,d,o):	throw "Finalizing module '"+this.fullName+"' somehow failed, type(s) pending: "+t ;
			case MUndefined(n, c):	throw "Finalizing module '"+this.fullName+"' somehow failed, undefined type found";
		}
		
		for (m in list) m.generateWith( code.newModule(m) );
		
		list.clear();
	}
	
	public function getMember(name:String) : ModuleMember {
		var m = this.members.get(name);
		return if (m != null) m else MUndefined(name, this);
	}
	
	public function defineType(name:String, type:ModuleMember)
	{
		if (!Util.startsUpperCase(name)) throw Err_NotUppercaseType(this, name);
		if (members.exists(name)) throw Err_TypeExists(type, members.get(name));
		members.set(name, type);
	}
	
	public function find(path:String) : ModuleMember
	{
		return (path.charAt(0) != '.')
			? root.doFind(splitPath(path), path)
			: this.doFind(splitPath(path.substr(1)), path);
	}
	
	public function findModule(path:String) : Module
	{
		return switch (find(path)) {
			case MModule(m):				m;
			case MUndefined(n,m):		m;
			case MType(type):			Util.unpackPTypedef(type).module;
			case MPending(type,m,d,o): 	Util.unpackPTypedef(type).module;
		}
	}
	
	public function toString():String {
		var s = "\n\n<Module name='"+getFullName()+"'>";
		
		for (m in this.members) switch(m) {
			case MModule(t):		s += "\n"+ t.toString();
			case MType(t):			s += "\n\t"+ Util.unpackPTypedef(t).toString();
			case MPending(t,m,d,o):	s += "\n\t<Pending><type>"+ Std.string(t) +"</type>\n\t<definition>"+ Std.string(d)+"</definition>\n\t<options>"+Std.string(o)+"</options>\n</Pending>";
			case MUndefined(n,m):	s += "\n\t<Undefined name='"+n+"'>"+m.fullName+"</Undefined>";
		}
		return s;
	}
	
	static public function defineTypeIndex(index:Int, type:TypeDefinition)
	{
		if (types.exists(index))
			throw "Type index: " + index + " for '"+ type.fullName +"' already used: " + types.get(index);
		
		types.set(index, type);
	}
	
	private function addPendingDefition(index:Int, kind:PTypedef, name:String, metadata:Dynamic, definition:Dynamic, ?options:Array<ClassOption>) : TypeDefinition
	{
		var s = this.splitPath(name);
		var defName = s[s.length - 1];
		if (!Util.startsUpperCase(defName)) throw Err_NotUppercaseType(this, defName);
		
		var mod:Module;
		
		switch( doFind(s, name) )
		{
			case MModule(m):		mod = m;
			case MUndefined(n,m):	mod = m;
			case MType(t):			throw "shouldnt happen";
			
			case MPending(type, ex_meta, ex_members, ex_options):
				var id = Type.enumIndex(kind);
				if (Type.enumIndex(type) != id) switch (type)
				{
					case Tenum(e):		throw "There is an enum pending with the same name as "+		Type.getEnumConstructs(PTypedef)[id] +": "+ name;
					case Tclass(cl):	throw "There is a class pending with the same name as "+		Type.getEnumConstructs(PTypedef)[id] +": "+ name;
				}
				
				trace("Warning: adding/overwriting properties in pending definition: "+name);
				Util.overwriteProps(ex_meta,	metadata);
				Util.overwriteProps(ex_members,	definition);
				Util.overwriteProps(ex_options,	options);
				
				return Util.unpackPTypedef(type);
		}
		
		var def:TypeDefinition;
		switch (kind)
		{
			case Tclass(d):		kind = Tclass	(cast def = new ClassDef(index, defName, mod));
			case Tenum(e):		kind = Tenum	(cast def = new EnumDef (index, defName, mod));
		}
		mod.members.set(defName, MPending(kind, metadata, definition, options));
		defineTypeIndex(index, def);
		
		return def;
	}
	
	public function _class(index:Int, name:String, metadata:Dynamic, definition:Dynamic, ?options:Array<ClassOption>) : ClassDef {
		var def = this.addPendingDefition(index, Tclass(null), name,metadata,definition,options);
	//	trace("Defined class: "+ def.fullName);
		return cast def;
	}
	
	public function mixin(index:Int, name:String, definition:Dynamic) : ClassDef {
		var def : ClassDef = cast this.addPendingDefition(index, Tclass(null), name, {}, definition, []);
		def.isMixin = true;
	//	trace("Defined mixin: "+ def.fullName);
		return def;
	}
	
	public function _enum(index:Int, name:String, metadata:Dynamic, definition:Dynamic) {
	//	trace("Defining enum "+name);
		return cast this.addPendingDefition(index, Tenum(null), name, metadata, definition, []);
	}
	
	public function finalize()
	{
	//	trace("Finalizing module: "+this.fullName);
		
		var pass = 0;
		var pendingCount = 0;
		do 
		{
			if (pass > 100) throw pass;
			
	//		trace("Module pass: " + (++pass));
			pendingCount = 0;
			
			for (m in this.members) switch(m)
			{
				case MModule(m):	m.finalize();
				case MType(t):		Util.unpackPTypedef(t).finalize();
				
			 	case MPending(t,m,d,o):
					var target:TypeDefinition = Util.unpackPTypedef(t);
					target.finalize();
					this.members.set(target.name, MType(t));
					++pendingCount;
				
				case MUndefined(n,m):	throw "error ?";
			}
		}
		while (pendingCount != 0);
	}
	
	public function finalizeOptions()
	{
		for (m in this.members) switch(m)
		{
			case MModule(m): m.finalizeOptions();
			
		 	case MPending(t,m,d,o):	throw "impossible ?";
			case MUndefined(n,m):	throw "error ?";
			case MType(t):
				var target:TypeDefinition = Util.unpackPTypedef(t);
				target.finalizeOptions();
		}
	}
}

class Util
{
	static public function addFullName(code:StringBuf, t:TypeDefinition, interfaceT = false)
	{
		var a = code.add;
		
		if (interfaceT) {
			a(t.module.fullName); a(".I"); a(t.name);
		}
		else a(t.fullName);
		
		if (Std.is(t,ClassDef) && !cast(t, ClassDef).isMixin) a("VO");
	}
	
	static public function toString(v:Dynamic)
	{
		if (Std.is(v,String)) return v;
		
		if (Std.is(v,SpecialValue)) switch(cast(v,SpecialValue)) {
			case staticurl(uri): return uri;
		}
		
		return switch (Type.typeof(v))
		{
			case TFloat:	Std.string(v);
			case TInt:	Std.string(v);
			
			default: '';
		}
	}
	
	static public function startsUpperCase(s:String):Bool {
		return s.substr(0,1) == s.substr(0,1).toUpperCase();
	}
	
	static public function capitalize(s:String):String {
		return s.substr(0,1).toUpperCase() + s.substr(1);
	}
	
	static public function unpackMType(type:ModuleMember) : PTypedef
	{
		var pdef:PTypedef;
		
		switch(type)
		{
			case MType(type):			pdef = type;
			case MPending(type, m,d,o):	pdef = type;
			case MModule(m):			throw Err_ModuleInsteadOfType(null, m.name);
			case MUndefined(n,m):		throw Err_UndefinedType(m == Module.root ? n : m.fullName+"."+n);
		}
		
		return pdef;
	}
	
	static public function getIDProperty(t:PType) return switch (t) {
		case Tdef(ptypedef): switch (ptypedef) {
			case Tclass		(def):	getIDPropertyFromTWithProperties(def);
			case Tenum		(def):	throw "Enums have no ID property";
		}
		default:
			throw "Only Value-objects have ID properties";
	}
	
	static public function getIDPropertyFromTWithProperties(t:TypeDefinitionWithProperties) {
		var found = Lambda.filter(t.property, function(p) return p.hasOption(unique));
		if (found.length > 1) throw "Only 1 unique ID property per VO supported";
		
		return found.first();
	}
	
	static public function inPType(type:PType, def:TypeDefinition) : Bool {
		return def == unpackPTypedef(getPTypedef(type));
	}
	
	static public function isArray(type:PType) return switch (type) {
		case Tarray(_,_,_): true;
		default: false;
	}
	
	static public function isMixin(type:PType) return switch (type) {
		case Tdef(type): switch(type) {
			case Tclass (def):	def.isMixin;
			case Tenum  (def):	false;
		}
		default: false;
	}
	
	static public function isEnum(type:PType) return switch (type) {
		case Tdef(type): switch(type) {
			case Tclass (def):	false;
			case Tenum  (def):	true;
		}
		case TenumConverter(_): true;
		default: false;
	}
	
	static public function unpackPTypedef(type:PTypedef) : TypeDefinition
	{
		if (type == null) return null;
		
		switch (type) {
			case Tclass (def):	return def;
			case Tenum  (def):	return def;
		}
	}
	
	static public function getPTypedef(type:PType) : PTypedef
	{
		if (type == null) return null;
		
		return switch (type) {
			case Tdef(type): 				type;
			case Tarray(type, min, max):	getPTypedef(type);
			
			default: null;
			//Turi,TuniqueID,Tstring,Tinteger,Temail,Tdecimal,Tcolor,Tbool
		}
	}
	
	static public function overwriteProps(existing, newProps) {
		for (f in Reflect.fields(newProps)) Reflect.setField(existing, f, Reflect.field(newProps, f));
	}
	
	static public function isPTypeIterable(ptype:PType)
	{
		return switch(ptype)
		{
			case Tarray(_,_,_):			true;
			
			case Tdef(_):				false;
			case Turi:					false;
			case TuniqueID:				false;
			case TfileRef:				false;
			case Tstring:				false;
			case Tinteger(_,_,_):		false;
			case TenumConverter(_):		false;
			case Temail:				false;
			case Tdecimal(_,_,_):		false;
			case Tcolor:				false;
			case Tbool(_):				false;
			case Tdate:					false;
			case Tdatetime:				false;
			case Tinterval:				false;
			case TclassRef(_):			false;
		}
	}
	
	static public function isPTypeBuiltin(ptype:PType)
	{
		return switch(ptype)
		{
			case Tdef(p):				false;
			
			case Tarray(_,_,_):			true;
			case Turi:					true;
			case TuniqueID:				true;
			case TfileRef:				true;
			case Tstring:				true;
			case Tinteger(_,_,_):		true;
			case TenumConverter(_):		true;
			case Temail:				true;
			case Tdecimal(_,_,_):		true;
			case Tcolor:				true;
			case Tbool(_):				true;
			case Tdate:					true;
			case Tdatetime:				true;
			case Tinterval:				true;
			case TclassRef(_):			true;
		}
	}
	
	static public function isSingleValue(ptype:PType)
	{
		return switch(ptype)
		{
			case Tdef(p):				
				switch(p) {
					case Tenum		(_): true;
					case Tclass		(_): false;
				}
			
			case Tarray(t,_,_):			isSingleValue(t);
			case Turi:					true;
			case TuniqueID:				true;
			case Tstring:				true;
			case TfileRef:				true;
			case Tinteger(_,_,_):		true;
			case TenumConverter(_):		true;
			case Temail:				true;
			case Tdecimal(_,_,_):		true;
			case Tcolor:				true;
			case Tbool(_):				true;
			case Tdate:					true;
			case Tdatetime:				true;
			case Tinterval:				true;
			case TclassRef(_):			true;
		}
	}
	
	static public inline function isDefinedInSuperClassOf(def:ClassDef, p:Property)
		return def.extendsType(p.parent) || (def.superClass != null && def.superClass.property.exists(p.name))
}

enum MetadataError {
	/** Thrown when stumbled upon an invalid metadata property **/
	Err_InvalidArgument(option:String, given:String, expected:String);
}

enum ResolveError {
	/** The requested property could not found, possibly still waiting to be defined. **/
	Err_PropertyNotFound(name:String);
	/** While looking for the requested property, a property with no members blocked the search. **/
	Err_PropertyHasNoMembers;
}

enum PropertyDefinitionError {
	/** A property with this name was already defined **/
	Err_PropertyExists		(property:Property, existing:Property);
	/** A type with this name was already defined **/
	Err_TypeExists			(type:ModuleMember, existing:ModuleMember);
	
	/** An unhandled runtime-type was used in the type-definition for this property **/
	Err_UnknownType			(p:PropertyTypeResolver, runtimeType:ValueType);
	/** No type could be resolved for this property **/
	Err_NoType				(p:PropertyTypeResolver, def:Dynamic);
//	Err_UndefinedType		(p:PropertyTypeResolver, name:String);
	/** There is somehow more then 1 type defined for this property **/
	Err_MultiType			(p:PropertyTypeResolver, def:Dynamic);
	/** A type-option/modifier was set which couldn't be handled' **/
	Err_UnknownTypeOption	(p:PropertyTypeResolver, name:String, value:Dynamic);
	/** A module definition was found instead of a type for this property **/
	Err_ModuleInsteadOfType	(p:PropertyTypeResolver, typename:String);
	
	/** Tried to define a type which didn't start with an Uppercase letter. **/
	Err_NotUppercaseType		(m:Module, name:String);
	
	/** The property got a value supplied which was of an incompatible type **/
	Err_IncompatibleDefaultValue(p:Property, value:Dynamic);
}

enum TypeDefinitionError {
	/** The type referenced by name was never defined **/
	Err_UndefinedType		(name:String);
	/** The type name given in _extends could not be used (it's probably not a class) **/
	Err_CannotExtendType		(type:TypeDefinition);
	/** The type name given in _implements could not be used (it's probably an enum) **/
	Err_CannotImplementType	(type:TypeDefinition);
}

enum TodoListState {
	CannotFinish;
}

class TodoList
{
	/** List of tasks put aside for later. Should return true when completed and should be removed from list. **/
	private var todo : List<Bool->Bool>;
	
	public var hasPending (checkForPending,null) : Bool;
	
	private function checkForPending() {
		return todo != null && todo.length > 0;
	}
	
	public function new() {
		todo = new List();
	}
	
	public function tryToComplete()
	{
		handleTodoList();
		if (todo != null && todo.length > 0) throw CannotFinish;
	}
	
	public function addSubtask(task:Void->Void) {
		add(function(v) {
			try task() catch(err:TodoListState) return false;
			return true;
		});
	}
	
	public function add(task:Bool->Bool) {
		if (todo == null) todo = new List();
		todo.add(task);
	}
	
	public function tryOrQueue(task:Bool->Bool) {
		if (task(false)) return;
		add(task);
	}
	
	private function handleTodoList() {
		if (todo != null) {
			// Try to get everything done
			for (item in todo) if(item(true)) todo.remove(item);
		}
	}
}

class PropertyTypeResolver
{
	static public function resolve(property:Property, def:Dynamic)
	{
		var r = new PropertyTypeResolver();
		r.prop = property;
		r.fullDef = def;
		r.run(def);
		if (r.type == null) throw Err_NoType(r, def);
		return r.type;
	}
	static public function update(property:Property, def:Dynamic)
	{
		trace("Updating: "+property.parent.fullName +"::"+ property.name);
		var r = new PropertyTypeResolver();
		r.type = property.type;
		r.prop = property;
		r.fullDef = def;
		r.run(def);
	}
	
	private function new() {
		todo = new TodoList();
		handleAttr = new AttributeHandler(this);
	}
	// ---
	
	var inArray:Bool;
	var todo:TodoList;
	// --- End state
	
	public var fullDef : Dynamic;
	
	public var prop	: Property;
	public var type	: Null<PType>;
	
	private var handleAttr : AttributeHandler;
	
	private function run(def:Dynamic)
	{
		if (def == null) return;
		var runtimeType = Type.typeof(def);
		
		if (!inArray && Std.is(def, Array)) {
			inArray = true;
			for (d in cast(def,Array<Dynamic>)) run(d);
			inArray = false;
			
			todo.tryToComplete();
			return;
		}
		
		switch(runtimeType)
		{
			case TBool:
				if (type != null) throw Err_MultiType(this, fullDef);
				type = Tbool(def == true);
			
			case TEnum(e):
				if 		(Type.getEnumName(e) == Type.getEnumName(AbstractPType))	handleAbstractPType(def);
				else if	(Type.getEnumName(e) == Type.getEnumName(PType))			handlePType(def);
				else if	(Type.getEnumName(e) == Type.getEnumName(PropertyOption))	handlePropertyOption(def);
				else if	(Type.getEnumName(e) == Type.getEnumName(Platform))			handlePlatform(def);
				else throw Err_UnknownType(this, runtimeType);
			
			case TObject:
				for (f in Reflect.fields(def)) { var val = Reflect.field(def, f);
					switch(f) {
					case "desc", "description":	prop.description = val;
					case "min":					todo.tryOrQueue(callback(this.handleAttr.min, val));
					case "max":					todo.tryOrQueue(callback(this.handleAttr.max, val));
				}}
			
			case TClass(cl):
				throw Err_UnknownType(this, runtimeType);
			
			case
			  TInt:
				this.prop.index = Std.int(def);
			
			case
			  TFloat,
			  TFunction,
			  TNull,
			  TUnknown:
				throw Err_UnknownType(this, runtimeType);
		}
		
		if (!this.inArray) todo.tryToComplete();
	}
	
	static function builtinAbstractPType(type:AbstractPType) : Null<PType>
	{
		return switch(type) {
			case integer:		Tinteger();
			case decimal:		Tdecimal();
			case color:			Tcolor;
			case date:			Tdate;
			case datetime:		Tdatetime;
			case interval:		Tinterval;
			case string:		Tstring;
			case URI:			Turi;
			case EMail:			Temail;
			case UniqueID:		TuniqueID;
			case FileRef:		TfileRef;
			case ClassRef(r):	TclassRef(r);
			
			case subclass(_,_,_),
				namedSetOf(_,_,_,_),
				is(_),
				arrayOfType(_),
				arrayOf(_):
				throw "Non-primitive: "+type;
		}
	}
	
	private function handlePropertyOption(value:PropertyOption)
	{
		if (prop.opts == null)
			prop.opts = [value];
		else if (!Lambda.has(prop.opts, value))
			prop.opts.push(value);
	}


	private function handlePlatform (value:Platform)
	{
		if (prop.platforms == null)
			prop.platforms = [value];
		else
			prop.platforms.push(value);
	}

	
	private function handleAbstractPType(value:AbstractPType)
	{
		type = switch(value)
		{
			case integer, decimal, color, date, datetime, interval, string, URI, EMail, UniqueID, FileRef, ClassRef(_):
				builtinAbstractPType(value);
			
			case is(typeName):
				switch( prop.parent.module.find(typeName) ) {
					case MModule(m):		throw Err_ModuleInsteadOfType(this, typeName);
					case MUndefined(n,m):	throw Err_UndefinedType(typeName);
					
					case MType(t):			if (Util.isMixin(Tdef(t))) throw "Cannot use mixins as member variables, property: '"+this.prop.name+"', in type: '"+this.prop.parent.fullName+"'\n"+t; Tdef(t);
					case MPending(t,m,d,o):	Tdef(t);
				}
			
			case subclass(index, meta, propertyMap):
				var sname = prop.parent.name + Util.capitalize(prop.name);
			//	trace(" -- Subclass: "+sname);
				
				Tdef(Tclass( prop.parent.module._class(index, sname, meta, propertyMap, []) ));
			
			case arrayOfType(t):
				Tarray(t);
			
			case arrayOf(basetype):
				Tarray(Tdef( Util.unpackMType(prop.parent.module.find(basetype)) ));
				
			case namedSetOf(atype, index, uniqueKeys, propertyMap):
				var itemName = Util.capitalize(prop.name) + 's';
				var nsName = prop.parent.name + itemName;
				var base:PType = switch(atype)
				{	
					case arrayOfType(ptype):	Tarray(ptype);
					case arrayOf(path):			Tarray(Tdef( Util.unpackMType(prop.parent.module.find(path)) ));
					
					case is(path):
						switch(prop.parent.module.find(path)) {
							case MType(t):			Tdef(t);
							case MPending(t,m,m,o):	Tdef(t);
							
							case MModule(m):		throw Err_ModuleInsteadOfType(this, path);
							case MUndefined(n,m):	throw Err_UndefinedType(path);
						}
					
					case integer, decimal, color, date, datetime, interval, string, URI, EMail, UniqueID, FileRef, ClassRef(_):
						builtinAbstractPType(atype);
					
					case namedSetOf	(_,_,_,_):	throw "namedSetOf(namedSetOf(...)) unsupported";
					case subclass	(_,_,_):	throw "namedSetOf(subclass(...)) unsupported";
				}
				
				Assert.that(base != null);
				
				var ns = new NamedSetDef(index, nsName, Util.capitalize(prop.name)+'s', prop.parent.module, base);
				ns.setUniqueKeys(uniqueKeys);
				ns.setPropertyMap(propertyMap);
				
				Module.defineTypeIndex(index, ns);
				
				var nt = Tclass(ns);
				prop.parent.module.defineType(nsName, MPending(nt,uniqueKeys,propertyMap,null));
				
				Tdef(nt);
		}
	}
	
	private function handlePType(value:PType)
	{
		type = value;
	/*	switch(value)
		{
			case Tnumber(min,max,stride):
			case Tcolor:
			case Tbool(value):
		}
	*/
	}
}

class AttributeHandler
{
	var p:PropertyTypeResolver;
	
	public function new(parent:PropertyTypeResolver){ this.p = parent; }
	
	public function min(value:Dynamic, verbose:Bool):Bool
	{
		if (p.type == null) {
			if (verbose) trace("Failed to apply 'min':"+Std.string(p));
			return false;
		}
		
		switch (p.type) {
			case Tinteger(min, max, stride):	p.type = Tinteger(value, max, stride);
			case Tdecimal(min, max, stride):	p.type = Tdecimal(value, max, stride);
			case Tarray(type, min, max):		p.type = Tarray(type, value, max);
			
			default:
				throw Err_UnknownTypeOption(p, "min", value);
		}
		
		return true;
	}
	
	public function max(value:Dynamic, verbose:Bool):Bool
	{
		if (p.type == null) {
			if (verbose) trace("Failed to apply 'max':"+Std.string(p));
			return false;
		}
		
		switch (p.type) {
			case Tinteger(min, max, stride):	p.type = Tinteger(min, value, stride);
			case Tdecimal(min, max, stride):	p.type = Tdecimal(min, value, stride);
			case Tarray(type, min, max):		p.type = Tarray(type, min, value);
			
			default:
				throw Err_UnknownTypeOption(p, "max", value);
		}
		
		return true;
	}
}

class Property
{
	private var mappings					: Array<MappingType>;
	
	public var index						: Int;
	public var name			(default,null)	: String;
	public var parent		(default,null)	: TypeDefinitionWithProperties;
	public var definedIn	(default,null)	: TypeDefinitionWithProperties;
	
	public var type							: PType;
	public var opts							: Array<PropertyOption>;
	public var description					: String;
	public var defaultValue	(default,null)	: Dynamic;
	public var platforms					: Array<Platform>;
	
	
	public function new(name:String, parent:TypeDefinitionWithProperties) {
		this.index = primevc.types.Number.INT_NOT_SET;
		this.name = name;
		this.mappings = [];
		this.parent = this.definedIn = parent;
	}
	
	public function copyFor(newParent:TypeDefinitionWithProperties)
	{
		var p			= new Property(this.name, newParent);
		p.index			= this.index;
		p.definedIn		= this.definedIn;
		p.type			= this.type;
		p.description	= this.description;
		p.defaultValue	= this.defaultValue;
		
		if (this.opts != null)
			p.opts = this.opts.copy();
		
		return p;
	}
	
	public function bitIndex()
	{
		return if (Std.is(parent, BaseTypeDefinition)) cast(parent,BaseTypeDefinition).bitIndex(this); else this.index;
	}
	
	public function propertyID()
	{
		return definedIn.index << 8 | this.index;
	}
	
	public function isBindable()
	{
		return hasOption(PropertyOption.bindable);
	}
	
	public function isTransient()
	{
		return hasOption(transient);
	}
	
	public function isArray ()
	{
		return switch (this.type) {
			case Tarray(type,min,max):	true;
			default:					false;
		}
	}
	
	public function isDisposable ()
	{
		return isBindable() || switch (this.type) {
			case Tdef(type):				!Util.isEnum(this.type);
			case Tarray(type,min,max):		true;
			default:						false;
		}
	}
	
	public function hasClonableType ()
	{
		return switch (this.type) {
			case Tdef(_):					!Util.isEnum(this.type);
			case Tarray(_,min,max):			true;
			default:						false;
		}
	}
	
	public function hasOption(option:PropertyOption) {
		if (opts != null) for (opt in opts) if (opt == option) return true;
		return false;
	}

	public inline function isPlatformSpecific () : Bool {
		return platforms != null && platforms.length > 0;
	}
	
	public function checkDefaultValue(defaultval:Dynamic)
	{
		var self	= this;
		var assert	= function(v) if (!v) throw Err_IncompatibleDefaultValue(self, defaultval);
		var hasType	= function(t:Dynamic) return Std.is(defaultval,t);
		
		switch (this.type)
		{
			case Tdecimal(min,max,stride):
				assert(hasType(Int) || hasType(Float));
				if (min != null)	assert(defaultval >= min);
				if (max != null)	assert(defaultval <= max);
				if (stride != null)	assert(defaultval == (defaultval / stride) * stride);
			
			case Tinteger(min,max,stride):
				assert(hasType(Int));
				if (min != null)	assert(defaultval >= min);
				if (max != null)	assert(defaultval <= max);
				if (stride != null)	assert(defaultval == (defaultval / stride) * stride);
			
			case Turi:				assert(hasType(String) || Util.toString(defaultval) != '');
			case TuniqueID:			assert(hasType(String) || hasType(Int));
			case Tstring:			assert(hasType(String));
			case Temail:			assert(hasType(String));
			case Tcolor:			assert(hasType(Int));
			case Tbool(v):			assert(hasType(Int) || hasType(Bool));
			case TfileRef:			assert(hasType(String));
			case TclassRef(_):		throw "not implemented";
			
			case TenumConverter(e):	throw "not implemented";
			case Tdate:				throw "not implemented";
			case Tdatetime:			throw "not implemented";
			case Tinterval:			throw "not implemented";
			case Tarray(t,x,y):		throw "not implemented";
			
			case Tdef(td):
				var base:BaseTypeDefinition;
				switch (td) {
					case Tenum(def):
						if (!def.enumerations.exists(defaultval))
							throw Err_IncompatibleDefaultValue(this, defaultval);
						
						return;
					
					case Tclass(def):	base = def;
				}
				try {
					base.finalize();
				} catch(e:Dynamic) {}
				
				if (Type.typeof(defaultval) != TObject) throw Err_IncompatibleDefaultValue;
				
				for (key in Reflect.fields(defaultval))
				{
					if (!base.property.exists(key)) throw Err_PropertyNotFound(key);
					base.property.get(key).checkDefaultValue( Reflect.field(defaultval,key) );
				}
				
		}
	}
	
	public function setDefaultValue(val:Dynamic) {
		checkDefaultValue(val);
		this.defaultValue = val;
	}
	
	public function toString():String {
		return "\n\t\t<Property idx='"+index+"' name='"+name+"' defined-in='"+ this.definedIn.fullName +"' parent='"+parent.fullName+"'>"+Std.string(type)+"</Property>";
	}
	
	public function isMappedTo(t:MappingType):Bool {
		return Lambda.has(mappings, t);
	}
	
	public function copyOptions(prop:Property) {
		trace("copyOptions not implemented ("+ this.definedIn.fullName +"."+ prop.name +")");
	}
	
	public function shouldHaveGetter () : Bool {
		return !hasOption(transient) && !Util.isPTypeBuiltin(type) && !Util.isEnum(type);
	}
	
	public function shouldHaveSetter () : Bool {
		return !hasOption(transient);
	}
}

interface TypeDefinition
{
	public var index		(default,null)		: Int;
	public var name			(default,null)		: String;
	public var fullName		(getFullName,null)	: String;
	public var module		(default,null)		: Module;
	public var description						: String;
	public var finalized	(default,null)		: Bool;
	
	public function finalize() : Void;
	public function finalizeOptions() : Void;
	public function toString() : String;
	
	public function findProperty(path:String) : Property;
	public function doFindProperty(node:Array<String>) : Property;
	public function tryTasks() : Void;
	
	public function getOptionDef<T>(cl:Class<IClassOption>) : T;
}

class Enumeration
{
	public var name : String;
	public var parent : EnumDef;
	public var conversions : Hash<String>;
	public var description : String;
	public var type : PType;
	public var intValue (default, null) : Int;
	
	public function new(name:String, parent:EnumDef) {
		this.intValue = primevc.types.Number.INT_NOT_SET;
		this.description = "";
		this.name = name;
		this.parent = parent;
		this.conversions = new Hash();
		this.conversions.set("toString", name);
	}
	
	public function toString()
	{
		var s = "";
		s += "\n\t\t\t<enum name='"+name+"' desc='"+description+"' type='"+type;
		if (Lambda.count(conversions) > 0)
			s += "'>\n\t\t\t\t<conversions>"+ Std.string(conversions) +"</conversions>\n\t\t\t</enum>";
		else
			s += "'/>";
		
		return s;
	}
	
	public function addConversions(map:Hash<String>) {
		for (key in map.keys()) if (!this.conversions.exists(key)) this.conversions.set(key, map.get(key));
	}
	
	public function copyFor(target:EnumDef) {
		var e = new Enumeration(this.name, target);
		for (key in this.conversions.keys()) e.conversions.set(key, this.conversions.get(key));
		
		return e;
	}
	
	public function parseOption(opt:Dynamic)
	{
		if (Type.getEnum(opt) == AbstractPType)
		{
			switch (cast(opt,AbstractPType)) {
				case string:	this.type = PType.Tstring;
				case integer:	this.type = PType.Tinteger();
				case decimal:	this.type = PType.Tdecimal();
				case color:		this.type = PType.Tcolor;
				case UniqueID:	this.type = PType.TuniqueID;
				case URI:		this.type = PType.Turi;
				case EMail:		this.type = PType.Temail;
				
				default:
					throw "Ignored enumeration type: "+Std.string(opt);
			/*
				case subclass(o,p):
				case namedSetOf(b,u,p):
				case is(x):
				case arrayOfType(t):
				case arrayOf(t):
			*/
			}
			if (this.parent.catchAll != null) throw "Only one catchall enum property allowed ("+this.name+" tried to overwrite "+this.parent.catchAll.name+") for: "+this.parent.fullName;
			this.parent.catchAll = this;
			
			return;
		}
		else switch (Type.typeof(opt))
		{
			case TInt:
				this.intValue = cast opt;
			
			case TFloat:
			
			case TObject:
				for (field in Reflect.fields(opt)) {
					var val = Reflect.field(opt,field);
					
					if (field == "desc" || field == "description")
						this.description = val;
					else if (field == "value")
						this.intValue = Std.is(val, Int)? cast val : throw "Not an int: '"+ val + "', in: "+this;
					
					else if(StringTools.startsWith(field, "to"))
						this.conversions.set(field, Std.string(val));
				}
			
			case TBool, TUnknown, TNull, TFunction, TClass(_), TEnum(_):
				throw "Currently unsupported enum option: "+Std.string(opt) + ", for: "+ this;
		}
		
		
	}
}

typedef EnumConversionTuple = {
	var def		: Enumeration;
	var value	: String;	
}

class EnumConversionProperty extends Property
{
	public var enums		(default,null)	: List<Enumeration>;
	public var definition	(default,null)	: EnumDef;
	
	public function new(name:String, parent)
	{
		this.definition = parent;
		super(name, parent);
		this.enums = new List();
		this.type = TenumConverter(this);
	}
}

class EnumDef implements TypeDefinitionWithProperties
{
//	public var numProperties(default,null)		: Int;
	public var name			(default,null)		: String;
	public var module		(default,null)		: Module;
	public var description						: String;
	
	public var enumerations	(default,null)		: Hash<Enumeration>;
	public var catchAll							: Enumeration;
	public var conversions	(default,null)		: Hash<EnumConversionProperty>;
	public var finalized	(default,null)		: Bool;
	
	public var fullName		(getFullName,null)	: String;
	private var supertypes	(default,null)		: List<EnumDef>;
	
	private function getFullName():String { return module.fullName +'.'+ name; }
	
	public var property		(default,null)		: Hash<Property>;
	public var propTodo		(default,null)		: TodoList;
	
	public var defaultXMLMap(default,null)		: XMLMapping;
	public var index		(default,null)		: Int;
	
	public var propertiesSorted	(getPropertiesSorted,null) : Array<Property>;	
	private function getPropertiesSorted()
	{
		if (propertiesSorted != null) return propertiesSorted;
		
		var nameSorted = Lambda.array(property);
		nameSorted.sort(function(a,b) return ArrayUtils.compareAlphabetically(a.name, b.name));
		
		return propertiesSorted = nameSorted;
	}
	
	public var utilClassName (getUtilClassName, null) : String;
		private function getUtilClassName() {
			return name + "_utils";
		}
	
	public var utilClassPath (getUtilClassPath, null) : String;
		private function getUtilClassPath() {
			return getFullName() + "_utils";
		}
	
	public function new(index:Int, name:String, module:Module) {
//		this.numProperties = 0;
		this.index = index;
		this.description = "";
		this.module = module;
		this.name = name;
		this.enumerations = new Hash();
		this.supertypes = new List();
		this.property = cast this.conversions = new Hash();
		this.propTodo = new TodoList();
	}
	
	
	public function getOptionDef<T>(cl:Class<IClassOption>) : T {
		return null;
	}
	
	public function defineEnumeration(name:String, def:Dynamic)
	{
		var e = this.enumerations.get(name);
		if (e == null) {
			e = new Enumeration(name, this);
			this.enumerations.set(name, e);
		}
		
		if (Std.is(def, Array))
			for (opt in cast(def,Array<Dynamic>)) e.parseOption(opt);
		else
			e.parseOption(def);
		
		Assert.that(e.intValue.isSet(), "\n\tEnumeration '"+ name +"' in '"+ this.fullName +"' has no value\n");
		
		var conversionNum = 0;
		for (k in e.conversions.keys()) {
			var c:EnumConversionProperty = this.conversions.get(k);
			if (c == null) {
				this.conversions.set(k, c = new EnumConversionProperty(k, this));
				c.index = ++conversionNum;
			}
			
			if (!Lambda.has(c.enums, e))
				c.enums.add(e);
		}
	}
	
	public function tryTasks() {}
	
	public function finalize():Void
	{
		if (finalized) return;
		
		var member = this.module.getMember(this.name);
		
		switch (member) {
			case MPending(t,m,d,o): this.setMetadata(m);
			default:
		}
		
		for (type in this.supertypes)
		{
			type.finalize();
			
			for (key in type.enumerations.keys())
			{
				var item = type.enumerations.get(key);
				if (this.enumerations.exists(key))
					this.enumerations.get(key).addConversions(item.conversions);
				else
					this.enumerations.set(key, item.copyFor(this));
			}
		}
		
		switch (this.module.getMember(this.name)) {
			case MPending(t,m,d,o):
				this.setDefinition(d);
			
			default: //throw "impossible";
		}
		
		finalized = true;
	//	trace("Fully resolved: "+this.fullName);
	}
	
	public function extend(type:ModuleMember) : Void
	{
		var def:TypeDefinition = Util.unpackPTypedef(Util.unpackMType(type));
		if (!Std.is(def,EnumDef)) throw Err_CannotExtendType(def);
		this.supertypes.add(cast def);
	}
	
	public function setMetadata(obj:Dynamic)
	{
		for (name in Reflect.fields(obj))
		{
			var opt = Reflect.field(obj, name);
			switch (name) {
				case "_extends": try {
					if (Std.is(opt,String)) {
						this.extend(module.find(opt));
					}
					else if (Std.is(opt,Array)) for (t in cast(opt,Array<Dynamic>))
					{
						if (Std.is(t,String))
							this.extend(module.find(t));
						
						else if (Std.is(t,AbstractPType)) switch(cast(t,AbstractPType))
						{
							case is(type):
								this.extend(module.find(type));
							default:
								throw Err_InvalidArgument("_extends", Std.string(opt), "expected paths to other enums");
						}
					}
				}
				catch(err:TypeDefinitionError) {
					switch(err) {
					 case Err_CannotImplementType(type):
					 case Err_CannotExtendType(type):
						trace(this.fullName + " -> couldn't extend type: '"+type.name+"'");
					 case Err_UndefinedType(name):
						trace(this.fullName + " -> couldn't extend enum: '"+name+"' because it was not defined.");
					}
					throw err;
				}
			}
		}
	}
	
	public function setDefinition(obj:Dynamic)
	{
		for (name in Reflect.fields(obj))
		{
			var def = Reflect.field(obj, name);
			defineEnumeration(name, def);
		}
	}
	
	public function setOptions(obj:Array<ClassOption>)
	{
		
	}
	
	public function finalizeOptions()
	{
		
	}
	
	public function toString():String
	{
		var s = "\t<EnumDef name='"+this.fullName+"' desc='"+this.description+"'>";
		for (e in this.enumerations) {
			s += e.toString();
		}
		s += "\n\t</EnumDef>";
		return s;
	}
	
	public function findProperty(path:String) : Property {
		return doFindProperty(path.split('.'));
	}
	
	public function doFindProperty(node:Array<String>) : Property {
		return this.conversions.get(node[0]);
	}
}

interface IClassOption
{
	public function apply(type:TypeDefinition, data:Dynamic) : Void;
	public function toString() : String;
}

class XMLMap
{
	static public function isSingleValue(v:XMLMapValue)
	{
		return switch (v) {
			case XM_empty:					true;
			case XM_not		(val):			isSingleValue(val);
			
			case XM_String	(_):			true;
			case XM_binding	(_,  to):		Util.isSingleValue(to.type);
			case XM_toInt	(_, val):		true;
			case XM_join	(_, _, _):		true;
			case XM_format	(_, _, _):		true;
			
			case XM_concat	(_):			true;
			case XM_child	(_, _):			false;
			case XM_children(_, _):			false;
			case XM_typeMap	(_):			false;
		}
	}
}

enum XMLMapValue
{
	XM_empty;
	
	XM_not		(val:XMLMapValue);
	
	XM_String	(val:String);
	XM_binding	(path:String,  to:Property);
	XM_toInt	(path:String, val:Property);
	XM_join		(path:String, val:Property, seperator:String);
	XM_format	(path:String, val:Property, formatStr:String);
	
	XM_concat	(values:Array<XMLMapValue>);
	XM_child	(path:String, from:Property);
	XM_children	(path:String, from:Property);
	XM_typeMap	(valueToType:Hash<ClassDef>);
}

class XMLMapStringParser
{
	var node:XMLMapNode;
	
	public function new() {}
	
	public function parse(str:String, node:XMLMapNode)
	{
		if (!Std.is(str, String)) throw "What is this? "+Std.string(str)+ " in " +Std.string(node);
		
		this.node = node;
		if (str.charAt(0) == '{' && str.charAt(str.length - 1) == '}') {
			return brackets(str.substr(1, str.length - 2));
		}
		
		var lastpos = 0;
		var values:Array<XMLMapValue> = [];
		var brackets = this.brackets;
		
		~/{([^}]+)}/.customReplace(str, function(e:EReg) {
			var p = e.matchedPos();
			lastpos += p.len + p.pos;
			values.push( XM_String(e.matchedLeft()) );
			values.push( brackets(e.matched(1)) );
			
			return '';
		});
		
		if (values.length == 0) // Nothing needs to be concatenated
			return XM_String(str);
		
		values.push( XM_String(str.substr(lastpos)) );
		
		return XM_concat(values);
	}
	
	private static function bindProp(path:String, node:XMLMapNode)
	{
		var p;
		try {
			p = node.mapping.type.findProperty(path);
			if (p == null) throw "Property = null?  "+p+" -- "+path+" --> "+node;
			node.mapping.propertiesMapped.add({path: path, prop: p});
			node.valuePath = path;
			
			return p;
		}
		catch (e:ResolveError) {
			var bt:BaseTypeDefinition = cast node.mapping.type;
			throw "Couldn't find property in brackets {"+path+"} in "+bt.toString() +" "+ bt.property;
		}
	}
	
	function brackets(substr:String) : XMLMapValue
	{
		// Check for NOT
		if (substr.charCodeAt(0) == '!'.code)
			return XM_not(brackets(substr.substr(1)));
		
		// Check for value casts/modifier
		var right = substr.indexOf(')');
		if (substr.charCodeAt(0) == '('.code && right > 1)
		{
			var path = StringTools.trim(substr.substr(right+1));
			var p = bindProp(path, this.node);
			
			var mod = substr.substr(1, right-1);
			var mregx = ~/(\w+)\s*(.*)/;
			
			if ( mregx.match(mod) ) switch(mregx.matched(1))
			{
				case "int":
					return XM_toInt(path, p);
				
				case "join":
					var param = ~/"(.*?)"/;
					if (!param.match(mregx.matched(2)))
						throw this.node.nodeName +": join syntax error in: "+substr;
					
					return XM_join(path, p, param.matched(1));
				
				case "format":
					var param = ~/"(.*?)"/;
					if (!param.match(mregx.matched(2)))
						throw this.node.nodeName +": format syntax error in: "+substr;
					
					return XM_format(path, p, param.matched(1));
				
				default:
					throw "Unknown value modifier: "+substr;
			}
		}
		else if(right > -1)
			throw "Unmatched ')' in "+substr;
		
		node.valuePath = substr;
		return XM_binding(substr, bindProp(substr, this.node));
	}
}

class XMLMapNode
{
	/*static function prefixValuePropertyPath(v:XMLMapValue)
	{
		if (v == null) return null;
		
		return switch(v)
		{
			case XM_toInt(path, val):	
			case XM_not(val):
			case XM_join(path, val, sep):
			case XM_empty:					v;
			case XM_concat(vals):
			case XM_children(from):
			case XM_binding
			case XM_String
		}
	}*/
	
	public var nodeName		: String;
	public var attributes	: Hash<XMLMapValue>;
	
	public var valuePath	: String;
	public var value		: XMLMapValue;
	
	public var parent		: XMLMapNode;
	public var children		: Array<XMLMapNode>;
	public var mapping		: XMLMapping;
	
	public function new(parent:XMLMapNode, name:String)
	{
		if (parent != null) this.mapping = parent.mapping;
		this.parent   = parent;
		this.nodeName = name;
		this.children = [];
	}
	
	public function isXMLTypeMap()
	{
		var isTypeMap = false;
		
		// Handle value->type mappings first
		if (this.attributes != null) for (attr in this.attributes.keys()) switch (this.attributes.get(attr))
		{
			case XM_typeMap(map):
				isTypeMap = true;
			default:
		}
		
		for (child in this.children)
			if (!isTypeMap) isTypeMap = child.isXMLTypeMap();
			else break;
		
		return isTypeMap;
	}
	
	function shouldCloneNode(targetNode:XMLMapNode)
	{	
		if (targetNode.nodeName == this.nodeName)
		{
			// Copy value mapping
			if (targetNode.value != null && this.value == null)
				return true;
			
			// Copy attributes
			if (targetNode.attributes != null) for (att in targetNode.attributes.keys()) if (this.attributes == null || !this.attributes.exists(att))
				return true;
		}
		
		if (targetNode.children.length > 0)
			return true;
		
		return false;
	}
	
	/** Returns a new XMLMapNode which is merged with targetNode */
	public function getMerged(targetNode:XMLMapNode)
	{	
		// First check if we should clone this node
		if (targetNode == null || !shouldCloneNode(targetNode))
		 	return this;
		
		trace("Merging XMLMapNode for: "+this.mapping.type.fullName + ", nodeName: " + this.nodeName);
		
		var node = new XMLMapNode(this.parent, this.nodeName);
		node.value = this.value;
		node.children = [];
		
		if (targetNode.nodeName == this.nodeName)
		{
			// Copy value mapping
			if (targetNode.value != null && this.value == null)
				node.value = targetNode.value;
			
			// Copy attributes
			if (this.attributes == null)
			{
				if (targetNode.attributes != null) {
					node.attributes = new Hash();
					for (a in targetNode.attributes.keys())
					 	node.attributes.set(a, targetNode.attributes.get(a));
				}
			}
			else // this map has no attributes
			{
				node.attributes = new Hash();
				
				if (targetNode.attributes != null)
				  for (att in targetNode.attributes.keys()) if (!this.attributes.exists(att))
					node.attributes.set(att, targetNode.attributes.get(att));
				
				for (att in this.attributes.keys())
					node.attributes.set(att, this.attributes.get(att));
			}
		}
		
		for (child in targetNode.children)
 		  for (thisChild in this.children)
			if (child.nodeName == thisChild.nodeName)
				node.children.push(thisChild.getMerged(child));
			else
				node.children.push(child);
		
		/*
		for (child in this.children)
		{
			var add = true;
			for (c in node.children) if (c.nodeName == child.nodeName) {
				add = false;
				break;
			}
		}
		*/
		
		return node;
	}
	
	static var stringParser = new XMLMapStringParser();
	
	private function addChildren(properties:Array<String>)
	{
		for (path in properties)
		{
			var p = mapping.type.findProperty(path);
			var t:TypeDefinitionWithProperties = cast Util.unpackPTypedef(Util.getPTypedef(p.type));
			
			if (t == null) return;
			t.finalizeOptions();
			
			// Only 1 XMLMap-childNode needs to be added, because the type (t) will handle all nodes in setFromXML();
			var node = new XMLMapNode(this, t.defaultXMLMap.root.children[0].nodeName);
			node.valuePath = path;
			node.value = XM_children(path, p);
			
			this.children.push(node);
			/*
			if (t.defaultXMLMap != null && t.defaultXMLMap.root != null && t.defaultXMLMap.root.children != null)
				for (c in t.defaultXMLMap.root.children) {
					var node = new XMLMapNode(this, c.nodeName);
					node.valuePath = path;
					node.value = XM_children(path, p);
					this.children.push(node);
				}
			*/
		}
	}
	
	/*public function copy(parent:XMLMapNode, ?pathPrefix:String)
	{
		var n = new XMLMapNode(parent, this.nodeName);
		n.value = prefixValuePropertyPath(this.value);
		
		for (attr in this.attributes.keys())
			n.attributes.set(attr, prefixValuePropertyPath(this.attributes.get(attr)));
		
		for (child in this.children)
			n.children.push(child.copy(n, pathPrefix));
		
		return n;
	}*/
	
	public function parseData(data:Dynamic) {
		return doParseData(this, data);
	}
	
	
	static function doParseData(obj:XMLMapNode, data:Dynamic)
	{
		var attrs = XMLMapDefinition.attributes;
		
		if (Std.is(data, String)) {
			obj.value = stringParser.parse(data, obj);
		}
		else if(Std.is(data, XMLMapDefinition)) switch(cast(data, XMLMapDefinition)) {
			case emptyNode:
				obj.value = XM_empty;
			
			case attributes(map):
				if (obj.attributes == null) obj.attributes = new Hash();
				for (f in Reflect.fields(map))
				{
					var val = Reflect.field(map, f);
					if (Std.is(val, String))
						obj.attributes.set( f, stringParser.parse(val, obj) );
					else if (Std.is(val, Array))
					{
						// Attribute value-to-subtype mapping
						var map = obj.parseArrayMap(cast val);
						for (v in map.keys()) {
							var cl = map.get(v);
							cl.defaultXMLMap.root.getByNodePath(obj).attributes.set(f, XM_String(v));
						}
						obj.attributes.set( f, XM_typeMap(map) );
					}
					else throw "What is this? " + val;
				}
			
			case childNode(property):
				var p = obj.mapping.type.findProperty(property);
				obj.value = XM_child(property, p);
				if (obj.valuePath == null) obj.valuePath = property;
		}
		else if(Std.is(data, Array)) {
			for (v in cast(data, Array<Dynamic>)) obj.parseData(v);
			return obj;
		}
		else if(Reflect.isObject(data)) for (field in Reflect.fields(data))
		{
		//	children.push(new XMLMapNode(obj, field, data));
			if (field == "addChildren")
			{
				var props : Array<String> = data.addChildren;
				
				if (Std.is(props, String))
					props = cast [props];
				
				if(!Std.is(props, Array))
					throw Err_InvalidArgument("xmlmap("+Std.string(data)+")", data, "Array of children to add");
				
				obj.addChildren(props);
			}
			else {
				obj.children.push( new XMLMapNode(obj, field).parseData(Reflect.field(data, field)) );
			}
		}
		
		return obj;
	}
	
	private function parseArrayMap(arr:Array<Dynamic>)
	{
		if ((arr.length & 1) != 0) throw "Array map should be have an even length:  1 slot for value, 1 slot for target class";
		
		var key:String = null;
		var map:Hash<ClassDef> = new Hash();
		
		for (i in 0 ... arr.length) if (i & 1 == 0)
			key = arr[i];
		else
		{
			var tdef:ClassDef = cast Util.unpackPTypedef(Util.unpackMType(this.mapping.type.module.find(arr[i])));
			if (!Std.is(tdef, ClassDef)) throw "XML typemaps only support class subtypes";
			if (!tdef.extendsType(mapping.type)) throw "XML typemap: mapped value '"+key+"' to type '"+tdef.fullName+"' should be a subclass of: "+mapping.type.fullName;
			
			map.set(key, tdef);
		}
		
		return map;
	}
	
	public function getByNodePath(other:XMLMapNode)
	{
		var nodes = [other.nodeName];
		var n = other;
		while ((n = n.parent).parent != null) {
			nodes.insert(0, n.nodeName);
		}
		
		if (nodes.length == 0) return this;
		
		var depth = 0;
		var currentRoot = this;
		for (search in nodes)
		{
			if (currentRoot.children.length == 1) {
				currentRoot = currentRoot.children[0];
				++depth;
			}
			else for (child in currentRoot.children) if (search == child.nodeName) {
				currentRoot = child;
				++depth;
			}
		}
		
		if (depth != nodes.length) throw "Child node not found in "+this.mapping+": depth != "+nodes.length+", "+nodes;
		
		return currentRoot;
	}
}

class XMLMapping implements IClassOption
{
	public var module						 	: Module;
	public var type								: TypeDefinition;
	public var root								: XMLMapNode;
	public var propertiesMapped	(default, null) : List<{path:String, prop:Property}>;
	
	public function new()
	{
		this.propertiesMapped = new List();
	}
	
	public function apply(type:TypeDefinition, data:Dynamic)
	{
		this.type = type;
		this.root = new XMLMapNode(null, "");
		this.root.mapping = this;
		this.root.parseData(data);
	}
	
	public function getMergedMapping() : XMLMapNode
	{
		if (!Std.is(type, ClassDef)) return this.root;
		
		var cl:ClassDef = cast type;
		if (cl.superClass == null || cl.superClass.defaultXMLMap == null || cl.superClass.defaultXMLMap.root.isXMLTypeMap()) return this.root;
		
		cl = cl.superClass;
		return root.getMerged(cl.defaultXMLMap.getMergedMapping());
	}
	
	/*public function withPropertyPath(name:String, parent:XMLMapping)
	{
		var newroot:XMLMapNode = new XMLMapNode(null, "");
		this.root.mapping = parent;
		
		for (c in root.children)
			newroot.children.push(c.copy(name+"."));
	}*/
	
	public function toString()
	{
		var s =
		'<XMLMapping type="'+type.fullName+'">\n\t' +
			'<properties num='+this.propertiesMapped.length+'>\n';
			for (p in this.propertiesMapped) s += '\t\t'+p.path+' = '+p.prop.toString()+'\n';
			s += '\t</properties>\n</XMLMapping>\n';
		
		return s;
	}
}

interface TypeDefinitionWithProperties implements TypeDefinition
{
	public var property			(default,null) : Hash<Property>;
	public var propTodo			(default,null) : TodoList;
	public var defaultXMLMap	(default,null) : XMLMapping;
	
	public var propertiesSorted	(getPropertiesSorted,null) : Array<Property>;
}

class BaseTypeDefinition implements TypeDefinitionWithProperties
{
	public var index		(default,null)		: Int;
	public var finalized	(default,null)		: Bool;
	
	public var name			(default,null)		: String;
	public var module		(default,null)		: Module;
	public var description						: String;
	public var property		(default,null)		: Hash<Property>;
	public var propertyByNum(default,null)		: IntHash<Property>;
	public var supertypes	(default,null)		: List<BaseTypeDefinition>;
	public var fullName		(getFullName,null)	: String;
	
	public var options		(default,null)		: List<ClassOption>;
	public var defaultXMLMap(default,null)		: XMLMapping;
	
	public var implementedBy(default,null)		: Hash<BaseTypeDefinition>;
	public var imports		(getImports,null)	: List<TypeDefinition>;
	
	public var propertiesDefined(getPropertiesDefined, null) : IntHash<Property>; // key is property.index
	public var numPropertiesDefined(getNumPropertiesDefined, null) : Int; // Count(propertiesDefined)
	public var maxPropertyIndexNonTransient(getMaxPropertyIndexNonTransient, null) : Int; // Count(propertiesDefined)
	public var maxDefinedPropertyIndex(getMaxDefinedPropertyIndex, null) : Int; // max(propertiesDefined.index) non-transient
	public var propertiesSorted	(getPropertiesSorted,  null) : Array<Property>;
	
	public var settings		(default, null)	: {
		var mongo_proxied: MongoProxyType;
	};
	
	public function hasOption(opt:Dynamic) {
		for (o in options) if (o == opt) return true;
		return false;
	}
	
	
	var propertyBitIndex : Hash<Int>;
	var lastPropertyBitIndex : Int;
	
	private function genPropertyBitIndex(t:TypeDefinition)
	{
		if (Std.is(t, BaseTypeDefinition)) {
			for (s in cast(t,BaseTypeDefinition).supertypes) genPropertyBitIndex(s);
		}
		
		for (i in 0 ... propertiesSorted.length) if (propertiesSorted[i].definedIn == t) {
			// found first prop for this type
			var maxPropIndex = 0-1;
			
			var startIndex = lastPropertyBitIndex;
			for (j in i ... propertiesSorted.length) { var p = propertiesSorted[j]; if (p.definedIn != t) break; else {
				if (!p.hasOption(transient) && p.index > maxPropIndex) maxPropIndex = p.index;
				propertyBitIndex.set(p.name, startIndex + p.index);
			}}
			
			lastPropertyBitIndex += maxPropIndex + 1;
			break;
		}
	}
	
	public function bitIndex(prop:Property)
	{
		if (prop.hasOption(transient)) return this.lastPropertyBitIndex + prop.index;
		
		if (propertyBitIndex == null) {
			lastPropertyBitIndex = 0;
			propertyBitIndex = new Hash();
			genPropertyBitIndex(this);
		}
		
		return propertyBitIndex.get(prop.name);
	}
	
	private function getMaxPropertyIndexNonTransient()
	{
		getPropertiesDefined();
		return maxPropertyIndexNonTransient;
	}
	
	private function getNumPropertiesDefined()
	{
		getPropertiesDefined();
		return numPropertiesDefined;
	}
	
	private function getMaxDefinedPropertyIndex()
	{
		getPropertiesDefined();
		return maxDefinedPropertyIndex;
	}
	
	private function getPropertiesDefined()
	{
		if (propertiesDefined != null) return propertiesDefined;
		
		var n = maxDefinedPropertyIndex = -1;
		propertiesDefined = new IntHash();
		for (p in this.property)
		{
			if (!p.hasOption(transient) && bitIndex(p) > maxPropertyIndexNonTransient) maxPropertyIndexNonTransient = bitIndex(p);
			
			if (p.definedIn == this) {
				++n;
				propertiesDefined.set(p.index, p);
				if (!p.hasOption(transient) && p.index > maxDefinedPropertyIndex) maxDefinedPropertyIndex = p.index;
			}
		}
		Assert.that(n <= 31, "Max index used: "+ n + ", class: "+fullName);
		numPropertiesDefined = n + 1;
		return propertiesDefined;
	}
	
	private function getPropertiesSorted()
	{
		if (propertiesSorted != null) return propertiesSorted;
		
		var nameSorted = Lambda.array(property);
		nameSorted.sort(function(a,b) return a.index - b.index);
		
		return propertiesSorted = nameSorted;
	}
	
	private function getImports() {
		var l = new List<TypeDefinition>();
		for (p in this.property) if(p.type != null) switch(p.type)
		{
			case Tdef(type): 
				l.add(Util.unpackPTypedef(type));
			case Tarray(type, min,max):
				var t = Util.getPTypedef(type);
				if (t != null) l.add(Util.unpackPTypedef(t));
			
			default:
			//Turi,TuniqueID,Tstring,Tinteger,Temail,Tdecimal,Tcolor,Tbool
		}
		return l;
	}
	
	private function getFullName():String { return module.fullName +'.'+ name; }
	
	private var todo		: TodoList;
	private var optionsTodo	: TodoList;
	
	public var propTodo		(default,null)		: TodoList;
	
	public function new(index:Int, name:String, module:Module)
	{
		this.index = index;
		this.description = "";
		this.module = module;
		this.name = name;
		this.numPropertiesDefined = 0;
		this.maxPropertyIndexNonTransient = 0;
		this.property = new Hash();
		this.propertyByNum = new IntHash();
		this.implementedBy = new Hash();
		this.options = new List();
		this.supertypes = new List();
		this.todo = new TodoList();
		this.propTodo = new TodoList();
		this.settings = { mongo_proxied: null }
	}
	
	public function getOptionDef<T>(cl:Class<IClassOption>) : T
	{
		for (opt in options) switch (opt) {
			case editable:
			case xmlmap(mapping): if (Std.is(mapping, cl)) return cast mapping;
		}
		
		return null;
	}
	
	public function tryTasks() {
		try todo.tryToComplete() catch(e:Dynamic) trace("Todo unfinished");
	}
	
	public function toString():String {
		var s = "";
		for (p in this.property) {
			s += "\t\t\t" + p.toString();
		}
		return s;
	}
	
	public function setMetadata(obj:Dynamic)
	{
		for (name in Reflect.fields(obj))
		{
			var opt:Dynamic = Reflect.field(obj, name);
			switch (name) {
				case "mongo_proxied":
					if (!Std.is(opt, MongoProxyType)) throw "'"+ opt +"' is not a MongoProxyType.\n[!] mongo_proxied requires MongoProxyType configuration value: none, singleType or typeCast.";
					this.settings.mongo_proxied = opt;
				
				case "_extends":
					if (!Std.is(opt,String)) throw Err_InvalidArgument("_extends", Std.string(opt), "expected a String");
					this.extend(Util.unpackPTypedef(Util.unpackMType( module.find(opt) )));
				
				case "_implements": try {
					if (!Std.is(opt,Array))
						opt = cast [opt];
					
					for (t in cast(opt,Array<Dynamic>))
					{
						if (Std.is(t,String))
							this.implement(Util.unpackPTypedef(Util.unpackMType( module.find(t) )));
						else if (Std.is(t,AbstractPType)) switch(cast(t,AbstractPType))
						{
							case UniqueID:
								this.implement(UniqueIDTrait.type);
							case is(type):
								this.implement(Util.unpackPTypedef(Util.unpackMType( module.find(type) )));
							
							default:
								throw Err_InvalidArgument("_implements", Std.string(opt), "expected Strings and/or Traits");
						}
						else
							throw "don't know how to implement: "+Std.string(t);
					}
				}
				catch(err:TypeDefinitionError) {
					switch(err) {
					 case Err_CannotExtendType(type):
						trace(this.fullName + " -> couldn't extend type: '"+type.name+"'");
					 case Err_CannotImplementType(type):
						trace(this.fullName + " -> couldn't implement type: '"+type.name+"'");
					 case Err_UndefinedType(name):
						trace(this.fullName + " -> couldn't implement type: '"+name+"' because it was not defined.");
					}
					trace(haxe.Stack.exceptionStack());
					throw err;
				}
			}
		}
	}
	
	private function propertyTypeFinalized(p:Property) {
		var t = Util.unpackPTypedef(Util.getPTypedef(p.type));
		return if (t != null) t.finalized else true;
	}
	
	public function finalize():Void
	{
		for (s in this.supertypes) if (!s.finalized) {
			this.finalized = false;
			break;
		}
		if (finalized) return;
		
		var member = this.module.getMember(this.name);
		
		switch (member) {
			case MPending(t,m,d,o): this.setMetadata(m);
			default:  //throw "impossible";
		}
		
		for (type in this.supertypes)
		{
			type.finalize();
			Assert.that(type.finalized);
			
			for (p in type.property)
			{	
				Assert.that(p.index >= 0, "Index < 0: " + p);
				
				try this.addProperty(p.copyFor(this))
				catch (err:PropertyDefinitionError) switch(err)
				{
					default: throw err;
					case Err_PropertyExists(newProp,current):
						current.copyOptions(newProp);
						Assert.that(current.index >= 0, "Copied index < 0: " + current);
				}
			}
		}
		
		switch (this.module.getMember(this.name)) {
			case MPending(t,m,d,o):
				this.setDefinition(d);
				this.setOptions(o);
			
			default: throw "impossible";
		}
		
		this.propTodo.tryToComplete();
		this.todo.tryToComplete();
		
		for (p in this.property) checkPropertyIsNumbered(p);
		
		finalized = true;
	}
	
	private function checkPropertyIsNumbered(p:Property)
	{
		if (!(p.index >= 0)) throw "\n\tProperty has no Index: " + p;
		if (p.index >= 30 && !p.hasOption(transient)) throw "\n\tNon transient Property requires index under 30 (but is "+p.index+" instead): " + p;
		
		if (p.definedIn == this) {
			if (this.propertyByNum.exists(p.index))
				Assert.that(this.propertyByNum.get(p.index) == p, "\n\tIndex of property:" + p + "\n\tis the same as: "+ this.propertyByNum.get(p.index));
			else
				this.propertyByNum.set(p.index, p);
		}
	}
	
	public function extend(type:TypeDefinition) : Void { throw "Abstract method"; }
	
	public function implement(type:TypeDefinition) : Void
	{
		if (!Std.is(type,BaseTypeDefinition)) throw Err_CannotImplementType(type);
		
		finalized = false;
		
		var btype:BaseTypeDefinition = cast type;
		this.supertypes.add(btype);
		btype.implementedBy.set(this.fullName, this);
	}
	
	public function setDefinition(obj:Dynamic)
	{
		for (name in Reflect.fields(obj))
		{
			var def = Reflect.field(obj, name);
			defineProperty(name, def);
		}
	}
	
	public function setOptions(options:Array<ClassOption>)
	{
		if (options == null) return;
		if (optionsTodo == null) optionsTodo = new TodoList();
		
		for (option in options) switch(option)
		{
			default:
				this.options.add(option);
			
			case xmlmap(map):
				var xmap = new XMLMapping();
				this.defaultXMLMap = xmap;
				optionsTodo.addSubtask(callback(xmap.apply, this, map));
				this.options.add(xmlmap(xmap));
		}
	}
	
	public function finalizeOptions()
	{
		if (this.settings.mongo_proxied == null)
			this.settings.mongo_proxied = this.implementedBy.iterator().hasNext()
											|| (Lambda.exists(this.property, function(p:Property) return p.hasOption(unique)) == false)
				? MongoProxyType.none
				: MongoProxyType.singleType;
		
		Assert.that(this.settings.mongo_proxied != null);
		
		if (this.optionsTodo != null)
			this.optionsTodo.tryToComplete();
	}
	
	public function defineProperty(name:String, def:Dynamic)
	{
		var p = property.get(name);
		if (p == null) {
			p = new Property(name, this);
			todo.addSubtask(function(){ /*trace("Defining new property: "+p.name);*/ p.type = PropertyTypeResolver.resolve(p, def); });
			addProperty(p);
		}
		else {
			if (todo.hasPending) todo.addSubtask(function(){ trace("Updating property: "+p.name); PropertyTypeResolver.update(p, def); });
			else PropertyTypeResolver.update(p, def);
		}
	}
	
	
	public function addProperty(p:Property)
	{
		if (property.exists(p.name))
			throw Err_PropertyExists(p, property.get(p.name));
		property.set(p.name, p);
	}
	
	public function findProperty(path:String) : Property {
		return doFindProperty(path.split('.'));
	}
	
	public function doFindProperty(node:Array<String>) : Property {
		var p = this.property.get(node[0]);
		if (p == null) throw Err_PropertyNotFound(node[0]);
		
		if (node.length == 1) return p;
		
		var nextPropType = p.type;
		
		var def:TypeDefinition;
		switch (nextPropType)
		{
			case Tdef(tdef):		def = Util.unpackPTypedef(tdef);
			
			case Tstring:			throw("TODO: implement binding to string properties");		throw Err_PropertyHasNoMembers;
			case Turi:				throw("TODO: implement binding to URI    properties");		throw Err_PropertyHasNoMembers;
			case Temail:			throw("TODO: implement binding to e-mail properties");		throw Err_PropertyHasNoMembers;
			case Tdate:				throw("TODO: implement binding to date properties");		throw Err_PropertyHasNoMembers;
			case Tdatetime:			throw("TODO: implement binding to datetime properties");	throw Err_PropertyHasNoMembers;
			case Tinterval:			throw("TODO: implement binding to interval properties");	throw Err_PropertyHasNoMembers;
			case Tarray(t,m,n):		throw("TODO: implement binding to array properties");		throw Err_PropertyHasNoMembers;
			
			
			case TenumConverter(e):	throw Err_PropertyHasNoMembers;
			case Tbool(t):			throw Err_PropertyHasNoMembers;
			case Tcolor:			throw Err_PropertyHasNoMembers;
			case Tdecimal(a,b,c):	throw Err_PropertyHasNoMembers;
			case Tinteger(a,b,c):	throw Err_PropertyHasNoMembers;
			case TuniqueID:			throw Err_PropertyHasNoMembers;
			case TfileRef:			throw Err_PropertyHasNoMembers;
			case TclassRef(a):		throw Err_PropertyHasNoMembers;
		}
		
		return def.doFindProperty(node.slice(1));
	}
}

class ClassDef extends BaseTypeDefinition
{
	public	var isMixin : Bool;
	public  var superClass (default, null) : Null<ClassDef>;
	private var subclasses : Null<List<ClassDef>>;
	
	public function new(index:Int, name:String, module:Module) {
		super(index,name,module);
	}
	
	override function getPropertiesSorted()
	{
		if (this.propertiesSorted != null) return propertiesSorted;
		
		var myProps = [];
		myProps = myProps.concat(getSuperProps(this));
		myProps = myProps.concat(getSortedProps(this));
		
		// Remove duplicate (by inheritance) properties
		var i = 0; //(superClass != null? superClass.propertiesSorted.length : 0);
		while (i < myProps.length)
		{
			var count = 0;
			var p = myProps[i];
			for (i2 in 0 ... myProps.length) if (p != null) {
				var p2 = myProps[i2];
				if (p2 != null && p.name == p2.name && p.definedIn == p2.definedIn && count++ > 0) {
					myProps[i2] = null;
					myProps.remove(null);
				}
			}
			
			++i;
		}
		
		return this.propertiesSorted = myProps;
	}
	
	function getSuperProps(type:BaseTypeDefinition)
	{
		var props = [];
		for (t in type.supertypes) {
			props = props.concat(getSuperProps(t));
			props = props.concat(getSortedProps(t));
		}
		
		return props;
	}
	
	function getSortedProps(type:TypeDefinitionWithProperties) {
		var props = [];
		for (p in this.property) if (p.definedIn == type) props.push(p);
		
		props.sort(function(a,b) return a.index - b.index); // return ArrayUtils.compareAlphabetically(a.name, b.name));
		return props;
	}
	
	public function extendsType(t:TypeDefinition) : Bool
	{
		var cl = this.superClass;
		while (cl != null) if (cl == t) return true; else cl = cl.superClass;
		return false;
	}
	
	public function implementsType (t:TypeDefinition) : Bool
	{
		for (type in this.supertypes)
			if (type == t)
				return true;
		
		return false;
	}
	
	override public function toString():String {
		return "<Class name='"+fullName+"'>";//"\n"+super.toString()+"\n\t</Class>";
	}
	
	override public function finalize():Void {
		if (superClass != null) {
		//	trace("Finalizing super class: "+superClass.fullName);
			superClass.finalize();
		}
		super.finalize();
	}
	
	override public function extend(type:TypeDefinition):Void {
		this.implement(type);
		var btype = supertypes.last();
		this.supertypes.remove(btype);
		this.supertypes.push(btype); // Add superclass to front of the list.
		
		if (Std.is(type,ClassDef)) this.superClass = cast type;
		else throw Err_CannotExtendType(type);
	}
	
	
	public function getRootSuperClass () : ClassDef
	{
		var superDef = superClass;
		while (superDef != null && superDef.superClass != null)
			superDef = superDef.superClass;
		
		return superDef;
	}
}

class MagicClassDef  extends ClassDef  {}

class NamedSetDef extends MagicClassDef
{
	private var pendingKeys	: Array<String>;
	private var pendingProps	: Dynamic;
	
	public var itemName : String;
	public var baseType	: PType;
	public var keys		: Array<{ path:String, prop:Property }>;
	
	public function new(index:Int, name:String, itemName:String, module:Module, baseType:PType) {
		Assert.that(baseType != null);
		
		super(index,name,module);
		this.itemName = itemName;
		this.keys = [];
		this.pendingKeys = [];
		this.pendingProps = {};
		this.baseType = baseType;
		this.finalized = false;
	}
	
	override public function setDefinition(v)	{}
	override public function setMetadata(v)	{}
	override public function setOptions(v)	{}
	
	public function setUniqueKeys(meta:Array<String>)	{ pendingKeys = meta.concat(pendingKeys); }
	public function setPropertyMap(map)				{ Util.overwriteProps(pendingProps, map); }
	
	public function getValueByPath(obj:Dynamic, keyPath:String)
	{
		var idx = keyPath.indexOf(".");
		if (idx > 0)
			return getValueByPath(Reflect.field(obj, keyPath.substr(0, idx)), keyPath.substr(idx + 1));
		
		return Reflect.field(obj, keyPath);
	}
	
	override public function finalize() : Void
	{
		var btype = Util.unpackPTypedef(Util.getPTypedef(baseType));
		if (btype != null) {
			btype.finalize();
			Assert.that(btype.finalized);
		}
		if (this.finalized) return; // already done
		
		for (field in Reflect.fields(pendingProps))
		{
			var p = new Property(field, this);
			p.type = baseType;
			
			var def:Array<Dynamic> = Reflect.field(pendingProps, field);
			if (Std.is(def, Int)) def = [def];
			
			Assert.that(Std.is(def,Array), "NamedSet '"+ fullName +"' Not an array: "+def);
			
			var valueMap = null;
			for (v in def) if (Std.is(v, Int)) {
				p.index = Std.int(v);
			} else {
				Assert.that(valueMap == null);
				valueMap = v;
			}
			
			if (valueMap != null && Reflect.fields(valueMap).length > 0) try p.setDefaultValue(valueMap) catch(err:ResolveError) switch(err) {
				case Err_PropertyNotFound(name):
					throw "Property '"+name+"' for NamedSet "+this.fullName+" does not exist in basetype: "+baseType + "in map: "+ valueMap;
				default: throw err;
			}
			
			this.property.set(field, p);
		}
		
		if (Std.is(btype, TypeDefinitionWithProperties)) {
			for (key in pendingKeys) keys.push( { path: key, prop: btype.findProperty(key) } );
		}
		else if (pendingKeys.length > 0) throw "NamedSet keys set for a type with no properties: " + baseType;
		
		this.pendingProps = null;
		this.pendingKeys  = null;
		this.finalized    = true;
	}
	
	
	override public function toString():String {
		return "<NamedSet name='"+fullName+"' basetype='"+Std.string(baseType)+"' keys='"+Std.string(keys)+"'>\n"+super.toString()+"\n\t</NamedSet>";
	}
}

class UniqueIDTrait extends MagicClassDef
{
	public static var type (default,null) : UniqueIDTrait;
	
	static var init = function(){ type = new UniqueIDTrait(); }();
	
	private function new() {
		super(0x1D, "ObjectId", Module.traits);
		Module.types.set(0x1D, this);
		
		var p = new Property("id", this);
		p.index = 0;
		p.type  = TuniqueID;
		p.opts  = [unique];
		this.property.set(p.name, p);
		
		this.isMixin   = true;
		this.finalized = true;
//		untyped Module.traits.members.set(name, MType(Tclass(this)));
	}
}

// ---
enum PTypedef
{
	Tclass		(def:ClassDef);
	Tenum		(def:EnumDef);
}

enum PType
{	
	Tdef			(type:PTypedef);
	Tarray			(type:PType,	?min:Int,	?max:Int);
	
	Tbool			(val:Bool);
	Tinteger		(?min:Float,	?max:Float,	?stride:Int);
	Tdecimal		(?min:Float,	?max:Float,	?stride:Float);
	Tdate;
	Tdatetime;
	Tinterval;
	Tcolor;
	
	Tstring;
	Turi;
	Temail;
	TclassRef		(ref:String);
	
	TuniqueID;
	TfileRef;
	
	TenumConverter	(prop:EnumConversionProperty);
}

// ----------------
// -- END PARSER --
// ----------------

enum AbstractPType
{
	arrayOfType(type:PType);
	arrayOf(cl:String);
	namedSetOf(basetype:AbstractPType, index:Int, uniqueKeys:Array<String>, properties:Dynamic);
	
	is(path:String);
	subclass(index:Int, options:Dynamic, properties:Dynamic);
	
	// numeric
	decimal;
	integer;
	color;
	
	// string
	string;
	URI;
	EMail;
	ClassRef(ref:String);		//ref contains full classpath to the requested class
	
	// special data
	date;
	datetime;
	interval;
	
	// traits
	UniqueID;
	FileRef;
	
	//true = non-nullable bool default true
	//false = non-nullable bool default false
}


enum PropertyOption
{
	bindable;
	required;
	unique;
	transient;
	mongo_reference;
	mongo_typeCast;
	optional;
}

enum Platform
{
	js;
	flash9;
	flash;
	scala;
}

enum ClassOption
{
	editable;
	xmlmap(mapping:Dynamic);
}

enum SpecialValue
{
	staticurl(uri:String);
}

enum XMLMapDefinition
{
	emptyNode;
	attributes(map:Dynamic);
	childNode(property:String);
}

enum MappingType
{
	MapXML;
}

enum MongoProxyType
{
	/** Don't generate a Proxy */
	none;
	/** Generate one Proxy that can contain this VO and all subclasses using a hidden 'cast' property with the type number */
	typeCast;
	/** Generate a proxy which only takes this VO into account */
	singleType;
}
