package prime.tools.valueobjects.output;
 import prime.tools.valueobjects.VODefinition;
 import prime.utils.NumberUtil;
  using prime.utils.TypeUtil;
  using prime.tools.valueobjects.output.ScalaUtil;
  using prime.tools.valueobjects.VODefinition;

#if haxe3
private typedef IntHash<T> = Map<Int,T>;
#end

class ClojureDefVO implements CodeGenerator
{
	var file : sys.io.FileOutput;
	var map  : Map<Int,Bool>;

	public function new(file : sys.io.FileOutput) {
		map       = new Map();
		this.file = file;
	}

	public function genClass(def : ClassDef)
	{
		// Generate all supertypes first
		for (t in def.supertypes) if (t.is(ClassDef)) {
			var c : ClassDef = cast t;
			if (!c.isMixin) genClass(c);
		}
		if (!def.isMixin && !map.exists(def.index))
		{
			// Prevent repeated defs
			map.set(def.index, true);
			// Make sure any types we use are initialized first
			for (p in def.propertiesSorted) {
				var type = switch (p.type) {
					case Tarray(ptype,_,_): Util.unpackPTypedef(Util.getPTypedef(p.type));
					case Tdef(ptypedef): Util.unpackPTypedef(ptypedef);
					default: null;
				}
				if (type.is(ClassDef)) genClass(cast type);
			}
			// Finally write our own def
			file.writeString("\n(defvo ");
			file.writeString(def.fullName);
			for (p in def.propertiesSorted) file.writeString(" :" + p.name);
			file.writeString(")");
			// Write ValueObjectManifest initialization hack, to work around scala class loading issues
			if (def.subclasses.isEmpty()) writeManifestInitHack(def);
		}
	}
	private function writeManifestInitHack(def : ClassDef)
	{
		file.writeString("\n(. (");
		file.writeString(def.module.fullName);
		file.writeString("/");
		file.writeString(def.name);
		file.writeString(" {}) voManifest)");

		/* Handle cases like this:
			Box
				WhiteBox
				BlackBox
					CoolBox
						AwesomeBox

			BlackBox and CoolBox should be constructed as well,
			or it will not be included in Box.manifest.subtypes at runtime.
		*/
		if (def.superClass != null && def.superClass.superClass != null)
			writeManifestInitHack(def.superClass);
	}


	public function genEnum(def:EnumDef) {}

	public function newModule(module:Module) : CodeGenerator {
		return this;
	}


	public static function generate() for (m in Module.pkgRoots)
	{
		var parts = m.fullName.split(".");
		var filename = parts.pop();
		var dir = null;
		for (p in parts) {
			if (dir != null) dir += "/" + p; else dir = p;
			try sys.FileSystem.createDirectory(dir);
		}

		filename = (dir == null? "" : dir + "/") + filename + ".clj";
		trace("WRITING: "+filename);
		
		var file = sys.io.File.write(filename, false);
			file.writeString("(ns ");
			file.writeString(m.fullName);
			file.writeString("\n  (:use prime.vo.definition))\n");

		var clj = new ClojureDefVO(file);
		m.generateWith(clj);
		
		file.close();
	}
}
