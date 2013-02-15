package prime.tools.valueobjects.output;
 import prime.tools.valueobjects.VODefinition;
 import prime.utils.NumberUtil;
  using prime.utils.TypeUtil;
  using prime.tools.valueobjects.output.ScalaUtil;
  using prime.tools.valueobjects.VODefinition;

class ClojureDefVO implements CodeGenerator
{
	var file : sys.io.FileOutput;
	var map  : IntHash<Bool>;

	public function new(file : sys.io.FileOutput) {
		map       = new IntHash();
		this.file = file;
	}

	public function genClass(def : ClassDef)
	{
		if (!def.isMixin && !map.exists(def.index)) {
			map.set(def.index, true);
			file.writeString("\n(defvo ");
			file.writeString(def.fullName);
			for (p in def.propertiesSorted) file.writeString(" :" + p.name);
			file.writeString(")");
		}
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
