package ;
import prime.tools.valueobjects.VODefinition;
import prime.tools.valueobjects.output.HTML;
import prime.tools.valueobjects.output.Haxe;
import prime.tools.valueobjects.output.Scala;
import prime.tools.valueobjects.output.Clojure;
using Sys;
using sys.FileSystem;

// --
class TestValueObjects  { static public function define() {
// --

var m = Module.declare('prime.test.vo', true);

m.mixin(0, "Point", {
  x:              [0, integer, bindable],
  y:              [1, integer, bindable]
});
var Point = "prime.test.vo.Point";

m.mixin(1, "Sized", {
  width:          [0, integer, bindable],
  height:         [1, integer, bindable]
});
var Sized = "prime.test.vo.Sized";



//
// For unit testing:
//
m._class(250, "DatedAction", {}, {
  id: [0, integer, unique],
  at: [1, datetime],
});

m._class(251, "ValueObjectSized", {_implements: [UniqueID, Sized]}, {});

m._class(252, "ValueObjectTestMeta", {_implements: [UniqueID]}, {
  created: [0, is(".DatedAction")],
  tags:    [1, arrayOfType(Tstring)],
  name:    [2, string],
  o:       [3, is(".ValueObjectSized"), reference],
});

m._class(253, "ValueObjectTest", {_implements: UniqueID}, {
  a:       [ 0, string],
  b:       [ 1, integer],
  c:       [ 2, color],
  d:       [ 3, decimal],
  e:       [ 4, EMail],
  f:       [ 5, FileRef],
  g:       [ 6, URI],
  h:       [ 7, datetime],
  i:       [ 8, interval],
  j:       [ 9, date],
  k:       [10, UniqueID],
  l:       [11, arrayOf(".ValueObjectTest"), reference],
  m:       [12, arrayOfType(Tstring)],
  n:       [13, is(".ValueObjectSized")],
  o:       [14, is(".ValueObjectTest"), reference],
  p:       [15, false],
  name:    [16, string],
  owner:   [17, is(".VOTestRef")],
  meta:    [18, is(".ValueObjectTestMeta")],
});

m._class(254, "ValueObjectExtendsTest", {_extends: ".ValueObjectTest", _implements: [Sized, Point]}, {
  q:       [0, integer],
  r:       [1, is(".ValueObjectExtendsTest")],
  af:      [2, arrayOfType(TfileRef)],
});

m._class(255, "VOTestRef", {}, {
  ref:     [0, is(".ValueObjectTest"), reference],
  name:    [1, string],
  a:       [2, arrayOf(".ValueObjectTestMeta")],
  b:       [3, integer],
});

m._class(256, "IntIDObjectTest", {}, {
  id:      [0, integer, unique],
  name:    [1, string],
  a:       [2, string],
  members: [3, arrayOf(".IntIDObjectTest"), reference],
  objects: [4, arrayOf(".ValueObjectTestMeta")],
  tags:    [5, arrayOfType(Tstring)],
  objrefs: [6, arrayOf(".ValueObjectTest"), reference],
});


} // end VODefinition

static public function main() {
  define();
  Module.root.finalize();
  HTML.generate("test-valueobjects.html");

  "generated-src/haxe".setCwd();
  Haxe.generate();

  "../scala".setCwd();
  Scala.generate();

  "../clojure".setCwd();
  ClojureDefVO.generate();
}

// --
}
// --