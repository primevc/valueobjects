package primevc.tools.valueobjects;
 import primevc.tools.valueobjects.VODefinition;
 import primevc.tools.valueobjects.output.HTML;
 import Type;

class Specs
{
	static function main()
	{
		var r = new hxspec.SpecRunner();
		
		r.add(new specs.ArrayUtilsSpecs()); // TODO: Move to PrimeVC project
		r.add(new org.valueobjects.specs.VODefinitionSpecs());
		r.add(new org.valueobjects.specs.output.HaxeSpecs());
		
		var ok = r.run();
	}
}
