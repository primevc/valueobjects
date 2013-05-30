package prime.tools.valueobjects;
 import prime.tools.valueobjects.VODefinition;
 import prime.tools.valueobjects.output.HTML;
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
