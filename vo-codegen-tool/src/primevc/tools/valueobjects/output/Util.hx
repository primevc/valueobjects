package primevc.tools.valueobjects.output;

class Util
{
	static public inline function addCapitalized(b:StringBuf, s:String):StringBuf
	{
		b.add(s.substr(0,1).toUpperCase());
		b.addSub(s, 1);
		return b;
	}
}
