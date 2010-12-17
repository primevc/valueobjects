package primevc.types;
 import haxe.io.BytesOutput;
 import primevc.core.traits.IMessagePackable;
 import primevc.core.traits.IEditEnabledValueObject;

interface IUniqueID implements IEditEnabledValueObject, implements IMessagePackable
{
	public var id	(default,null) : primevc.types.UniqueID;
}
