package primevc.core.traits;
 import haxe.io.BytesOutput;
 import primevc.core.traits.IMessagePackable;
 import primevc.core.traits.IEditEnabledValueObject;

interface IObjectId implements IEditEnabledValueObject implements IMessagePackable
{
	public var id	(default,null) : primevc.types.ObjectId;
}
