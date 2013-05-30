package prime.core.traits;
 import haxe.io.BytesOutput;
 import prime.core.traits.IMessagePackable;
 import prime.core.traits.IEditEnabledValueObject;

interface IObjectId implements IEditEnabledValueObject implements IMessagePackable
{
	public var id	(default,null) : prime.types.ObjectId;
}
