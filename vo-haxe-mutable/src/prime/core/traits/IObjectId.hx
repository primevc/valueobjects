package prime.core.traits;

interface IObjectId extends IEditEnabledValueObject extends IMessagePackable
{
	public var id	(default,null) : prime.types.ObjectId;
}
