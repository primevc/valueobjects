package primevc.tools.valueobjects;
 import primevc.core.collections.ListChange;
 import primevc.core.traits.IEditableValueObject;
 import primevc.core.traits.IValueObject;
 import primevc.core.collections.RevertableArrayList;
 import primevc.core.dispatcher.Signal1;
 import primevc.core.RevertableBindable;
 import primevc.utils.FastArray;
  using primevc.utils.IfUtil;
  using primevc.utils.TypeUtil;

/**
 * Class description
 * 
 * @author Danny Wilson
 * @creation-date Dec 03, 2010
 */
class ValueObjectBase implements IValueObject
{
	public var change (default, null) : Signal1<ObjectChangeSet>;
	
	private var _changedFlags	: Int;
	private var _propertiesSet	: Int;
	
	private function new ()
	{
		change = new Signal1();
	}
	
	
	public function dispose()
	{
		if (change.notNull()) {
			change.dispose();
			change = null;
		}
		_changedFlags = 0;
	}
	
	public function isEmpty() {
		return _propertiesSet == 0;
	}
	
	public function commitEdit()
	{
		if (_changedFlags.not0())
		{
			var set = ObjectChangeSet.make(this, _changedFlags);
			addChanges(set);
			this.change.send(set);
		}
		commitBindables();
	}
	
	private function addChanges(changeSet:ObjectChangeSet); // Creates and adds all PropertyChangeVO and ListChangeVO
	private function commitBindables();
	public function beginEdit();
	public function cancelEdit();
	
/*
	Kijken wat kleinere SWF geeft: calls hiernaar, of methods genereren...
	
	private static function propertyChangeHandler<T>(instance:ValueObjectBase, propertyBit : Int) : Void -> Void
	{
		return function() {
			instance._changedFlags |= propertyBit;
		}
	}
*/
	
	
	static inline public function bytesUsedInInt(n:Int)
	{
		return if (n <= 0x0000FF)	1;
		  else if (n <= 0x00FFFF)	2;
		  else if (n <= 0xFFFFFF)	3;
		  else 						4;
	}
}

class ChangeVO extends primevc.core.ListNode<PropertyChangeVO>, implements IValueObject
{
	public function dispose()
	{
		if (this.n.notNull()) {
			this.n.dispose();
			this.n = null;
		}
	}
}

class PropertyChangeVO extends ChangeVO
{
	public var propertyID	(default, null) : Int;
}

class PropertyValueChangeVO extends PropertyChangeVO
{
	public var oldValue		(default, null) : Dynamic;
	public var newValue		(default, null) : Dynamic;
	
	private function new();
	
	static inline public function make(propertyID, oldValue, newValue)
	{
		var p = new PropertyValueChangeVO(); // Could come from freelist if profiling tells us to
		p.propertyID = propertyID;
		p.oldValue   = oldValue;
		p.newValue   = newValue;
		return p;
	}
	
	override public function dispose() {
		propertyID = -1;
		this.oldValue = this.newValue = null;
		super.dispose();
	}
}

class ListChangeVO extends PropertyChangeVO
{
	public var changes : FastArray<ListChange<Dynamic>>;
	
	private function new();
	
	static inline public function make(propertyID, changes : FastArray<ListChange<Dynamic>>)
	{
		var l = new ListChangeVO(); // Could come from freelist if profiling tells us to
		l.propertyID = propertyID;
		l.changes = changes.concat();
		return l;
	}
	
	override public function dispose()
	{
		if (this.changes.notNull()) {
			for (i in 0 ... this.changes.length) changes[i] = null;
			this.changes = null;
		}
		super.dispose();
	}
}
/*
ObjectChangeVO {
  vo : instanceof PublicationVO
  id : "pub1"
  propertiesChanged: SPREAD
  
  next: ObjectChangeVO {
    vo: instanceof SpreadVO
    id: "spread1"
    propertiesChanged: (bits)[ X, Y ]
    next: PropertyChangeVO { propertyID: X, oldValue: 0, newValue: 100,
      next: PropertyChangeVO { propertyID: Y, oldValue: 0, newValue: 100 }
    }
  }
}
*/

class ObjectChangeSet extends ChangeVO
{
	static public function objectChangedHandler(changeSignal:Signal1<ObjectChangeSet>, propertyID : Int) : ObjectChangeSet -> Void
	{
		var pathNode = ObjectPathVO.make(this, propertyID); // Same ObjectPathVO instance reused
		
		return function(change:ObjectChangeSet)
		{
			var p = change.parent;
			
			// Find either pathNode, or the last parent
			while (p.notNull() && p.parent.notNull() && p.parent != pathNode) p = p.parent;
			
			untyped p.parent = pathNode;
			changeSignal.send(change);
		}
	}
	
	
	public var vo					(default, null) : IValueObject;
	public var parent				(default, null) : ObjectPathVO;
	public var timestamp			(default, null) : Float;
	public var propertiesChanged	(default, null) : Int;
	
	private function new(); 
	
	static inline public function make(vo:ValueObjectBase, changes:Int)
	{
		var s = new ObjectChangeSet(); // Could come from freelist if profiling tells us to
		s.vo = vo;
		s.timestamp = haxe.Timer.stamp();
		s.propertiesChanged = changes;
		return s;
	}
	
	public function add(change:PropertyChangeVO) {
		untyped change.n = this.n;
		n = change;
	}
	
	inline public function addChange(id:Int, flagBit:Int, value:Dynamic)
	{
		if (flagBit.not0())
			add(PropertyValueChangeVO.make(id, null, value));
	}
	
	inline public function addBindableChange(id:Int, flagBit:Int, bindable:RevertableBindable<Dynamic>)
	{
		if (flagBit.not0())
			add(PropertyValueChangeVO.make(id, bindable.shadowValue, bindable.value));
	}
	
	inline public function addListChanges(id:Int, flagBit:Int, list:RevertableArrayList<Dynamic>)
	{
		if (flagBit.not0())
			add(ListChangeVO.make(id, list.changes));
	}
}

class ObjectPathVO implements IValueObject
{	
	public var parent		(default, null) : ObjectPathVO;
	public var object		(default, null) : IValueObject;
	public var propertyID	(default, null) : Int;
	
	private function new(); 
	
	public function dispose()
	{
		this.parent = null;
		this.object = null;
	}
	
	static inline public function make(vo:ValueObjectBase, propertyID:Int)
	{
		var p = new ObjectPathVO();
		p.object = vo;
		p.propertyID = propertyID;
		return p;
	}
}
