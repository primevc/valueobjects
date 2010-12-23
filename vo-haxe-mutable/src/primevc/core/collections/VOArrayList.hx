/*
 * Copyright (c) 2010, The PrimeVC Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE PRIMEVC PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE PRIMVC PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 *
 * Authors:
 *  Danny Wilson	<danny @ prime.vc>
 */
package primevc.core.collections;
 import primevc.core.collections.RevertableArrayList;
 import primevc.core.dispatcher.Signal1;
 import primevc.core.traits.IValueObject;
 import primevc.tools.valueobjects.ValueObjectBase;
 import primevc.utils.FastArray;
  using primevc.utils.IfUtil;

/**
 * A specialized RevertableArrayList for ValueObjects.
 * 
 * Used to support bubbling of ObjectChangeSets.
 * 
 * @author Danny Wilson
 * @creation-date Dec 20, 2010
 */
class VOArrayList<DataType : IValueObject> extends ArrayList<DataType> #if GenericArrays, implements haxe.rtti.Generic #end
{
	private var changeHandlerFn : ObjectChangeSet -> Void;
	public  var itemChange : Signal1<ObjectChangeSet>;
	
	public function new ( wrapAroundList:FastArray<#if GenericArrays DataType #else Dynamic #end> = null )
	{
		super(untyped wrapAroundList);
		itemChange = new Signal1();
	}
	
	override public function dispose()
	{
		super.dispose();
		
		if (itemChange.notNull()) {
			itemChange.dispose();
			itemChange = null;
		}
	}
	
	
	public function setChangeHandler(changeHandler : ObjectChangeSet -> Void)
	{
		itemChange.dispose();
		this.changeHandlerFn = changeHandler;
		VOArrayListUtil.setChangeHandler(this, #if !GenericArrays untyped #end list, changeHandler);
	}
	
	
	override public function add (item:DataType, pos:Int = -1) : DataType
	{
		super.add(item);
		cast(item, ValueObjectBase).change.bind(this, changeHandlerFn);
		
		return item;
	}
	
	
	override public function remove (item:DataType, oldPos:Int = -1) : DataType
	{
		super.remove(item);
		cast(item, ValueObjectBase).change.unbind(this);
		
		return item;
	}
	
	
	override public function clone () : IReadOnlyList<DataType>
	{
		return untyped new VOArrayList<DataType>( #if !GenericArrays untyped #end list.concat() /* copy */ );
	}
}

class VOArrayListUtil
{
	static inline public function setChangeHandler<T>(owner:Dynamic, list:FastArray<T>, changeHandler : ObjectChangeSet -> Void)
	{
		if (changeHandler.notNull()) {
			for (i in 0 ... list.length)
			 	cast(list[i], ValueObjectBase).change.bind(owner, changeHandler);
		} else {
			for (i in 0 ... list.length)
			 	cast(list[i], ValueObjectBase).change.unbind(owner);
		}
	}
}
