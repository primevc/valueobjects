/*
 * Copyright (c) 2010, The PrimeVC Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *	 notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *	 notice, this list of conditions and the following disclaimer in the
 *	 documentation and/or other materials provided with the distribution.
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
 * DAMAGE.s
 *
 *
 * Authors:
 *  Danny Wilson	<danny @ prime.vc>
 */
package primevc.core.net;
 import primevc.core.events.CommunicationEvents;
 import primevc.core.dispatcher.Signals;
 import primevc.core.dispatcher.Signal0;
 import primevc.core.dispatcher.Signal1;
 import primevc.core.dispatcher.Wire;
 import primevc.core.traits.IDisposable;
 import primevc.core.traits.IMessagePackable;
 import primevc.core.net.URLLoader;
 import primevc.types.URI;
  using primevc.utils.Bind;



/**
 * Service class to simplify communication with a REST Resource that accepts 
 * MessagePack data.
 * 
 * @author	Danny Wilson
 * @since	Jan 20, 2011
 */  
class MessagePackResource <Data> implements IDisposable
{
	public var events		(default, null) : DataServiceEvents <Data>;
	public var bytesSending (default, null) : Int;
	public var data			(default, null) : Data;
	public var uriPrefix	: URI;
	
	private var reader		: primevc.utils.msgpack.Reader;
	private var loader		: URLLoader;
	private var bytes		: haxe.io.Bytes;
	private var typeMap		: IntHash<Class<Dynamic>>;
	private var onComplete  : Wire<Void   -> Void>;
	private var onError		: Wire<String -> Void>;
		

	public function new(uriPrefix : URI, typeMap : IntHash<Class<Dynamic>>)
	{
		this.typeMap   = typeMap;
		this.uriPrefix = uriPrefix;
		Assert.notNull(typeMap);
		
		reader = new primevc.utils.msgpack.Reader(typeMap);
		loader = new URLLoader();
//#if flash9
//		loader.dataFormat = flash.net.URLLoaderDataFormat.BINARY;
//#end
		
		var load	= loader.events.load;
		onComplete	= load.completed.bind( this, doNothing );
		onError		= load.error.observe( this, doNothing );
		handleStatus.on( loader.events.httpStatus, this );
		events		= new DataServiceEvents(load.progress);
	}


	public function dispose ()
	{
		events.dispose();
		reader.dispose();
		loader.dispose();
		
		onComplete.dispose();
		onError.dispose();
		
		onError		= null;
		onComplete	= null;
		typeMap		= null;
		uriPrefix	= null;
		bytes		= null;
		loader		= null;
		reader		= null;
		events		= null;
	}
	

	private function doNothing () throw "impossible"
	

	/**
	 * GET a single object by proving a uriSuffix.
	 */
	public function get (uriSuffix:String)
	{
		Assert.notNull(uriPrefix);
		var l   = loader,
			e   = events.receive,
			uri = uriSuffix == null? uriPrefix : new URI(uriPrefix.string + uriSuffix);
		
		onComplete.handler	= handleGET;
		onError.handler		= cast (events.receive.error, Signal1<Dynamic>).send;
		
		trace("get "+uri);
		l.load(uri);
		e.started.send();
	}
	

	/**
	 * Serialize and POST an object to the Resource. 
	 * @param uriSuffix Required to prevent accidental overwriting of an entire resource.
	 */
	public function send (uriSuffix:String, obj:IMessagePackable)
	{
		Assert.notNull(uriPrefix);
		var l   = loader,
			e   = events.send,
			uri = uriSuffix == null? uriPrefix : new URI(uriPrefix.string + uriSuffix);
		
		
		var bytes		= serialize(obj);
		bytesSending	= bytes.length;
		
		// Send
		onComplete.handler	= handlePOST;
		onError.handler		= cast(events.send.error, Signal1<Dynamic>).send;
		
		l.binaryPOST(uri, bytes);
		e.started.send();
	}
	

	private function handleGET ()
	{
		trace(loader.bytesLoaded+" / "+loader.bytesTotal);
		
		var bytes	= haxe.io.Bytes.ofData(loader.data);
		var input	= reader.input = new haxe.io.BytesInput(bytes);
		input.bigEndian = true;
		
		data = reader.readMsgPackValue();
		events.receive.completed.send();
	}
	

	private function handlePOST()
	{
		trace(loader.bytesLoaded+" / "+loader.bytesTotal);
		bytesSending = 0;
		events.send.completed.send();
	}


	private function handleStatus(status:Int)
	{
		trace(status+" => "+loader.bytesLoaded+" / "+loader.bytesTotal);
	}
	
	
	public static inline function serialize (obj:IMessagePackable) : haxe.io.Bytes
	{
		// Serialize
		var out			= new haxe.io.BytesOutput();
		out.bigEndian	= true;
		var origLen		= obj.messagePack(out);
		var b			= out.getBytes();
		Assert.equal(b.length, origLen);
		return b;
	}
}




/**
 * @author	Danny Wilson
 * @since	Jan 20, 2011
 */
class MessagePackResourceSignals extends CommunicationSignals
{
	public function new (progessSignal)
	{
		this.progress	= progessSignal;
		this.started	= new Signal0();
		this.completed	= new Signal0();
		this.error		= new Signal1<String>();
	}
}




/**
 * @author	Danny Wilson
 * @since	Jan 20, 2011
 */
class DataServiceEvents <Data> extends Signals
{
	var receive	(default, null) : CommunicationSignals;
	var send	(default, null) : CommunicationSignals;
	

	public function new (progessSignal)
	{
		receive = new MessagePackResourceSignals(progessSignal);
		send	= new MessagePackResourceSignals(progessSignal);
	}
}
