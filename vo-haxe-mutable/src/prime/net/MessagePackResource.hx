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
package prime.net;
 import haxe.io.Bytes;
 import haxe.io.BytesData;
 import haxe.io.BytesInput;
 import haxe.io.BytesOutput;

 import prime.core.events.CommunicationEvents;
 import prime.core.traits.IMessagePackable;
 import prime.net.URLLoader;
 import prime.signals.Signals;
 import prime.signals.Signal0;
 import prime.signals.Signal1;
 import prime.signals.Wire;
 import prime.types.URI;
 import prime.utils.msgpack.Reader;
  using prime.utils.Bind;
  using prime.utils.TypeUtil;
  using prime.net.HttpStatusCodes;



/**
 * Service class to simplify communication with a REST Resource that accepts 
 * MessagePack data.
 * 
 * @author	Danny Wilson
 * @since	Jan 20, 2011
 */  
class MessagePackResource <Data> implements prime.core.traits.IDisposable
{
	public var events			(default, null) : DataServiceEvents;
	public var bytesSending 	(default, null) : Int;
	public var data				(default, null) : Data;
	public var uriPrefix		: URI;
	
	public var loader			(default, null) : URLLoader;
	
	/**
	 * Cached value of the last returned http-status-code
	 */
	public var lastHttpStatus	(default, null)	: Int;
	
	private var reader			: Reader;
	private var bytes			: Bytes;
	private var typeMap			: Map<Int,Class<Dynamic>>;
	private var onComplete  	: Wire<Void   -> Void>;
	private var onError			: Wire<String -> Void>;
		

	public function new(uriPrefix : URI, typeMap : Map<Int,Class<Dynamic>>)
	{
		this.typeMap   = typeMap;
		this.uriPrefix = uriPrefix;
		Assert.isNotNull(typeMap);
		
		reader = new prime.utils.msgpack.Reader(typeMap);
		loader = new URLLoader();

		var load	= loader.events.load;
		onComplete	= load.completed.bind( this, doNothing );
		onError		= load.error.observe( this, doNothing );
		
		cacheStatus.on( loader.events.httpStatus, this );
#if debug
		handleError.on( loader.events.load.error, this );
#end
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
	

	private function doNothing () throw "impossible " + uriPrefix;


#if debug
	private var getStarted : Float;
#end


	/**
	 * GET a single object by proving a uriSuffix.
	 */
	public function request (uriSuffix:String = null, method:RequestMethod = null)
	{
		Assert.isNotNull(uriPrefix);
		var l   = loader,
			e   = events.receive,
			uri = uriSuffix == null? uriPrefix : new URI(uriPrefix.string + uriSuffix);
		
		onComplete.handler = handleGET;
		onError   .handler = events.receive.error.send;
		
#if debug getStarted = prime.utils.TimerUtil.stamp(); #end
		l.requestBinary(uri, method);
		e.started.send();
	}
	

	/**
	 * Serialize and POST an object to the Resource. 
	 * @param uriSuffix Required to prevent accidental overwriting of an entire resource.
	 */
	public inline function send (uriSuffix:String, obj:IMessagePackable)
		sendBytes( uriSuffix, serialize(obj).getData() );
	

	
	public function sendBytes (uriSuffix:String, bytes:BytesData)
	{
		Assert.isNotNull(uriPrefix);
		var l   = loader,
			e   = events.send,
			uri = uriSuffix == null ? uriPrefix : new URI(uriPrefix.string + uriSuffix);
		
		l.bytes		 		= bytes;
		bytesSending 		= l.bytesTotal;
		// Send
		onComplete.handler  = handlePOST;
		onError   .handler  = cast(events.send.error, Signal1<Dynamic>).send;
#if debug getStarted 		= prime.utils.TimerUtil.stamp(); #end
		l.sendBinary(uri);
		e.started.send();
	}

	

	private function handleGET ()
	{
	//	trace(loader.bytesProgress+" / "+loader.bytesTotal+" [ "+uriPrefix+" ]");
#if debug
		trace("received data in "+(prime.utils.TimerUtil.stamp() - getStarted) + " ms; bytes: "+loader.bytesTotal);
#end
		data = deserialize( loader.data, reader );
		events.receive.completed.send();
		
		onComplete.setVoidHandler(doNothing);
		onError.setVoidHandler(doNothing);
	}
	

	private function handlePOST()
	{
#if debug
		trace("posted data in "+(prime.utils.TimerUtil.stamp() - getStarted) + " ms; bytes: "+loader.bytesTotal);
#end
	//	trace(loader.bytesProgress+" / "+loader.bytesTotal+" [ "+uriPrefix+" ]");
		bytesSending = 0;
		events.send.completed.send();
		
		onComplete.setVoidHandler(doNothing);
		onError.setVoidHandler(doNothing);
	}

	
	private function cacheStatus(status:Int)
	{
		lastHttpStatus = status;
		if (status != 200)
			trace(status.read()+" => "+loader.bytesProgress+" / "+loader.bytesTotal+" [ "+uriPrefix+" ]");
	}
	
	
#if debug
	private function handleError(e:String)		{ trace(e+"; [ "+uriPrefix+" ]"); }
#end
	
	
	public static inline function serialize (obj:IMessagePackable) : Bytes
	{
		// Serialize
#if debug
		var start = prime.utils.TimerUtil.stamp();
#end
		var out			= new BytesOutput();
		out.bigEndian	= true;
		var origLen		= obj.messagePack(out);
		var b			= out.getBytes();

#if debug
		Assert.isEqual(b.length, origLen);
		trace("serialized data in "+(prime.utils.TimerUtil.stamp() - start)+" ms");
#end
		return b;
	}
	
	
	
	public static inline function deserialize<Data> (data:BytesData, reader:Reader) : Data
	{
#if debug
		var start = prime.utils.TimerUtil.stamp();
#end
//		var bytes	= Bytes.ofData(data);
	
#if flash10
		data.endian	 = flash.utils.Endian.BIG_ENDIAN;
		reader.bytes = data;
#else
		reader.input = #if js data.is(String)? new ByteStringInput(data).as(haxe.io.Input) : #end new BytesInput(Bytes.ofData(data));
		reader.input.bigEndian = true;
#end
		
#if debug
		var o = reader.readMsgPackValue();
		trace("deserialized data in "+(prime.utils.TimerUtil.stamp() - start)+" ms");
		return o;
#else
		return reader.readMsgPackValue();
#end
	}

	
	/**
	 * Method will the error send by the server
	 */
	public function getErrorResponse ()
	{
		var error		= Std.string( loader.getRawData() );
		var beginBody	= error.indexOf( "<body>" ) + 6;			//add 6 for the 6 characters of <body>
		var endBody		= error.indexOf( "</body>", beginBody );
		return error.substr( beginBody, endBody - beginBody );
	}
	
	
/*	public static inline function serializeToString (obj:IMessagePackable) : String
	{
		var b = serialize(obj);
		var d = b.getData();
		var o = new StringBuf();
		d.position = 0;
		
		for (i in 0...b.length)
			o.addChar(d.readByte() & 0xFF);
		
	/*	// Test to make sure the serialized string contains the same bytes after deserializing again as the original data
		var d2 = stringToBytes( o.toString() );
		Assert.isEqual(d.length, d2.length);
		
		trace(o.toString());
		d.position = 0;
		d2.position = 0;
		for (i in 0...b.length) {
			var j1 = d.readByte();
			var j2 = d2.readByte();
			trace(StringTools.hex(j1, 2)+" / "+StringTools.hex(j1, 2));
			Assert.isEqual( j1, j2 );
		} //*/
		
/*		return o.toString();
	}*/
	
	
/*	public static inline function toBytes (input:String) : BytesData
	{
		var d = new BytesData();
	//	var s2 = haxe.Timer.stamp();
		d.writeUTFBytes(input);
		
	//	trace(prime.utils.BytesUtil.toHex(d));
	//	s2 = haxe.Timer.stamp() - s2;
		return d;
	}*/
}



#if js
/**
 * Optimized Input stream for use with XMLHttpRequest undecoded byte Strings
 * @author	Danny Wilson
 * @since	Apr 14, 2011
 */
class ByteStringInput extends haxe.io.Input
{
	var b : String;
	var pos : Int;
	
	public function new(charString : String) {
		this.pos = 0;
		this.b = charString;
	}
	
	override public function readByte() : Int {
		return StringTools.fastCodeAt(b, pos++) & 0xFF;
	}
	
	override public function readString( len : Int ) : String {
		var str = b.substr(pos, len);
		pos += len;
		return str;
	}
}
#end



/**
 * @author	Danny Wilson
 * @since	Jan 20, 2011
 */
class MessagePackResourceSignals extends CommunicationSignals
{
	public function new (progessSignal)
	{
		super();
		progress	= progessSignal;
		started	= new Signal0();
		completed	= new Signal0();
		error		= new Signal1<String>();
	}
}




/**
 * @author	Danny Wilson
 * @since	Jan 20, 2011
 */
class DataServiceEvents extends Signals
{
	var receive	(default, null) : CommunicationSignals;
	var send	(default, null) : CommunicationSignals;
	

	public function new (progessSignal)
	{
		super();
		receive = new MessagePackResourceSignals(progessSignal);
		send	= new MessagePackResourceSignals(progessSignal);
	}
}