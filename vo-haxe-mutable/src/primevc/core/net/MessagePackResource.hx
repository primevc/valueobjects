package primevc.core.net;
 import primevc.core.events.CommunicationEvents;
 import primevc.core.dispatcher.Signals;
 import primevc.core.dispatcher.Signal0;
 import primevc.core.dispatcher.Signal1;
 import primevc.core.dispatcher.Wire;
 import primevc.core.traits.IMessagePackable;
 import primevc.core.net.URLLoader;
 import primevc.types.URI;

class MessagePackResourceSignals extends CommunicationSignals
{
    public function new(progessSignal)
    {
        this.progress  = progessSignal;
        this.started   = new Signal0();
        this.completed = new Signal0();
        this.error     = new Signal1<String>();
    }
}

class DataServiceEvents <Data> extends Signals
{
    var receive     (default, null) : CommunicationSignals;
    var send        (default, null) : CommunicationSignals;
    
    public function new(progessSignal)
    {
        receive = new MessagePackResourceSignals(progessSignal);
        send    = new MessagePackResourceSignals(progessSignal);
    }
}

/**
 * Service class to simplify communication with a REST Resource that accepts MessagePack data.
 */  
class MessagePackResource <Data>
{
    public var events       (default, null) : DataServiceEvents <Data>;
    public var bytesSending (default, null) : Int;
    public var data         (default, null) : Data;
    
    var reader      : primevc.utils.msgpack.Reader;
    var uriPrefix   : URI;
    var loader      : URLLoader;
    var bytes       : haxe.io.Bytes;
    var typeMap     : IntHash<Class<Dynamic>>;
    var onComplete  : Wire<Void   -> Void>;
    var onError     : Wire<String -> Void>;
    
    public function new(uriPrefix : URI, typeMap : IntHash<Class<Dynamic>>)
    {
        this.typeMap   = typeMap;
        this.uriPrefix = uriPrefix;
        Assert.notNull(uriPrefix);
        Assert.notNull(typeMap);
        
        reader = new primevc.utils.msgpack.Reader(typeMap);
        loader = new URLLoader();
        loader.dataFormat = flash.net.URLLoaderDataFormat.BINARY;
        
        var l = loader.events.load;
        onComplete    = l.completed.bind(this, nothing);
        onError       = l.error.observe(this, nothing);
        events        = new DataServiceEvents(l.progress);
    }
    
    private function nothing() throw "impossible"
    
    /**
     * GET a single object by proving a uriSuffix.
     */
    public function get(uriSuffix : String)
    {
        var l   = loader,
            e   = events.receive,
            uri = uriSuffix == null? uriPrefix : new URI(uriPrefix.string + uriSuffix);
        
        onComplete.handler = handleGET;
        onError.handler = cast(events.receive.error, Signal1<Dynamic>).send;
        
        l.load(uri);
        e.started.send();
    }
    
    /**
     * Serialize and POST an object to the Resource. 
     * @param uriSuffix Required to prevent accidental overwriting of an entire resource.
     */
    public function send(uriSuffix : String, obj : IMessagePackable)
    {
        var l   = loader,
            e   = events.send,
            uri = new URI(uriPrefix.string + uriSuffix);
        
        // Serialize
        var out = new haxe.io.BytesOutput();
        out.bigEndian = true;
        this.bytesSending = obj.messagePack(out);
        
        var bytes = out.getBytes();
        Assert.that(bytes.length == bytesSending);
        
        // Send
        onComplete.handler = handlePOST;
        onError.handler = cast(events.send.error, Signal1<Dynamic>).send;
        
        l.binaryPOST(uri, bytes);
        e.started.send();
    }
    
    private function handleGET()
    {
        var b = haxe.io.Bytes.ofData(loader.data);
        var input = reader.input = new haxe.io.BytesInput(b);
        input.bigEndian = true;
        
        this.data = reader.readMsgPackValue();
        events.receive.completed.send();
    }
    
    private function handlePOST()
    {   
        this.bytesSending = 0;
        events.send.completed.send();
    }
}
