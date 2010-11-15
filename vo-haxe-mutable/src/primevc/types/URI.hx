package primevc.types;

enum URIScheme
{
	http;
	https;
	mailto;
	javascript;
	Scheme(p:String);
}

/**
 * A URI is a uniform resource <i>identifier</i> while a URL is a uniform
 * resource <i>locator</i>.  Hence every URL is a URI, abstractly speaking, but
 * not every URI is a URL.  This is because there is another subcategory of
 * URIs, uniform resource <i>names</i> (URNs), which name resources but do not
 * specify how to locate them.
 *  
 * A URL must be absolute, that is, it must always specify a scheme.
 * A URL string is parsed according to its scheme.
 *  
 *  http://en.wikipedia.org/wiki/URI_scheme
 *  
 *  Ook interessant: http://www.php.net/manual/en/function.parse-url.php#90365
 */
class URI
{
#if debug
	static function __init__()
	{
		var u = new URI();
		var mailURI = "mailto:mediahuis@tntpost.nl?subject=Een onvergetelijke kerst";
		
		u.parse(mailURI);
		Assert.that(u.scheme == mailto,  u.string);
		Assert.that(u.userinfo == "mediahuis",  u.userinfo);
		Assert.that(u.host == "tntpost.nl",  u.host);
		Assert.that(u.query == "subject=Een onvergetelijke kerst",  u.query);
		Assert.that(u.string == mailURI, u.string);
		
		u.parse("http://decube.net/a");
		Assert.that(u.scheme == http, u.string);
		Assert.that(u.host == "decube.net", u.string);
		Assert.that(u.path == "/a", u.path);
		
		u.parse("/decube.net/a");
		Assert.that(u.scheme == null, u.string);
		Assert.that(u.host == null, u.string);
		Assert.that(u.path == "/decube.net/a", u.string);
		
		u.parse("decube.net/a");
		Assert.that(u.scheme == null, u.string);
		Assert.that(u.host == null, u.string);
		Assert.that(u.path == "decube.net/a", u.string);
		
		u.parse("decube.net:80/a");
		Assert.that(u.string == "decube.net:80/a", u.string);
		Assert.that(u.scheme == null, Std.string(u.scheme));
		Assert.that(u.host == "decube.net", u.host);
		Assert.that(u.port == 80, Std.string(u.port));
		Assert.that(u.path == "/a", u.path);
	}
#end
	
	public var string (toString, null) : String;
	
	public var scheme	(default, setScheme)	: URIScheme;
	public var userinfo	(default, setUserinfo)	: String;
	public var host		(default, setHost)		: String;
	public var port		(default, setPort)		: Int;
	public var path		(default, setPath)		: String;
	public var query	(default, setQuery)		: String;
	public var fragment	(default, setFragment)	: String;
	
	private inline function setScheme(v)	{ string = null; return scheme = v; }
	private inline function setUserinfo(v)	{ string = null; return userinfo = v; }
	private inline function setHost(v)		{ string = null; return host = v; }
	private inline function setPort(v)		{ string = null; return port = v; }
	private inline function setPath(v)		{ string = null; return path = v; }
	private inline function setQuery(v)		{ string = null; return query = v; }
	private inline function setFragment(v)	{ string = null; return fragment = v; }

	/** Returns true if this URI has a scheme and thus is a URL **/
	public function isURL() return scheme != null
	
	/** Returns the string after the last dot in the path.
	 	Returns an empty string if it has no dots in the path.
	 	Returns empty string if the first char is a dot and there are no other dots (UNIX hidden file convention).
	*/
	public var fileExt	(getFileExt,setFileExt): String;
		private inline function getFileExt() : String {
			if (path == null) return "";
			else {
				var idx = path.lastIndexOf('.');
				return idx <= 1? "" : path.substr(idx+1);
			}
		}
		private inline function setFileExt(v) : String {
			Assert.that(v != null);
			
			var idx = path.lastIndexOf('.');
			path = idx <= 1
				? path + '.' + v
				: path.substr(0, path.lastIndexOf('.')) + '.' + v;
			return v;
		}
	
	public var isSet		(getIsSet, never) : Bool;
		private function getIsSet() { return (string != null && string.length != 0) || (host != null && host.length != 0) || (path != null && path.length != 0); }
	
	public function new() {
		port = -1;
	}
	
	public function toString()
	{
		if (this.string != null) return this.string;
		
		var s:StringBuf = new StringBuf();
		
		if (scheme != null) switch (scheme)
		{
			case mailto: s.add("mailto:");
			
			case Scheme(x):
				s.add(x);
				s.add("://");
			default:
				s.add(Std.string(scheme));
				s.add("://");
		}
		
		if (userinfo != null) {
			s.add(userinfo);
			s.addChar('@'.code);
		}
		
		if (host != null)
			s.add(host);
		
		if (port != -1) {
			s.addChar(':'.code);
			s.add(Std.string(port));
		}
		if (path != null)
			s.add(path);
		
		if (query != null) {
			s.addChar('?'.code);
			s.add(query);
		}
		if (fragment != null) {
			s.addChar('#'.code);
			s.add(fragment);
		}
		
		return string = s.toString();
	}
	
	#if flash9
	public function toRequest() {
		return new flash.net.URLRequest(this.string);
	}
	#end
	
	public function parse(str:String) : URI
	{
		if (str == null) return this;
		
		// Reset values
		this.port = -1; this.scheme = null; this.string = this.userinfo = this.host = this.path = this.query = this.fragment = null;
		
		var pos:Int = 0;
		
		var scheme_pos = str.indexOf(':');
		if (scheme_pos != -1)
		{	
			var has2slashes = str.charCodeAt(scheme_pos + 1) + str.charCodeAt(scheme_pos + 2) == '/'.code << 1;
			var scheme_str = str.substr(0, scheme_pos);
			
			var us = this.scheme = Reflect.field(URIScheme, scheme_str);
			if (us == null)
			{
				if (has2slashes) {
					this.scheme = Scheme(scheme_str);
					pos = scheme_pos + 3;
				}
				else {
					// No generic scheme specified, it's all URI - could be anything
				//	this.string = str;
				//	return this;
				}
			}
			else switch (us)
			{
				case javascript:
					this.string = str;
					return this;
				
				default:
					pos = scheme_pos + 1;
					if (has2slashes) pos += 2;
			}
		}
		
		var user_pos:Int  = str.indexOf('@', pos);
		if (user_pos != -1) {
			this.userinfo = str.substr(pos, user_pos - pos);
			pos = user_pos + 1;
		}
		
		var port_pos:Int  = str.indexOf(':', pos);
		var path_pos:Int  = str.indexOf('/', pos);
		var query_pos:Int = str.indexOf('?', (path_pos  == -1)? pos : path_pos);
		var frag_pos:Int  = str.indexOf('#', (query_pos == -1)? pos : query_pos);
		
		if (port_pos != -1)
		{
			var port_end = path_pos;
			if (port_end == -1) {
				port_end = query_pos;
				if (port_end == -1) {
					port_end = frag_pos;
					if (port_end == -1)
						port_end = str.length;
				}
			}
			
			this.host = str.substr(pos, port_pos - pos);
			this.port = Std.parseInt(str.substr(port_pos+1, port_end - port_pos));
			pos = port_end;
		}
		else if (scheme != null)
		{
			var host_end = path_pos;
			if (host_end == -1) {
				host_end = query_pos;
				if (host_end == -1) {
					host_end = frag_pos;
					if (host_end == -1)
						host_end = str.length;
				}
			}
			
			this.host = str.substr(pos, host_end - pos);
			pos = host_end;
		}
		
		if (query_pos != -1)
		{
			var query_end = frag_pos;
			if (query_end == -1)
				query_end = str.length;
			
			path  = str.substr(pos, query_pos - pos);
			query = str.substr(query_pos + 1, query_end - pos);
		}
		else if (frag_pos != -1)
		{
			path = str.substr(pos, frag_pos - pos);
			fragment = str.substr(frag_pos + 1);
		}
		else
			path = str.substr(pos);
		
		return this;
	}
}
