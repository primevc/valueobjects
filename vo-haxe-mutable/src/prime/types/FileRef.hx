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
 *  Ruben Weijers	<ruben @ prime.vc>
 */
package prime.types;
  using StringTools;

/**
 * FileRef class is meant for files that are hosted by online-touch. Use URI-class
 * for uri's that are hosted externally.
 *
 * TODO: Implement mostly the same functionality as the Scala version.
 */
class FileRef extends prime.types.URI
{
	static var cassandra = URIScheme.Scheme("cassandra");

	/**
	 * prefix that will be added before every URI.toString
	 * Will always end with a slash '/' character.
	 */
	public static var prefix (default, set_prefix) : String = "/";

	public function toURIString(cdnPostfix : String)
	{
		if (hasScheme(cassandra))
			return prefix + host + cdnPostfix;
		else
		{
			var uri = super.toString();
			var firstSlash  = uri != ""      && uri.charCodeAt(0) == '/'.code;
			var secondSlash = uri.length > 1 && uri.charCodeAt(1) == '/'.code;

			if (firstSlash)
				uri = (!secondSlash)? prefix + uri.substr(1) : uri;
			else
				uri = scheme == null? prefix + uri : uri;

			return uri + (path.indexOf(".") == -1? cdnPostfix : "");
		}
	}

	public function toURI(?cdnPostfix : String) return new URI(toURIString(if (cdnPostfix == null) "" else cdnPostfix.toLowerCase()));
	
	override public function toString () return toURIString("");
	
	private static inline function set_prefix (v:String)
	{
		Assert.that(v != null);
		return prefix = v == "" || v.charCodeAt(v.length - 1) != '/'.code ? v + "/" : v;
	}
}
