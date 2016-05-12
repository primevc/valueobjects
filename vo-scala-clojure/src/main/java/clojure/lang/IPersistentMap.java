package clojure.lang;

import clojure.lang.IPersistentMap;
import java.util.Iterator;

public interface IPersistentMap extends Iterable<Object>, Associative, Counted{


IPersistentMap assoc(Object key, Object val);

IPersistentMap assocEx(Object key, Object val) ;

IPersistentMap without(Object key) ;

}