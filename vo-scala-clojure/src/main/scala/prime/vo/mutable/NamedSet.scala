package primevc.core.traits

import collection.generic.{GenericCompanion, GenericSetTemplate}
import collection.mutable.{HashSet, Set, SetLike, FlatHashTable}
import primevc.core.traits._

/**
 * Created by IntelliJ IDEA.
 * User: blue
 * Date: Sep 23, 2010
 * Time: 9:24:40 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class NamedSet[A <: ValueObject] extends HashSet[A] with ValueObject
{
  override def size = this.numFieldsSet_? + tableSize
  override def add(vo:A) = if (!vo.empty_?) super.add(vo) else false
}
