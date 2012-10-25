package prime.utils.kryo;
 import prime.vo._
 import prime.types._
 import Conversion._
 import scala.collection.immutable.IntMap;
 import com.esotericsoftware.kryo._

class VOKryo(val enumMap : IntMap[_ <: Enum], val typeMap : IntMap[ValueObjectCompanion[_ <: ValueObject]]) extends Kryo
{
  setRegistrationRequired(true);  //[!] ValueObjectSerializer assumes this is true
  setReferences(false);           //[!] ValueObjectSerializer assumes this is false

  val       vectorRegistration =   register(classOf[ Vector[_]    ],      VectorSerializer, 0x0A) // a: array
/*val         dateRegistration =*/ register(classOf[ Date         ],        DateSerializer, 0x0B) // b: birthday, or d mirrored
/*val         rgbaRegistration =*/ register(classOf[ RGBA         ],        RGBASerializer, 0x0C) // c: color
/*val     dateTimeRegistration =*/ register(classOf[ DateTime     ],    DateTimeSerializer, 0x0D) // d: datetime
  val         enumRegistration =   register(classOf[ EnumValue    ],        EnumSerializer, 0x0E) // e: enum
/*val      fileRefRegistration =*/ register(classOf[ FileRef      ],     FileRefSerializer, 0x0F) // f: fileref
  val  valueObjectRegistration =   register(classOf[ ValueObject  ], ValueObjectSerializer, 0x10) //10:
/*val        voRefRegistration =*/ register(classOf[ VORefImpl[_] ],       VORefSerializer, 0x11) //11: 10 + 1
/*val          uriRegistration =*/ register(classOf[ URI          ],         URISerializer, 0x12) //12: I (go) to
/*val    emailAddrRegistration =*/ register(classOf[ EmailAddr    ],   EmailAddrSerializer, 0x13) //13: One 3lectronic mail
/*val     intervalRegistration =*/ register(classOf[ Interval     ],    IntervalSerializer, 0x14) //14: one for
/*val     objectIdRegistration =*/ register(classOf[ ObjectId     ],    ObjectIdSerializer, 0x15) //15:

  override def getRegistration(kind : Class[_]) : Registration = {
    if (kind == null) throw new IllegalArgumentException("type cannot be null.");

    return if (classOf[ValueObject  ].isAssignableFrom(kind)) valueObjectRegistration
      else if (classOf[IndexedSeq[_]].isAssignableFrom(kind))      vectorRegistration
      else if (classOf[EnumValue    ].isAssignableFrom(kind))        enumRegistration
      else super.getRegistration(kind);
  }
}


object VectorSerializer extends Serializer[IndexedSeq[_]](true,true)
{
  def write (kryo:Kryo, out:io.Output, value:IndexedSeq[_]) {
    if (value.size == 0)
      out.writeByte(0);
    else
    {
      val firstClass = value.head.getClass;
      if (value.forall(_.getClass eq firstClass))
      {
        // All items are of the same class
        val registration = kryo.getClassResolver.getRegistration(value.head.getClass);
        val serializer   = registration.getSerializer;
        out.writeLong(value.size << 1L | 1, true);
        out.writeInt (registration.getId , true);
        for (v <- value) kryo.writeObject(out, v, serializer);
      }
      else {
        out.writeLong(value.size << 1L    , true);
        for (v <- value) kryo.writeClassAndObject(out, v);
      }
    }
  }

  def read  (kryo:Kryo, in: io.Input, clz:Class[IndexedSeq[_]]): IndexedSeq[_] = {
    val header = in.readLong(true);
    if (header == 0)
      IndexedSeq.empty;
    else if ((header & 1) != 0) {
      // All items have are of the same class
      val typeID = in.readInt(true);
      val registration = kryo.getClassResolver.getRegistration(typeID);
      (0 until (header >> 1).toInt) map {i => kryo.readObject(in, registration.getType, registration.getSerializer) } toIndexedSeq
    }
    else {
      (0 until (header >> 1).toInt) map {i => kryo.readClassAndObject(in) } toIndexedSeq
    }
  }
}

/** Does not retain the Chronology or Timezone. However, the instant is still the same point in time. */
object DateSerializer extends Serializer[Date](true,true) {
  def write (kryo:Kryo, out:io.Output, value :   Date)        { out.writeLong(value.getMillis, true); }
  def read  (kryo:Kryo, in: io.Input,  clz:Class[Date]): Date = new Date( in.readLong(true) );
}

/** Does not retain the Chronology or Timezone. However, the instant is still the same point in time. */
object DateTimeSerializer extends Serializer[DateTime](true,true) {
  def write (kryo:Kryo, out:io.Output, value :   DateTime)            { out.writeLong(value.getMillis, true); }
  def read  (kryo:Kryo, in: io.Input,  clz:Class[DateTime]): DateTime = new DateTime( in.readLong(true) );
}

/** Does not retain the Chronology or Timezone. However, it is still the same period in time. */
object IntervalSerializer extends Serializer[Interval](true,true) {
  def write (kryo:Kryo, out:io.Output, value:Interval) {
    out.writeLong(value.getStartMillis, true);
    out.writeLong(value.getEndMillis  , true);
  }
  def read  (kryo:Kryo, in: io.Input, clz:Class[Interval]): Interval = new Interval(in.readLong(true), in.readLong(true));
}

object EnumSerializer extends Serializer[EnumValue](true,true) {
  def write (kryo:Kryo, out:io.Output, e:EnumValue) { kryo match {
    case kryo:VOKryo =>
      if (!classOf[scala.Product].isInstance(e)) {
        out.writeInt(e.owner.ID << 1    , true);
        out.writeInt(e.value,             true);
      }
      else {
        out.writeInt(e.owner.ID << 1 | 1, true);
        out.writeString(e.toString);
      }
  }}
  def read  (kryo:Kryo, in: io.Input, clz:Class[EnumValue]): EnumValue = kryo match {
    case kryo:VOKryo =>
      val header = in.readInt(true);
      val enum   = kryo.enumMap(header >>> 1);
      if ((header & 1) == 0) enum(in.readInt(true)) else enum(in.readString());
  }
}

object RGBASerializer extends Serializer[RGBA](true,true) {
  def write (kryo:Kryo, out:io.Output, value:RGBA) {
    out.writeInt(value.rgba);
  }
  def read  (kryo:Kryo, in: io.Input, clz:Class[RGBA]): RGBA = RGBA(in.readInt());
}

object URISerializer extends Serializer[URI](true,true) {
  def write (kryo:Kryo, out:io.Output, value:URI) {
    out.writeString(value.getEscapedURIReference)
  }
  def read  (kryo:Kryo, in: io.Input, clz:Class[URI]): URI = URI(in.readString());
}

object EmailAddrSerializer extends Serializer[EmailAddr](true,true) {
  def write (kryo:Kryo, out:io.Output, value:EmailAddr) {
    out.writeString(value.toString)
  }
  def read  (kryo:Kryo, in: io.Input, clz:Class[EmailAddr]): EmailAddr = EmailAddr(in.readString());
}

object FileRefSerializer extends Serializer[FileRef](true,true) {
  def write (kryo:Kryo, out:io.Output, value:FileRef) {
    out.writeString(value.uri);
    if (value.hash != null) {
      out.writeInt(value.hash.length,true);
      out.write(value.hash);
    }
    else out.write(0);
  }
  def read  (kryo:Kryo, in: io.Input, clz:Class[FileRef]): FileRef = {
    val uri = in.readString();
    val hashLength = in.readInt(true);
    val hash = if (hashLength == 0) null else in.readBytes(hashLength);

    FileRef(uri, hash)
  }
}

object VORefSerializer extends Serializer[VORef[_]](true,true) {
  def write (kryo:Kryo, out:io.Output, value:VORef[_]) {
    kryo.writeClassAndObject(out, value._id);
    if (value.isDefined) kryo.writeClassAndObject(out, value.get);
    else                 kryo.writeClass(out, null);
  }
  def read  (kryo:Kryo, in: io.Input, clz:Class[VORef[_]]): VORef[_] = {
    val _id = kryo.readClassAndObject(in).asInstanceOf[(ValueObject with ID)#IDType];
    val vo  = kryo.readClassAndObject(in).asInstanceOf[ ValueObject with ID        ];
    new VORefImpl(_id, vo);
  }
}

object ObjectIdSerializer extends Serializer[ObjectId](true,true) {
  def write (kryo:Kryo, out:io.Output, value:ObjectId) {
    out.write(value.toByteArray)
  }
  def read  (kryo:Kryo, in: io.Input, clz:Class[ObjectId]): ObjectId = {
    ObjectId(in.readBytes(12));
  }
}

/**
  Format pseudocode:
    0. [kryo ID: ValueObjectSerializer]
    do {
      1. ValueObject type ID
      2. fields << 1 | mixinFollows?
      3. data-type
      4. data
    }
    while (mixinFollows)
*/
object ValueObjectSerializer extends Serializer[ValueObject](true,true)
{
  import java.lang.Integer._

  def writeFields(kryo:Kryo, out:io.Output, vo:ValueObject, mixin: ValueObjectMixin[_]) = vo.foreach(mixin.fieldIndexMask) {
    (f: ValueObjectField[vo.VOType], value: Any) => kryo.writeClassAndObject(out, value);
  }

  def write (kryo:Kryo, out:io.Output, vo:ValueObject) {
    val voManifest = vo.voManifest;
    out.writeInt(voManifest.ID, true);

    val fieldSet  = vo.voIndexSet;
    if (fieldSet != 0)
    {
      val mixin         = voManifest.metaMixin;
      var fieldsToWrite = fieldSet & ~mixin.fieldIndexMask;
      val mixinBits     = mixin.indexBitsShifted(fieldSet) << 1L | (if (fieldsToWrite != 0) 1 /* Lowest bit 1: mixin follows after this. */ else 0);
      // Write fields header
      out.writeLong(mixinBits, true);
      // Write data if header isn't just a "mixin follows" bit.
      if (mixinBits != 1) writeFields(kryo, out, vo, mixin);

      // Per mixin typeID + header + data
      var i = 0; while(fieldsToWrite != 0)
      {
        val mixin      = voManifest.mixins(i);
        val mixinBits  = mixin.indexBitsShifted(fieldsToWrite);
        if (mixinBits != 0)
        {
          fieldsToWrite &= ~mixin.fieldIndexMask;
          val header = mixinBits << 1L | (if (fieldsToWrite != 0) 1 /* Lowest bit 1: mixin follows after this. */ else 0);
          out.writeInt (mixin.manifest.ID, true);
          out.writeLong(header           , true);
          writeFields(kryo, out, vo, mixin);
        }
        i += 1;
      }
    }
    else out.write(0);
  }


  def readFields(kryo:Kryo, in: io.Input, typeID:Int, fieldSet:Int, data:scala.collection.mutable.Builder[(Int, Any),scala.collection.immutable.Map[Int,Any]]) {
    assert(fieldSet != 0);

    val typeID_sl8 = typeID << 8;
    var i    = numberOfTrailingZeros(fieldSet);
    var bits = fieldSet >>> i;
    do {
      val classID = in.readInt(true);
      val value   = if (classID == 0x12 /* 0x10 + 2, bypass registration lookup and read directly as ValueSource */) readAsSource(kryo, in); else {
        val reg = kryo.getClassResolver.getRegistration(classID - 2);
        reg.getSerializer.asInstanceOf[Serializer[AnyRef]].read(kryo, in, reg.getType.asInstanceOf[Class[AnyRef]]);
      }

      data += ((typeID_sl8 | i, value));
      // calc next field ID
      bits >>>= 1;
      val inc = numberOfTrailingZeros(bits);
      bits >>>= inc;
      i      += 1 + inc;
    }
    while (bits != 0);
  }

  def readAsSource(kryo:Kryo, in: io.Input)              : source.ValueSource = readAsSource(kryo, in, in.readInt(true));
  def readAsSource(kryo:Kryo, in: io.Input, voType : Int): source.ValueSource = {
      var mixinHeader = in.readLong(true);
      if (mixinHeader != 0)
      {
        val data = scala.collection.immutable.Map.newBuilder[Int,Any];
        // Self-data
        if (mixinHeader != 1) // Header isn't just a 'mixin follows' marker byte
          readFields(kryo, in, voType, (mixinHeader >>> 1) toInt, data);

        // Mixins
        while ((mixinHeader & 1) != 0) {
          val mixinID = in.readInt (true);
          mixinHeader = in.readLong(true);
          readFields(kryo, in, mixinID, (mixinHeader >>> 1) toInt, data);
        }

        new source.ScalaMapValueSource_Int(data.result);
      }
      else source.ValueSource.empty;
  }

  def read        (kryo:Kryo, in: io.Input, clz:Class[ValueObject]): ValueObject = kryo match { case kryo:VOKryo =>
    val voCompanion = kryo.typeMap( in.readInt(true) );
    readAsSource(kryo, in, voCompanion.manifest.ID) match {
      case source.ValueSource.empty => voCompanion.empty
      case valueSource              => voCompanion(valueSource);
    }
  }
}
