package renesca.json.protocols

import renesca.compat
import renesca.json._
import spray.json.{DefaultJsonProtocol, JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue, RootJsonFormat, deserializationError, serializationError}

import scala.collection.mutable

object ValueProtocol extends DefaultJsonProtocol {

  implicit object JsonValueFormat extends RootJsonFormat[Value] {

    override def write(p: Value) = ???

    override def read(value: JsValue) = value match {
      case JsString(str) => StringValue(str)
      case JsNumber(num) if num.isValidLong => LongValue(num.toLong)
      case JsNumber(num) if compat.bigDecimal_isDecimalDouble(num) => DoubleValue(num.toDouble) //TODO: !num.isValidLong ?
      case JsBoolean(bool) => BooleanValue(bool)
      case JsArray(arr) => ArrayValue(arr.map(read)) // TODO: check type of arr
      case json => deserializationError(s"can not deserialize property value of type $json")
    }
  }

  implicit object PropertiesJsonValueFormat extends RootJsonFormat[Map[String, Value]] {

    override def write(jsonValues: Map[String, Value]) = {
      val jsonMap = jsonValues.map { case (k, v) => k -> toJsValue(v)}.toMap
      JsObject(jsonMap)
    }

    def toJsValue(value : Value) : JsValue =  value match {
      case StringValue(str) => JsString(str)
      case DoubleValue(num) => JsNumber(num)
      case LongValue(num) => JsNumber(num)
      case ArrayValue(arr) => JsArray((arr map toJsValue).toVector)
      case other => serializationError(s"can not serialize value of type $other")
    }

    override def read(value: JsValue) = value match {
      case JsObject(fields) =>
        fields.map { case (k, v) => k -> JsonValueFormat.read(v)}.toMap
      case json => deserializationError(s"can not deserialize property value map of type $json")
    }
  }

}