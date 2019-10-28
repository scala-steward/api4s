package api4s.codegen.ast

import scala.collection.immutable.ListMap

object Merge {
  class OverlappingNames(name: String) extends Exception(s"name '$name' is overlapping")

  class OverlappingEndpoints(path: List[Segment], method: Method)
    extends Exception(s"${method.toString} /${path.mkString("/")} is overlapping")

  private def mergeTypes(
    a: ListMap[String, Type],
    b: ListMap[String, Type]
  ): ListMap[String, Type] = {
    var res = a.toList

    b foreach {
      case (k, _) if a.contains(k) => throw new OverlappingNames(k)
      case x => res = x :: res
    }
    ListMap(res: _*)
  }

  private def mergeEndpoints(
    a: ListMap[List[Segment], ListMap[Method, Endpoint]],
    b: ListMap[List[Segment], ListMap[Method, Endpoint]]
  ): ListMap[List[Segment], ListMap[Method, Endpoint]] = {
    import scala.collection.mutable.{ ListMap => LMap }
    val res = LMap(a.toList: _*)

    b foreach { case (path, method) =>
      def mergeMethods(
        a: ListMap[Method, Endpoint],
        b: ListMap[Method, Endpoint]
      ): ListMap[Method, Endpoint] = {
        var res = a.toList

        b foreach {
          case (k, _) if a.contains(k) => throw new OverlappingEndpoints(path, k)
          case x => res = x :: res
        }
        ListMap(res: _*)
      }

      res.update(path, mergeMethods(a.getOrElse(path, ListMap.empty), method))
    }
    ListMap(res.toList: _*)
  }

  implicit class AstApiOps(val api: Api) extends AnyVal {
    def ++(other: Api): Api = Api(
      mergeTypes(api.types, other.types),
      mergeEndpoints(api.endpoints, other.endpoints)
    )
  }
}
