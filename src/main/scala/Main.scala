import basic_implicit.{T_Integer, t_Integer, v_or}

import scala.annotation.tailrec


class M_LOCAL_FIBER_CYCLE_OLD_STATIC[T_T](name : String,val t_T : C_TYPE[T_T] with C_TINY[T_T])
  extends Module(name) {
  type T_Result = T_T;
  val v_equal = t_T.v_equal;
  val v_string = t_T.v_string;
  val v_assert = t_T.v_assert;
  val v_node_equivalent = t_T.v_node_equivalent;
  type T_Root = t_T.T_Root;
  val t_Root = t_T.t_Root;
  val t_Wood = t_T.t_Wood;

  val t_Result: this.type = this;

  abstract class T_Context(t: I_PHYLUM[T_Context]) extends Node(t) {}

  val t_Context = new I_PHYLUM[T_Context]("Context");

  case class c_context(v_depth: T_Integer) extends T_Context(t_Context) {
    override def children: List[Node] = List();

    override def toString(): String = Debug.with_level {
      "context(" + v_depth + ")";
    }
  }

  def u_context(x: Any): Option[(T_Context, T_Integer)] = x match {
    case x@c_context(v_depth) => Some((x, v_depth));
    case _ => None
  };
  val v_context = f_context _;

  def f_context(v_depth: T_Integer): T_Context = c_context(v_depth).register;
  val p_context = new PatternFunction[(T_Context, T_Integer)](u_context);

  type T_ContextPtr = T_Context;
  val t_ContextPtr = t_Context;

  private class E_answer(anchor: T_Root) extends Evaluation[T_Root, T_Integer](anchor, anchor.toString() + "." + "answer") {
  }

  private class E_ptr(anchor: T_Context) extends Evaluation[T_Context, T_ContextPtr](anchor, anchor.toString() + "." + "ptr") {
  }
  private object a_ptr extends Attribute[T_Context, T_ContextPtr](t_Context, t_ContextPtr, "ptr") {
    override def createEvaluation(anchor: T_Context): Evaluation[T_Context, T_ContextPtr] = new E_ptr(anchor);
  }

  @tailrec
  def f_index_scope(v_sc: T_ContextPtr, v_i: T_Integer): T_ContextPtr = {
    var v1_result: T_ContextPtr = null.asInstanceOf[T_ContextPtr];
    if (v_or(new M__basic_2[T_ContextPtr](t_ContextPtr).v__op_0(v_sc, new M__basic_8[T_ContextPtr](t_ContextPtr).v_nil), new M__basic_2[T_Integer](t_Integer).v__op_0(v_i, 0))) {
      v1_result = v_sc;
    } else {
      val node = v_sc;
      node match {
        case p_context(_, v_0) => {
          v1_result = f_index_scope(a_ptr.get(v_sc), new M__basic_4[T_Integer](t_Integer).v__op_u(v_i, 1));
        }
        case _ => {
        }
      }
    }
    return v1_result;
  }
}

object Main {
  def main(args: Array[String]): Unit = {

  }
}
