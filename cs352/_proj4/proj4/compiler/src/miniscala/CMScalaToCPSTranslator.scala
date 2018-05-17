package miniscala

import miniscala.{ SymbolicCMScalaTreeModule => S }
import miniscala.{ SymbolicCPSTreeModule => C }

object CMScalaToCPSTranslator extends (S.Tree => C.Tree) {
  def apply(tree: S.Tree): C.Tree = {
    nonTail(tree){ z =>
      C.Halt(z)
    }(Set.empty)
  }

  private def nonTail(tree: S.Tree)(ctx: Symbol=>C.Tree)(implicit mut: Set[Symbol]): C.Tree = {
    // @unchecked to avoid bogus compiler warnings

    (tree: @unchecked) match {
      case S.Let(name, _, value, body) =>
        nonTail(value)(v =>
            C.LetP(name, MiniScalaId, Seq(v), nonTail(body)(ctx)))

      // TODO: complete the following cases and add the missing ones. 

      // Reference of an immutable variable
      case S.Ref(name) if !mut(name) =>
       ctx(name)

      // Reference of an mutable variable
      case S.Ref(name) => // if mut(name) =>
        val z = Symbol.fresh("mutVarRef_z")
        val v = Symbol.fresh("mutVarRef_v")
        C.LetL(z, IntLit(0),
          C.LetP(v, MiniScalaBlockGet, Seq(name, z), ctx(v))
        )

      case S.VarDec(name, _, value, body) =>
        val s = Symbol.fresh("varDec_s")
        val z = Symbol.fresh("varDec_z")
        val d = Symbol.fresh("varDec_d")

        val mut2 = mut + name

        C.LetL(s, IntLit(1),
          C.LetP(name, MiniScalaBlockAlloc(242), Seq(s),
            C.LetL(z, IntLit(0),
              nonTail(value)(v =>
                C.LetP(d, MiniScalaBlockSet, Seq(name, z, v),
                  nonTail(body)(ctx)(mut2)
                )
              )
            )
          )
        )

      case S.VarAssign(name, value) =>
        val z = Symbol.fresh("varAssign_z")
        val d = Symbol.fresh("varAssign_d")
        C.LetL(z, IntLit(0),
          nonTail(value)(v =>
            C.LetP(d, MiniScalaBlockSet, Seq(name, z, v), ctx(v))
          )
        )

      case S.Lit(x) =>
        val n = Symbol.fresh("lit_n")
        C.LetL(n, x, ctx(n))

      case S.Prim(op: MiniScalaValuePrimitive, args) =>
        val n = Symbol.fresh("prim_n")

        nonTail(args(0))(v =>
          nonTail(args(1))(v2 =>
            C.LetP(n, op, Seq(v, v2), ctx(n))
          )
        )

      case S.If(S.Prim(cond: MiniScalaTestPrimitive, args), tbr, ebr) =>
        val r = Symbol.fresh("if_r")
        val f = Symbol.fresh("if_f")

        C.LetL(f, BooleanLit(false),
          tempLetC("c", Seq(r), ctx(r))(c1 =>
            tempLetC("ct", Seq(), nonTail(tbr)(v2 => C.AppC(c1, Seq(v2))))(ct1 =>
              tempLetC("cf", Seq(), nonTail(ebr)(v3 => C.AppC(c1, Seq(v3))))(cf1 =>
                nonTail(args(0))(e1 =>
                  nonTail(args(1))(e2 =>
                    C.If(cond, Seq(e1, e2), ct1, cf1)
                  )
                )
              )
            )
          )
        )

      case S.If(cond, tbr, ebr) =>
        val r = Symbol.fresh("if_r")
        val f = Symbol.fresh("if_f")
        nonTail(cond)(v1 =>
          tempLetC("c", Seq(r), ctx(r))(c1 =>
            C.LetL(f, BooleanLit(false),
              tempLetC("ct", Seq(), nonTail(tbr)(v2 => C.AppC(c1, Seq(v2))))(ct1 =>
                tempLetC("cf", Seq(), nonTail(ebr)(v3 => C.AppC(c1, Seq(v3))))(cf1 =>
                  C.If(MiniScalaNe, Seq(v1, f), ct1, cf1)
                )
              )
            )
          )
        )

      case S.Prim(op: MiniScalaTestPrimitive, args) =>
        nonTail(S.If(S.Prim(op, args), S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))))(ctx)

      case S.While(e1, e2, e3) =>
        val f = Symbol.fresh("while_f")
        val loop = Symbol.fresh("while_loop")

        C.LetL(f, BooleanLit(false),
          C.LetC(Seq(C.CntDef(loop, Seq(),
            tempLetC("c", Seq(), nonTail(e3)(ctx))(c1 =>
              tempLetC("ct", Seq(), nonTail(e2)(v1 =>
                C.AppC(loop, Seq())
              ))(ct1 =>
                nonTail(e1)(v2 =>
                  C.If(MiniScalaNe, Seq(v2, f), ct1, c1)
                )  // nontail(e1)
              )  // tlc ct => ct1
            )  // tlc c => c1
          )), C.AppC(loop, Seq()))  // Loop LetC
        )  // flase LetL

      case S.LetRec(funs: List[S.FunDef], body) =>
        val cpsFuns = Seq()
        for(fun <- funs) {
          val c = Symbol.fresh("funDef_c")
          val argSeq = Seq()

          for(arg <- fun.args) {
            argSeq +: Seq(arg.name)
         }

          cpsFuns +: Seq(C.FunDef(fun.name, c, argSeq, nonTail(fun.body)(v =>
            C.AppC(c, Seq(v))
          )
        ))
        }

        C.LetF(cpsFuns, nonTail(body)(ctx))

      case S.App(f, _, args) =>
        val r = Symbol.fresh("app_r")
        val argSeq = Seq()

        for(arg <- args){
          val argv = Symbol.fresh("app_arg")
          argSeq +: Seq(nonTail(arg)(ctx))
        }

        nonTail(f)(v =>
          tempLetC("c", Seq(r), ctx(r))(c1 =>
            C.AppF(v, c1, argSeq)
          ) // tlc c1
        ) // nontail f

      /*
      Prim - done
      Ref - done
      If - done
      VarDec - done
      VarAssign - done
      While - done
      Arg
      FunDef
      LetRec
      App
      */
    }
  }

  private def nonTail_*(trees: Seq[S.Tree])(ctx: Seq[Symbol]=>C.Tree)(implicit mut: Set[Symbol]): C.Tree =
    trees match {
      case Seq() =>
        ctx(Seq())
      case t +: ts =>
        nonTail(t)(tSym => nonTail_*(ts)(tSyms => ctx(tSym +: tSyms)))
    }

  private def tail(tree: S.Tree, c: Symbol)(implicit mut: Set[Symbol]): C.Tree = {
    // @unchecked to avoid bogus compiler warnings
    (tree: @unchecked) match {
      case S.Let(name, _, value, body) =>
        nonTail(value)(v =>
          C.LetP(name, MiniScalaId, Seq(v), tail(body, c)))

      // TODO: add the missing cases.

    }
  }

  private def cond(tree: S.Tree, trueC: Symbol, falseC: Symbol)(implicit mut: Set[Symbol]): C.Tree = {

    def litToCont(l: CMScalaLiteral): Symbol =
      if (l != BooleanLit(false)) trueC else falseC

    tree match {
      case S.If(condE, S.Lit(tl), S.Lit(fl)) =>
        cond(condE, litToCont(tl), litToCont(fl))

      // TODO add missing cases

      case S.Prim(p: MiniScalaTestPrimitive, args) =>
        nonTail_*(args)(as => C.If(p, as, trueC, falseC))

      case other =>
        nonTail(other)(o =>
          nonTail(S.Lit(BooleanLit(false)))(n =>
            C.If(MiniScalaNe, Seq(o, n), trueC, falseC)))
    }
  }

  private def tempLetC(cName: String, args: Seq[C.Name], cBody: C.Tree)
                      (body: C.Name=>C.Tree): C.Tree = {
    val cSym = Symbol.fresh(cName)
    C.LetC(Seq(C.CntDef(cSym, args, cBody)), body(cSym))
  }
}
