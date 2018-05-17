package miniscala

import BitTwiddling.bitsToIntMSBF
import miniscala.{ SymbolicCPSTreeModule => H }
import miniscala.{ SymbolicCPSTreeModuleLow => L }

/**
 * Value-representation phase for the CPS language. Translates a tree
 * with high-level values (blocks, integers, booleans, unit) and
 * corresponding primitives to one with low-level values (blocks
 * and integers only) and corresponding primitives.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object CPSValueRepresenter_nc extends (H.Tree => L.Tree) {
  def apply(tree: H.Tree): L.Tree =
    transform(tree)(Map.empty)

  val unitLit = bitsToIntMSBF(0, 0, 1, 0)
  val optimized = false

  private def transform(tree: H.Tree)
                       (implicit worker: Map[Symbol, (Symbol, Seq[Symbol])])
      : L.Tree = tree match {

    // Literals
    case H.LetL(name, IntLit(value), body) =>
      L.LetL(name, (value << 1) | 1, transform(body))
    case H.LetL(name, CharLit(value), body) =>
      L.LetL(name, (value << 3) | bitsToIntMSBF(1, 1, 0), transform(body))
    case H.LetL(name, BooleanLit(value), body) =>
      if(value) {
        L.LetL(name, bitsToIntMSBF(1, 1, 0, 1, 0), transform(body))
      } else {
        L.LetL(name, bitsToIntMSBF(0, 1, 0, 1, 0), transform(body))
      }
    case H.LetL(name, UnitLit, body) =>
      L.LetL(name, bitsToIntMSBF(0, 0, 1, 0), transform(body))

    // TODO: Add missing literals  -- done
		/*
		int  -- done
		char  -- done
		bool  -- done
		unit  -- done
		*/


    // *************** Primitives ***********************  -- done
    // Make sure you implement all possible primitives
    // (defined in MiniScalaPrimitives.scala)
    //
    // Integer primitives
    case H.LetP(name, MiniScalaIntAdd, args, body) =>
      tempLetP(CPSAdd, args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSSub, Seq(r, c1), transform(body)) } }

    case H.LetP(name, MiniScalaIntSub, args, body) =>
      tempLetP(CPSSub, args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSAdd, Seq(r, c1), transform(body)) } }

    case H.LetP(name, MiniScalaIntMul, args, body) =>
      tempLetL(1) { c1 =>
				tempLetP(CPSSub, Seq(args(0), c1)) { n1 =>
					tempLetP(CPSArithShiftR, Seq(args(1), c1)) { m1 =>
						tempLetP(CPSMul, Seq(n1, m1)) { nm1 =>
							L.LetP(name, CPSAdd, Seq(nm1, c1), transform(body))
      }}}}

    case H.LetP(name, MiniScalaIntDiv, args, body) =>
			tempLetL(1) { c1 =>
				tempLetP(CPSSub, Seq(args(0), c1)) { n1 =>
					tempLetP(CPSSub, Seq(args(1), c1)) { m1 =>
						tempLetP(CPSDiv, Seq(n1, m1)) { nm1 =>
              encodeIntWithName(name, nm1) { res =>
                transform(body)
			}}}}}

    case H.LetP(name, MiniScalaIntMod, args, body) =>
      decodeInt(args(0)) { n =>
        decodeInt(args(1)) { m =>
          tempLetP(CPSMod, Seq(n, m)) { decRes =>
            encodeIntWithName(name, decRes) { res =>
              transform(body)
            }
      }}}

    case H.LetP(name, MiniScalaIntArithShiftLeft, args, body) =>
      tempLetL(1) {c1 =>
        tempLetP(CPSSub, Seq(args(0), c1)) {n =>
          decodeInt(args(1)) { m =>
            tempLetP(CPSArithShiftL, Seq(n, m)) { decRes =>
              L.LetP(name, CPSAdd, Seq(decRes, c1), transform(body))
      }}}}

      case H.LetP(name, MiniScalaIntArithShiftRight, args, body) =>
        tempLetL(1) {c1 =>
          tempLetP(CPSSub, Seq(args(0), c1)) {n =>
            decodeInt(args(1)) { m =>
              tempLetP(CPSArithShiftR, Seq(n, m)) { decRes =>
                L.LetP(name, CPSAdd, Seq(decRes, c1), transform(body))
        }}}}

      case H.LetP(name, MiniScalaIntBitwiseAnd, args, body) =>
        L.LetP(name, CPSAnd, args, transform(body))

      case H.LetP(name, MiniScalaIntBitwiseOr, args, body) =>
        L.LetP(name, CPSOr, args, transform(body))

      case H.LetP(name, MiniScalaIntBitwiseXOr, args, body) =>
        tempLetL(1) { c1 =>
          tempLetP(CPSXOr, Seq(args(0), args(1))) { n=>
            L.LetP(name, CPSAdd, Seq(n, c1), transform(body))
        }}

    // TODO: Add missing integer primitives  -- done
		/*
    add  -- done
		sub  -- done
		mul  -- done
		div  -- done
		mod  -- done
		arithshiftleft  -- done
		arithshiftright  -- done
		bitwiseand  -- done
		bitwiseor  -- done
		bitwisexor  -- done
		*/


    // Block primitives

    case H.LetP(name, MiniScalaBlockAlloc(k), args, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(args(0), c1)) { t1 =>
          L.LetP(name, CPSBlockAlloc(k), Seq(t1), transform(body))
      }}

    case H.LetP(name, MiniScalaBlockTag, args, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSBlockTag, Seq(args(0))) { t1 =>
          tempLetP(CPSArithShiftL, Seq(t1, c1)) { t2=>
            L.LetP(name, CPSAdd, Seq(t2, c1), transform(body))
      }}}

    case H.LetP(name, MiniScalaBlockSet, args, body) =>
      decodeInt(args(1)) { z =>
        L.LetP(name, CPSBlockSet, Seq(args(0), z, args(2)), transform(body))
      }

    case H.LetP(name, MiniScalaBlockGet, args, body) =>
      decodeInt(args(1)) { z =>
        L.LetP(name, CPSBlockGet, Seq(args(0), z), transform(body))
      }

    case H.LetP(name, MiniScalaBlockLength, args, body) =>
      tempLetP(CPSBlockLength, args) { t1 =>
        encodeIntWithName(name, t1) { tmp =>
          transform(body)
      }}

    // TODO: Add block primitives -- done
		/*
		blockalloc  -- done
		blocktag  -- done
		blocklength  -- done
		blockget  -- done
		blockset  -- done
		*/

    // Conversion primitives
    case H.LetP(name, MiniScalaCharToInt, args, body) =>
      tempLetL(2) {c2 =>
        L.LetP(name, CPSArithShiftR, Seq(args(0), c2), transform(body))
      }

    case H.LetP(name, MiniScalaIntToChar, args, body) =>
      tempLetL(2) { c2 =>
        tempLetP(CPSArithShiftL, Seq(args(0), c2)) { t1 =>
          L.LetP(name, CPSAdd, Seq(t1, c2), transform(body))
      }}
    // TODO  -- done
		/*
		int->char  -- done
		char->int  -- done
		*/

    // IO primitives
    case H.LetP(name, MiniScalaByteRead, args, body) =>
      tempLetP(CPSByteRead, Seq()) { t1 =>
        tempLetL(1) { c1=>
          tempLetP(CPSArithShiftR, Seq(t1, c1)) { t2 =>
            L.LetP(name, CPSAdd, Seq(t2, c1), transform(body))
      }}}

    case H.LetP(name, MiniScalaByteWrite, args, body) =>
      decodeInt(args(0)) { n1 =>
        L.LetP(name, CPSByteWrite, Seq(n1), transform(body))
      }

    // TODO  -- done
		/*
		byteread  -- done
		bytewrite  -- done
		*/

    // Other primitives
    case H.LetP(name, MiniScalaId, args, body) =>
      L.LetP(name, CPSId, args, transform(body))
    // TODO  -- done
    /*
    id  -- done
    */

    // Continuations nodes
    case H.LetC(cnts: Seq[H.CntDef], body) =>
      val cntDefs = cnts map { c =>
        L.CntDef(c.name, c.args, transform(c.body))
      }
      L.LetC(cntDefs, transform(body))

    case H.AppC(name, args) =>
      L.AppC(name, args)

    // TODO
		/*
		LetC -- done
 		AppC -- done
		*/

    // Functions nodes
    case H.LetF(funs: Seq[H.FunDef], body) =>
      val funDefs = funs map { f =>
        L.FunDef(f.name, f.retC, f.args, transform(f.body))
      }
      L.LetF(funDefs, transform(body))

    case H.AppF(name, retC, args) =>
      L.AppF(name, retC, args)
    // TODO
		/*
		LetF -- done
		AppF -- done
		*/

    // ********************* Conditionnals ***********************
    // Type tests

    case H.If(MiniScalaBlockP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(0, 0), thenC, elseC)

    case H.If(MiniScalaIntP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(1), thenC, elseC)

    case H.If(MiniScalaBoolP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(1, 0, 1, 0), thenC, elseC)

    case H.If(MiniScalaCharP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(1, 1, 0), thenC, elseC)

    case H.If(MiniScalaUnitP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(0, 0, 1, 0), thenC, elseC)

		// TODO: add missing cases  -- done
		/*
		int? -- done
		char? -- done
		bool? -- done
		unit? -- done
    block? -- done
		*/

    // Test primitives (<, >, ==, ...)
    case H.If(MiniScalaIntLt, Seq(a, b), thenC, elseC) =>
      L.If(CPSLt, Seq(a, b), thenC, elseC)

    case H.If(MiniScalaIntGt, Seq(a, b), thenC, elseC) =>
      L.If(CPSGt, Seq(a, b), thenC, elseC)

    case H.If(MiniScalaIntLe, Seq(a, b), thenC, elseC) =>
      L.If(CPSLe, Seq(a, b), thenC, elseC)

    case H.If(MiniScalaIntGe, Seq(a, b), thenC, elseC) =>
      L.If(CPSGe, Seq(a, b), thenC, elseC)

   case H.If(MiniScalaEq, Seq(a, b), thenC, elseC) =>
      L.If(CPSEq, Seq(a, b), thenC, elseC)

    case H.If(MiniScalaNe, Seq(a, b), thenC, elseC) =>
      L.If(CPSNe, Seq(a, b), thenC, elseC)

    // TODO  -- done
		/*
		lt  -- done
		le  -- done
		ge  -- done
		gt  -- done
		eq  -- done
		ne  -- done
		*/

    // Halt case
		case H.Halt(arg) =>
      decodeInt(arg) { v =>
        L.Halt(v)
      }
    // TODO  -- done
  }

  private def freeVariables(tree: H.Tree)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] = tree match {
    case H.LetL(name, _, body) =>
      freeVariables(body) - name
    case H.LetP(name, _, args, body) =>
      freeVariables(body) - name ++ args
    case H.LetC(cnts, body) =>
      val cntNames = cnts map (_.name)
      freeVariables(body) ++ (cnts flatMap freeVariables)
    case H.LetF(funs, body) =>
      val funNames = funs map ((_ : H.FunDef).name)
      freeVariables(body) ++ (funs flatMap freeVariables) -- funNames
    case H.AppC(_, args) =>
      args.toSet
    case H.AppF(fun, _, args) =>
      args.toSet ++ (worker getOrElse(fun, Seq(fun)))
    case H.If(_, args, _, _) =>
      args.toSet
    case H.Halt(arg) =>
      Set(arg)
  }

  /*
   * Auxilary function.
   *
   * Example:
   *  // assuming we have a function with symbol f and the return continuation is rc:
   *
   *  val names = Seq("first", "second")
   *  val values = Seq(42, 112)
   *  val inner = L.AppF(f, rc, names)
   *  val res = wrap(names zip values , inner) {
   *    case ((n, v), inner) => L.LetL(n, v, inner)
   *  }
   *
   *  // res is going to be the following L.Tree
   *  L.LetL("first", 42,
   *    L.LetL("second", 112,
   *      L.AppF(f, rc, Seq("first", "second"))
   *    )
   *  )
   */
  private def wrap[T](args: Seq[T], inner: L.Tree)(createLayer: (T, L.Tree) => L.Tree) = {
    def addLayers(args: Seq[T]): L.Tree = args match {
      case h +: t => createLayer(h, addLayers(t))
      case _ => inner
    }
    addLayers(args)
  }

  private def freeVariables(cnt: H.CntDef)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] =
    freeVariables(cnt.body) -- cnt.args

  private def freeVariables(fun: H.FunDef)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] =
    freeVariables(fun.body) - fun.name -- fun.args

  // Tree builders

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the given literal value.
   */
  private def tempLetL(v: Int)(body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetL(tempSym, v, body(tempSym))
  }

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the result of applying the given
   * primitive to the given arguments.
   */
  private def tempLetP(p: L.ValuePrimitive, args: Seq[L.Name])
                      (body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetP(tempSym, p, args, body(tempSym))
  }


  /**
   * Generate a tree to decode values
   */
  private def decodeInt(arg: L.Name)(body: L.Name => L.Tree): L.Tree = {
    tempLetL(1) { c1 =>
      tempLetP(CPSArithShiftR, Seq(arg, c1)) { arg1 =>
        body(arg1)
    }}
  }

  /**
   * Generate a tree to encode values
   */
  private def encodeInt(arg: L.Name)(body: L.Name => L.Tree): L.Tree = {
    tempLetL(1) { c1 =>
      tempLetP(CPSArithShiftL, Seq(arg, c1)) { arg1 =>
        tempLetP(CPSAdd, Seq(arg1, c1)) { arg2 =>
          body(arg2)
    }}}
  }

  /**
   * Generate a tree to encode values with a specific name
   */
  private def encodeIntWithName(name: L.Name, arg: L.Name)(body: L.Name => L.Tree): L.Tree = {
    tempLetL(1) { c1 =>
      tempLetP(CPSArithShiftL, Seq(arg, c1)) { arg1 =>
        L.LetP(name, CPSAdd, Seq(arg1, c1), body(arg1))
    }}
  }

  /**
   * Generate an If tree to check whether the least-significant bits
   * of the value bound to the given name are equal to those passed as
   * argument. The generated If tree will apply continuation tC if it
   * is the case, and eC otherwise. The bits should be ordered with
   * the most-significant one first (e.g. the list (1,1,0) represents
   * the decimal value 6).
   */
  private def ifEqLSB(arg: L.Name, bits: Seq[Int], tC: L.Name, eC: L.Name)
      : L.Tree =
    tempLetL(bitsToIntMSBF(bits map { b => 1 } : _*)) { mask =>
      tempLetP(CPSAnd, Seq(arg, mask)) { masked =>
        tempLetL(bitsToIntMSBF(bits : _*)) { value =>
          L.If(CPSEq, Seq(masked, value), tC, eC) } } }
}
