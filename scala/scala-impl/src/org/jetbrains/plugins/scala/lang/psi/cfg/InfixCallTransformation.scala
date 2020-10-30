package org.jetbrains.plugins.scala.lang.psi
package cfg

import com.intellij.psi.PsiNamedElement
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{MethodInvocation, ScInfixExpr, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement

private trait InfixCallTransformation { this: Transformer =>
  final def transformInfixCall(infixCall: ScInfixExpr): builder.Value = {
    val invocInfo = invocationInfoFor(infixCall)

    def isSugaredAssignment: Boolean = invocInfo.funcRef
      .collect { case elem: PsiNamedElement => elem }
      .exists(_.name == infixCall.operation.getText.init)

    if (infixCall.isAssignmentOperator && isSugaredAssignment) {


      infixCall.left match {
        case leftInvoc: MethodInvocation =>
          /*
            expr(idx) op= arg

            exprRef <- expr
            originalValue <- exprRef.apply(idx)
            resultValue <- originalValue.op(arg)
            exprRef.update(idx, resultValue)
          */
          ???
        case leftRef@ScReferenceExpression.withQualifier(qualifier) =>
          /*
            qualifier.refName op= arg

            qualifierRef <- qualifier
            propValue <- qualifierRef[refName]
            result <- propValue.op(arg)
            qualifierRef[refName] <- result

            !!!! could also be call to refName and refName_=
          */
          ???
        case ScReferenceExpression(variable: ScNamedElement) =>
          /*
            var op= arg

            varRef <- var
            originalValue <- varRef
            varRef <- originalValue.op(arg)
          */
          val varRef  = builder.readVariable(cfg.variable(variable))
          invocInfo.transform(thisRef = Some(varRef))
        case _ =>
          // forgot something? Also react to errors
          ???
      }
    } else if (infixCall.isRightAssoc) {
      invocInfo.transformRightAssoc()
    } else {
      invocInfo.transform()
    }
  }
}
