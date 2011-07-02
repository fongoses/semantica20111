abstract class Tipo
	case class Inteiro() extends Tipo
	case class Boleano() extends Tipo
	case class Funcao(t1: Tipo, t2: Tipo) extends Tipo

abstract class Expr
	case class N (n:Int) extends Expr //Inteiro
	case class B (b:Boolean) extends Expr //Booleano
	case class Sum (e1: Expr, e2: Expr) extends Expr //Soma
	case class Meq (e1: Expr, e2: Expr) extends Expr //Maior ou Igual
	case class If (e1: Expr, e2: Expr, e3: Expr) extends Expr //If
	case class Asg (e1: Expr, e2: Expr) extends Expr //Atribuicao (ASG?????)
	case class Deref (e: Expr) extends Expr //Deref
	case class Ref (e:Expr) extends Expr //Ref
	case class Skip() extends Expr //Skip
	case class Seq (e1: Expr, e2: Expr) extends Expr //Sequencia
	case class W (e1: Expr, e2: Expr) extends Expr //While
	case class Fn (s:String, t: Tipo, e: Expr) extends Expr //Funcoes
	case class App (e1: Expr, e2: Expr) extends Expr //Aplicacao
	case class X (s:String) extends Expr //Identificadores
	case class Let (s:String, t: Tipo, e1: Expr, e2: Expr) extends Expr //Let
	case class LetRec (s:String, f: Funcao, e1: Expr, e2: Expr) extends Expr //Let recursivo
		
	


//Main, criada pelo professor para que possamos testar nosso programa
object L3 
{
	def main (args: Array[String]) 
	{
		// Expressao e memoria para teste
//		val ex:Expr = Sum(Sum(N(5),N(10)), Sum(N(10),N(100))) //Expressao a ser avaliada
//		val sigma: List[(String,Int)] = List(("l1",5), ("l2",7)) //"Mapa" de Memoria
//		val gamma: List[(String,Tipo)] = List(("x",Inteiro()), ("y", Inteiro())) //"Mapa" de Identificadores
		
		//val interpretador = new L3Interpreter()

		//val tipo = interpretador.typecheck(ex,gamma) //Comando para rodar a verificacao de tipos
		//val res = interpretador.eval(ex,sigma) //Comando para executar a parte semantica
		println()
//		println("Expressao L3: " + ex) //Imprime a expressao informada
		println()
		

		//println("Tipo: " + tipo) //Imprime o resultado da avaliacao de tipos
		
		

		//res match //caso o retorno seja um valor e uma memoria, imprime a informação
		//{
		//	case Some((exp_final, sigma_final)) =>
		//		println("Resultado da avaliacao de (5 + 10) + (10 + 100): " + exp_final)
		//		println(sigma_final)
		//}
	}
}