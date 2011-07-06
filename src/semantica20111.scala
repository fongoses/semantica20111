////////////////////////////////////////////////////////////////////////////////
// Tipos de L3, conforme a especificacao
////////////////////////////////////////////////////////////////////////////////
abstract class Tipo
	case class Inteiro() extends Tipo
	case class Boleano() extends Tipo
	case class Unidade() extends Tipo
	case class Funcao(t1: Tipo, t2: Tipo) extends Tipo
	case class Referencia(t: Tipo) extends Tipo

////////////////////////////////////////////////////////////////////////////////
// Sintaxe das Expressoes
////////////////////////////////////////////////////////////////////////////////
abstract class Expr
	case class N (n:Int) extends Expr // Inteiro (n)
	case class B (b:Boolean) extends Expr // Booleano (b)
	case class Sum (e1: Expr, e2: Expr) extends Expr // Soma (e1+e2)
	case class Meq (e1: Expr, e2: Expr) extends Expr // Maior ou Igual (e1>=e2)
	case class If (e1: Expr, e2: Expr, e3: Expr) extends Expr // If (if e1 then e2 else e3)
	case class Asg (e1: Expr, e2: Expr) extends Expr // Atribuicao (e1:=e2)
	case class Deref (e: Expr) extends Expr // Deref (!e)
	case class Ref (e:Expr) extends Expr // Ref (ref e)
	case class Skip() extends Expr // Skip (skip)
	case class Seq (e1: Expr, e2: Expr) extends Expr // Sequencia (e1;e2)
	case class W (e1: Expr, e2: Expr) extends Expr // While (while e1 do e2)
	case class Fn (s:String, t: Tipo, e: Expr) extends Expr // Funcoes (fn x:T=>e)
	case class App (e1: Expr, e2: Expr) extends Expr // Aplicacao (e1 e2)
	case class X (s: String) extends Expr // Identificadores (x)
	case class Let (s: String, t: Tipo, e1: Expr, e2: Expr) extends Expr // Let (let x:T=e1 in e2 end)
	
	
class L3Interpreter {
  
    def testaTipos(e:Expr, gamma: List[(String,Tipo)])
    {
      
    	val interpretador = new L3Interpreter()
		println()		
		println("Expressao: " + e)
		println()
		println("Tipo: " + interpretador.typecheck(e,gamma))
		println("--------------------------------------")
            
    }

    ////////////////////////////////////////////////////////////////////////////////
	// Verificador de Tipos
    ////////////////////////////////////////////////////////////////////////////////
	def typecheck(e:Expr, gamma: List[(String,Tipo)]) : Option[Tipo] = e match 
	{
		//Inteiros
		case N (_) => Some(Inteiro()) 
		
		//Booleanos
		case B (_) => Some(Boleano()) 
		
		//Skip
		case Skip() => Some(Unidade())	
		
		//Soma
		case Sum (e1, e2) =>(typecheck(e1,gamma), typecheck(e2,gamma)) match 
		{
			case (Some(Inteiro()), Some(Inteiro())) => Some(Inteiro())
			case _ => None
		}
		
		//Maior ou Igual
		case Meq (e1, e2) =>(typecheck(e1,gamma),typecheck(e2,gamma)) match
		{
			case (Some(Inteiro()), Some(Inteiro())) => Some(Boleano())
			case _ => None 
		}

		//If then else 
		case If (e1, e2, e3) => (typecheck(e1,gamma)) match
		{
		  case (Some(Boleano())) => val returnValue : Option[Tipo] = typecheck(e2,gamma)
				  					if(typecheck(e3,gamma)==returnValue)
				  						returnValue
				  					else
				  					  None
		  case _ => None	
		}
		  
		//Ref
		case Ref (e) => (typecheck(e,gamma)) match
		{
			case (Some(t: Tipo)) => Some(Referencia(t))
			case _ => None
		}
		
		//Atribuicao
		case Asg (e1, e2) => (typecheck(e1,gamma)) match
		{
			case (Some(Referencia(t: Tipo))) => if(Some(t) == typecheck(e2,gamma))
													Some(Unidade())
												else
												  None
			case _ => None
		}

		//Deref
		case Deref (e) => (typecheck(e,gamma)) match
		{
		  case(Some(Referencia(t: Tipo))) => Some(t)
		  case _ => None
		}

		
		//Sequencia
		case Seq (e1, e2) =>	(typecheck(e1,gamma), typecheck(e2,gamma)) match 
		{
			case (Some(Unidade()), Some(t:Tipo)) => Some(t)
			case _ => 	None				
		}
		
        // While
		case W (e1, e2) => (typecheck(e1, gamma), typecheck(e2, gamma)) match
        {
            case (Some(Boleano()), Some(Unidade())) => Some(Unidade())
            case _ =>
                if (e1 != Boleano()) {
                    println("Erro: typecheck | W (e1, e2)")
                    None
                } else {
                    println("Erro: typecheck | W (Boleano(), e2)")
                    None
                }
        }
		
		// Funcoes
		case Fn (s:String, t: Tipo, e) => (typecheck(e, gamma)) match
        {
            case Some(tr: Tipo) =>
                /* 
                    Listas:
                        val fruit: List[String] = List("apples", "oranges", "pears")
                        ou
                        val fruit = "apples" :: ("oranges" :: ("pears" :: Nil))
                        ou
                        val fruit = "apples" :: "oranges" :: "pears" :: Nil

                    Fonte: http://www.scala-lang.org/docu/files/ScalaByExample.pdf página 64
                */
                val add_gamma = (s, t) :: gamma // Adiciona s, t ao ambiente gamma 
                Some(Funcao(t, tr))
            case _ =>
                println("Erro: typecheck | Fn (s, t, e)")
                None
        }
        
        // Aplica��o
		case App (e1, e2) => (typecheck(e1, gamma), typecheck(e2, gamma)) match
        {
            case (Some(Funcao(tp: Tipo, tr: Tipo)), Some(te2: Tipo)) => 
                if (tp == te2) { // Deve-se verificar de o tipo de e2 é o mesmo que o parâmetro de entrada da função e1
                Some(tr) // Deve resultar no tipo de retorno de e1
                } else {
                    println("Erro: typecheck | App (Funcao(), e2)")
                    None
                }
            case _ => 	
                println("Erro: typecheck | App (Funcao(), e2)")
                None
        }
        
        // Identificador
		/* 
            .find: retorna uma instancia contendo o primeiro elemento encontrado que satisfaça a propriedade ou nenhum em caso contrário
            Fonte: http://www.codecommit.com/blog/scala/scala-collections-for-the-easily-bored-part-3
        */
        //////////////////////////////////
        // ATENCAO: Achar um substitutivo
        // Coloquei aqui para podermos testar
        //////////////////////////////////
        case X (s: String) => (gamma.find(x => x._1 == s)) match 
        {
            case Some((s, t)) => Some(t)	// Se for encontrado um elemento da lista que satisfaça "s" retorna (s, tipo) então retorna tipo associado a "s"
            case _ => 	
                println("Erro: typecheck | X (s)") 
                None
        }
        
        // Let
		case Let (s: String, t: Tipo, e1, e2) => (typecheck(e1, gamma), typecheck(e2, gamma)) match {
            case (Some(t1: Tipo), Some(te2: Tipo)) =>
                if (t1 == t) { // O tipo de e1 deve ser igual a t
                    val add_gamma = (s, t) :: gamma // Adiciona s, t ao ambiente gamma 
                    Some(te2) // Deve retornar o tipo de e2
                } else {
                    println("Erro: typecheck | Let (s, t, Tipo(), Tipo())")
                    None
                }
            case _ => 
                println("Erro: typecheck | Let (s, t, e1, e2)") 
                None
        }
		
/*

		
		case Skip() =>
		case Seq (e1, e2) =>
		case W (e1, e2) =>
		case Fn (s:String, t: Tipo, e) =>
		case App (e1, e2) =>
		case X (s:String) =>
		case Let (s:String, t: Tipo, e1, e2) =>
		case LetRec (f: Tipo, e1, e2) =>
		*/
	}	
	
/*		
		
	////////////////////////////////////////////////////////////////////////////////
    // Semantica Operacional
    ////////////////////////////////////////////////////////////////////////////////

	def isvalue(e:Expr) : Boolean = e match 
	{
		case N(_) => true
		case X(_) => true
		case B(_) => true
		case Fn(_,_,_) => true
		case Skip() => true
		case _ => false
	}

	
	type Endereco = String
	type Memoria = List[(Endereco,Int)]

	def step(e: Expr, sigma: Memoria): Option[(Expr, Memoria)] = e match 
	{
		case N(_) => None
		case B(_) => None
		case Sum (e1, e2) => (e1,e2) match
		{
			case (N(n1),N(n2)) => Some ((N(n1 + n2), sigma))
			case (e1, e2) => if (isvalue(e1)) 
			{
				step(e2,sigma) match 
				{
					case Some((e2lin, sigmalin)) =>	Some((Sum(e1,e2lin), sigmalin))
					case None => None
				}
			} 
			else 
			{
				step(e1, sigma) match 
				{
					case Some((e1lin, sigmalin)) =>Some((Sum(e1lin, e2), sigmalin))
					case None => None
				}
			}
		}
		// case Prod (e1, e2) => ...
		// case Dif (e1, e2) => ...
		// case Eq (e1, e2) => ...
		case If(B(true), e2, e3) => Some(e2, sigma)
		case If(B(false), e2, e3) => Some(e3, sigma)
		// case If(e1, e3, e3) => ....
		// .....
	}

	def eval(e: Expr, sigma:Memoria): Option[(Expr, Memoria)] = step(e,sigma) match 
	{
		case None => Some((e,sigma))
		case Some((elin, sigmalin)) => eval(elin, sigmalin)
	}
*/	
}	



////////////////////////////////////////////////////////////////////////////////
// Main, criada pelo professor para que possamos testar nosso programa
////////////////////////////////////////////////////////////////////////////////
object L3 
{
	def main (args: Array[String]) 
	{

        ////////////////////////////////////////////////////////////////////////////////
        // Memoria (sigma) e Ambiente (gamma)
        ////////////////////////////////////////////////////////////////////////////////
		//val sigma: List[(String,Int)] = List(("l1",5), ("l2",7)) //"Mapa" de Memoria
		val gamma: List[(String,Tipo)] = List(("x",Inteiro()), ("y", Inteiro())) //"Mapa" de Identificadores
        println("Memoria e Ambiente prontos.")
        ////////////////////////////////////////////////////////////////////////////////

		
        ////////////////////////////////////////////////////////////////////////////////
        // Carrega o interpretador
        ////////////////////////////////////////////////////////////////////////////////
		val interpretador = new L3Interpreter()
        println("Interpretador Carregado.\n\n")
        ////////////////////////////////////////////////////////////////////////////////


        ////////////////////////////////////////////////////////////////////////////////
        // Fazendo as Verificacoes
        ////////////////////////////////////////////////////////////////////////////////
		
		println("========================================")
		println("Testes para Sum (e1, e2)")
		println("========================================")
		interpretador.testaTipos((Sum(N(10),Sum(N(80),N(5)))),gamma)
		interpretador.testaTipos((Sum(N(10),Sum(N(15),B(true)))),gamma)
		
		println("========================================")
		println("Testes para Meq (e1, e2)")
		println("========================================")
		interpretador.testaTipos((Sum(N(10),Sum(N(80),N(5)))),gamma)
		interpretador.testaTipos((Sum(N(10),Sum(N(15),B(true)))),gamma)

		println("========================================")
		println("Testes para If (e1, e2, e3)")
		println("========================================")
		interpretador.testaTipos((If(B(true),N(10),Sum(N(15),N(20)))),gamma)
		interpretador.testaTipos(If(B(false),N(10),B(true)),gamma)
		
		println("========================================")
		println("Testes para Asg(e1, e2)")
		println("========================================")
		interpretador.testaTipos((Asg(Ref(B(true)),B(false))),gamma)
		interpretador.testaTipos((Asg(Ref(B(true)),N(10))),gamma)
		
		println("========================================")
		println("Testes para Deref(e)")
		println("========================================")
		interpretador.testaTipos((Deref(Ref(N(10)))),gamma)
		interpretador.testaTipos((Deref(N(10))),gamma)	
		
		println("========================================")
		println("Testes para Ref(e)")
		println("========================================")
		interpretador.testaTipos((Ref(Sum(N(10),N(20)))),gamma)
		interpretador.testaTipos((Ref(B(true))),gamma)
		
		println("========================================")
		println("Testes para Skip()")
		println("========================================")
		interpretador.testaTipos(Skip(),gamma)
		
		println("========================================")
		println("Testes para Seq(e1, e2)")
		println("========================================")
		interpretador.testaTipos((Seq(Skip(), Sum(N(1), N(35)))),gamma)
		interpretador.testaTipos((Seq(Skip(), B(true))),gamma)
		
		println("========================================")
		println("Testes para W(e1, e2)")
		println("========================================")
		interpretador.testaTipos((W(B(true), N(0))),gamma)
		interpretador.testaTipos((W(B(false), Sum(N(7),N(9)))),gamma)	
		interpretador.testaTipos((W(N(27), B(false))),gamma)
		interpretador.testaTipos((W(B(true),Skip())),gamma)

		println("========================================")
		println("Testes para X(x)")
		println("========================================")
		interpretador.testaTipos((X("x")),gamma)
		interpretador.testaTipos((X("y")),gamma)
		interpretador.testaTipos((X("z")),gamma)

		println("========================================")
		println("Testes para Fn(s,t,e)")
		println("========================================")
		interpretador.testaTipos((Fn("x",Boleano(),Skip())),gamma)
		interpretador.testaTipos((Fn("x",Inteiro(),X("k"))),gamma)

		println("========================================")
		println("Testes para App(e1,e2)")
		println("========================================")
		interpretador.testaTipos(App(Fn("teste",Boleano(),N(10)),B(true)),gamma)
		interpretador.testaTipos(App(Fn("teste",Boleano(),Fn("var",Inteiro(),N(10))),B(true)),gamma)

		println("========================================")
		println("Testes para App(s,t,e1,e2)")
		println("========================================")
		interpretador.testaTipos(Let("aString",Boleano(),B(true),N(10)),gamma)
		interpretador.testaTipos(Let("aString",Boleano(),N(20),N(10)),gamma)


	

		//case class Let (s:String, t: Tipo, e1: Expr, e2: Expr) extends Expr //Let
	    

		

		//res match //caso o retorno seja um valor e uma memoria, imprime a informa��o
		//{
		//	case Some((exp_final, sigma_final)) =>
		//		println("Resultado da avaliacao de (5 + 10) + (10 + 100): " + exp_final)
		//		println(sigma_final)
		//}
	}
}