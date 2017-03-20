object demo {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
   case class TypeEnvironment()
   {
   val TypeHashTable = scala.collection.mutable.HashMap.empty[Var,ManyTypes]
   def assigntype (v:Var, n:ManyTypes)
   {
      TypeHashTable += (v -> n)
   }
   
   def readtype(v:Var):ManyTypes = TypeHashTable(v)
   }
  
 trait ManyTypes
 {
 var printtype:String
 }
 
 case class Intgr() extends ManyTypes
 {
 var printtype:String="Integer"
 }
 
 case class Bln() extends ManyTypes
 {
 var printtype:String="Boolean"
 }

case class error() extends ManyTypes
 {
 var printtype:String="error"
 }


   trait Aexp
  {
  	def TypeCheck(t:TypeEnvironment):ManyTypes
  }
  
  case class Truth(b:Boolean=true) extends Aexp
  {
     def TypeCheck(t:TypeEnvironment) = Bln()
  }
  
  case class False(b:Boolean=false) extends Aexp
  {
     def TypeCheck(t:TypeEnvironment) = Bln()
  }
  
  case class Num(n:Integer) extends Aexp
  {
     def TypeCheck(t:TypeEnvironment) = Intgr()
  }
  
  case class Var(c:Character) extends Aexp
  {
    
    def TypeCheck(t:TypeEnvironment) =
    {
    t.readtype(this)
    
    }
  }
  

  
  case class Add(left:Aexp, right:Aexp) extends Aexp
  {
   def TypeCheck(t:TypeEnvironment) =
   {
   			(left.TypeCheck(t), right.TypeCheck(t)) match {
   			case (Intgr(), Intgr()) => Intgr()
   			  									
   			                    
   			case _ => error()
   			                                        }
   			                    
   }
   
   }
 
 trait Cexp
 {
 def TypeCheck(t:TypeEnvironment):TypeEnvironment
 }
   
 case class Assign (v:Var, e:Aexp) extends Cexp
{
def TypeCheck(t:TypeEnvironment):TypeEnvironment =
{
   var s = e.TypeCheck(t)
   t.assigntype(v, s)
   return t
}
}

case class If (b:Aexp, c1:Cexp, c2:Cexp) extends Cexp
{
def TypeCheck(t:TypeEnvironment):TypeEnvironment =
{
	b.TypeCheck(t) match {
	
	           case(Bln()) =>	{
															var t1 = c1.TypeCheck(t)
															var t2 = c2.TypeCheck(t)
															//Check if hash table of t1 and t2 are same
															t2
														}
	
	          case _ => {
	                     println ("error")
	                     t
	                     }
	
	       }
}
}


// main

// Step 1 :   x := Truth      x -> Bln() in TypeEnvironment

  var x = Var('x')                                //> x  : demo.Var = Var(x)
  var t = TypeEnvironment()                       //> t  : demo.TypeEnvironment = TypeEnvironment()
  var asgn_x = Assign(x,Truth()).TypeCheck(t)     //> asgn_x  : demo.TypeEnvironment = TypeEnvironment()
  println (x.TypeCheck(asgn_x))                   //> Bln()

// Step 2:   a := 5
  var a = Num(5)                                  //> a  : demo.Num = Num(5)
  println (a.TypeCheck(asgn_x))                   //> Intgr()

// Step 3:   Num + Truth = error
  var add1 = Add(Num(5),Truth())                  //> add1  : demo.Add = Add(Num(5),Truth(true))
  println(add1.TypeCheck(asgn_x))                 //> error()
  
// Step 4: Num + Num = Intgr
  var add2 = Add(Num(5),Num(10))                  //> add2  : demo.Add = Add(Num(5),Num(10))
  println(add2.TypeCheck(asgn_x))                 //> Intgr()
  
// Step 5: If b is not Boolean in IFELSE
  var i = If(Num(5), Assign(x, False()), Assign(x, False()))
                                                  //> i  : demo.If = If(Num(5),Assign(Var(x),False(false)),Assign(Var(x),False(fa
                                                  //| lse)))
  
  println (i.TypeCheck(t))                        //> error
                                                  //| TypeEnvironment()
                                                  
 // Step 6: Print TypeEnvironment i
 i.TypeCheck(t).TypeHashTable.foreach {case (key, value) => println (key + "-->" + value)}
                                                  //> error
                                                  //| Var(x)-->Bln()
  
}