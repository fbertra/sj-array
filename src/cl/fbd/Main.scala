package cl.fbd

class A (i : Int) {
  override def toString () = "A: i = " + i  
}
class B (i: Int, d: Double) extends A(i) {
  override def toString () = "B: i = " + i + ", d = " + d  
} 

/*
 * 
 */

object Main {
  def main (args: Array[String]) = {
    testScalaArrayCopy()   
  }
  
  /*
   * copy/paste from (scalalib) scala.Array::copy
   */
  
  def ifInsideScalaArrayCopy (src: AnyRef, dest: AnyRef) = {
    val srcClass = src.getClass
    if (srcClass.isArray && dest.getClass.isAssignableFrom(srcClass))
      true
    else
      false          
  }
  
  /*
   * 
   */
  
  def testScalaArrayCopy() = {
    // primitive
    testIntToInteger ()
    testIntegerToInt ()
    testIntToAny ()
    testIntToString ()
    
    // class
    testBToA ()
        
    testBUndercoverAsAnyToA ()
    testAnyToA ()
  }
  
  //
  // tests with primitive type Int      
  //
      
  def testIntToInteger () = {    
    val arrInt = getArrInt ()    
    val arrJavaLangInteger = new Array[java.lang.Integer] (2)    
    
    println ("")
    println ("Array[Int] -> Array[java.lang.Integer]")
      
    println ("Fast version of Array.copy?: " + ifInsideScalaArrayCopy (arrInt, arrJavaLangInteger))
      
    println ("scala.Array::copy")
      
    scala.Array.copy (arrInt, 0, arrJavaLangInteger, 0, 2)
    
    dump (arrJavaLangInteger)
    
    println ("java.lang.System::arraycopy")
      
    try {
      java.lang.System.arraycopy(arrInt, 0, arrJavaLangInteger, 0, 2)
      
      println ("UNEXPECTED: " + arrJavaLangInteger.mkString ("; "))    
    }
    catch {
      case asex: java.lang.ArrayStoreException => {
        println ("This is ok in Java: " + asex.getClass().getName() + ": " + asex.getMessage())
      }
    }
    
  }
  
  def testIntegerToInt () = {
    val arrJavaLangInteger = new Array[java.lang.Integer] (2)
    arrJavaLangInteger (0) = new java.lang.Integer (10)
    arrJavaLangInteger (1) = new java.lang.Integer (11)
    
    val arrInt = new Array [Int] (2)
    
    println ("")
    
    println ("Array[java.lang.Integer] -> Array[Int]")
      
    println ("Fast version of Array.copy?: " + ifInsideScalaArrayCopy (arrJavaLangInteger, arrInt))
      
    println ("scala.Array::copy")
      
    scala.Array.copy (arrJavaLangInteger, 0, arrInt, 0, 2)
    
    dumpArrInt (arrInt)
    
    println ("java.lang.System::arraycopy")
      
    try {
      java.lang.System.arraycopy(arrJavaLangInteger, 0, arrInt, 0, 2)
      
      println ("UNEXPECTED: " + arrInt.mkString ("; "))    
    }
    catch {
      case asex: java.lang.ArrayStoreException => {
        println ("This is ok in Java: " + asex.getClass().getName() + ": " + asex.getMessage())
      }
    }        
  }
  
  def testIntToAny () = {
    val arrInt = getArrInt ()    
    val arrObj = new Array [Any] (2)
    
    println ("")
    
    println ("Array[Int] -> Array[Any]")
      
    println ("Fast version of Array.copy?: " + ifInsideScalaArrayCopy (arrInt, arrObj))
      
    println ("scala.Array::copy")
            
    scala.Array.copy (arrInt, 0, arrObj, 0, 2)
    
    dump (arrObj)
    
    println ("java.lang.System::arraycopy")
      
    try {
      java.lang.System.arraycopy(arrInt, 0, arrObj, 0, 2)
      
      println ("UNEXPECTED: " + arrObj.mkString ("; "))    
    }
    catch {
      case asex: java.lang.ArrayStoreException => {
        println ("This is ok in Java: " + asex.getClass().getName() + ": " + asex.getMessage())
      }
    }    
    
  }
  
  def testIntToString () = {
    val arrInt = getArrInt ()
    val arrString = new Array[String](2)
        
    println ("")
    
    println ("Array[Int] -> Array[String]")
      
    println ("Fast version of Array.copy?: " + ifInsideScalaArrayCopy (arrInt, arrString))
    
    println ("scala.Array::copy")
            
    try {
      scala.Array.copy (arrInt, 0, arrString, 0, 2)
      
      dump (arrString)
    }
    catch {
      case asex: java.lang.ArrayStoreException => {
        println ("This is ok in Scala: " + asex.getClass().getName() + ": " + asex.getMessage())
      }
    }
    
    println ("java.lang.System::arraycopy")
      
    try {
      java.lang.System.arraycopy(arrInt, 0, arrString, 0, 2)
      
      println (arrString.mkString ("; "))    
    }
    catch {
      case asex: java.lang.ArrayStoreException => {
        println ("This is ok in Java: " + asex.getClass().getName() + ": " + asex.getMessage())
      }
    }    
  }
  
  //
  // tests with non primitive      
  //
      
  def testBToA () = {
    val arrA = new Array[A] (2)
    val arrB = new Array[B] (2)
    
    arrB(0) = new B(1, 1.0)
    arrB(1) = new B(2, 2.0)
    
    println ("")
    
    println ("Array[B] -> Array[A]")
      
    println ("Fast version of Array.copy?: " + ifInsideScalaArrayCopy (arrB, arrA))
    
    println ("scala.Array::copy")
            
    scala.Array.copy (arrB, 0, arrA, 0, 2)
      
    dump (arrA)
    
    println ("java.lang.System::arraycopy")
      
    java.lang.System.arraycopy(arrB, 0, arrA, 0, 2)
      
    dump (arrA)
  }
  
  
  def testBUndercoverAsAnyToA () = {
    val arrA = new Array[A] (2)
    val arrAnyFilledWithBs = new Array[Any] (2)
    
    // array of Any but every element is of type B and assignable to A
    arrAnyFilledWithBs(0) = new B(1, 1.0)
    arrAnyFilledWithBs(1) = new B(2, 2.0)
    
    println ("")
    
    println ("Array[Any] filled with elements of type [B >: A]  -> Array[A]")
      
    println ("Fast version of Array.copy?: " + ifInsideScalaArrayCopy (arrAnyFilledWithBs, arrA))
    
    println ("scala.Array::copy")
            
    scala.Array.copy (arrAnyFilledWithBs, 0, arrA, 0, 2)
      
    dump (arrA)
    
    println ("java.lang.System::arraycopy")
      
    java.lang.System.arraycopy(arrAnyFilledWithBs, 0, arrA, 0, 2)
      
    dump (arrA)
  }
  
  def testAnyToA () = {
    val arrA = new Array[A] (2)
    val arrAny = new Array[Any] (2)
    
    // array of Any with one element of type B (assignable to A) and other of type String (not assignable to A)
    arrAny(0) = new B(1, 1.0)
    arrAny(1) = new String ("I'm not assignable to A")
    
    println ("")
    
    println ("Array[Any] -> Array[A]")
      
    println ("Fast version of Array.copy?: " + ifInsideScalaArrayCopy (arrAny, arrA))
    
    println ("scala.Array::copy")
     
    try {
      scala.Array.copy (arrAny, 0, arrA, 0, 2)
      
      dump (arrA)
    }
    catch {
      case asex: java.lang.ArrayStoreException => {
        println ("This is ok in Scala but unfortunate: " + asex.getClass().getName() + ": " + asex.getMessage())
      }
    }  
    
    println ("java.lang.System::arraycopy")
      
    try {
      java.lang.System.arraycopy(arrAny, 0, arrA, 0, 2)
      
      dump (arrA)
    }
    catch {
      case asex: java.lang.ArrayStoreException => {
        println ("This is ok in Java: " + asex.getClass().getName() + ": " + asex.getMessage())
      }
    }        
  }
  
  /*
   * 
   */
  
  def getArrInt() = {
    val arrInt = new Array[Int] (2)
    
    for (c <- 0 until arrInt.length)
      arrInt(c) = c
    
    arrInt
  }
  
  /*
   * 
   */
  def dump [T] (arr: Array[T]) = {
    println (arr.map (value => value.getClass.getName() + ": " + value.toString ())mkString ("; "))
  }
  
  def dumpArrInt (arrInt: Array[Int]) = {
    println (arrInt.mkString ("; "))
  }
    

}