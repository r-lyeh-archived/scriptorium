
class Test { 
   Test () 
   {
     Object obj = "Works";
     String st = (String) obj;
     println( st );
   } 
 
  public class A { 
    public A () {;} 
  } 
 
  public class B { 
    public B () {;} 
    public A something () { 
      if (true) {return null;} 
      return (A) new A (); 
    } 
  } 
 
} 
