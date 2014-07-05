package funsets

object Main extends App {
  import FunSets._
	val set123 = union(union(singletonSet(1),singletonSet(2)), singletonSet(3))
	println(exists(map(set123, x=>x+1), x => x==1));
	println(exists(map(set123, x=>x+1), x => x==2));
	println(exists(map(set123, x=>x+1), x => x==3));
	println(exists(map(set123, x=>x+1), x => x==4));
	println(exists(map(set123, x=>x+1), x => x==5));
}
