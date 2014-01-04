import std.stdio;
import esdl.data.rand;
import esdl.data.obdd;
import esdl.data.bvec;

class Bar: Randomizable
{
  @rand!160 byte[] foo;

  void display() {
    writeln("foo: ", foo);
  }
}

void main()
{
  auto foo = new Bar;
  for (size_t i=0; i!=32; ++i)
    {
      foo.randomize();
      foo.display();
    }
}
