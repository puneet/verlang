import std.stdio;
import esdl.data.rand;
import esdl.data.obdd;
import esdl.data.bvec;

class Bar: Randomizable
{
  @rand!16 byte[] foo;
  @rand byte[8] bar;

  void display() {
    writeln("foo: ", foo);
    writeln("bar: ", bar);
  }
  Constraint! q{
    foo.length > 7;
  } cstFooLength;
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
