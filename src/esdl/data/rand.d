// Written in the D programming language.

// Copyright: Coverify Systems Technology 2012 - 2013.
// License:   Distributed under the Boost Software License, Version 1.0.
//            (See accompanying file LICENSE_1_0.txt or copy at
//            http://www.boost.org/LICENSE_1_0.txt)
// Authors:   Puneet Goel <puneet@coverify.com>

module esdl.data.rand;

import esdl.data.obdd;

import std.ascii: whitespace;
import std.traits: isSomeString;
import std.traits: isIntegral;
import esdl.data.bvec: isBitVector;
import std.algorithm : min, max;
import esdl.data.bstr;

import std.exception: enforce;

version(RBTREE) {
  import esdl.data.rbtree: RBTree;
}

template rand(N...) {
  static if(CheckRandParams!N) {
    struct rand
    {
      enum maxBounds = N;
      // this(int N) {
      // }
    }
  }
}

// Make sure that all the parameters are of type size_t
template CheckRandParams(N...) {
  static if(N.length > 0) {
    import std.traits;
    static if(!is(typeof(N[0]) == bool) && // do not confuse bool as size_t
	      is(typeof(N[0]) : size_t)) {
      static assert(N[0] != 0, "Can not have arrays with size 0");
      static assert(N[0] > 0, "Can not have arrays with negative size");
      enum bool CheckRecurse = CheckRandParams!(N[1..$]);
      enum bool CheckRandParams = CheckRecurse;
    }
    else {
      static assert(false, "Only positive integral values are allowed as array dimensions");
      enum bool CheckRandParams = false;
    }
  }
  else {
    enum bool CheckRandParams = true;
  }
}

abstract class _ESDL__ConstraintBase
{
  this(ConstraintEngine eng, string name, uint index) {
    _cstEng = eng;
    _name = name;
    _index = index;
  }

  protected bool _enabled = true;
  protected ConstraintEngine _cstEng;
  protected string _name;
  // index in the constraint Database
  protected uint _index;

  public bool isEnabled() {
    return _enabled;
  }

  public void enable() {
    _enabled = false;
  }

  public void disable() {
    _enabled = true;
  }

  public bdd getConstraintBDD() {
    return _cstEng._buddy.one();
  }

  public string name() {
    return _name;
  }
  
  abstract public CstBlock getCstExpr();
  
  public void applyMaxArrayLengthCst(ref bdd stageBDD, uint stage) {}
}

abstract class Constraint (string C) : _ESDL__ConstraintBase
{
  this(ConstraintEngine eng, string name, uint index) {
    super(eng, name, index);
  }

  static immutable string _constraint = C;
  // enum _parseTree = CstGrammar.parse(_constraint);
  // pragma(msg, _parseTree.capture);

  // Called by mixin to create functions out of parsed constraints
  static char[] constraintFoo(string CST)() {
    import esdl.data.cstx;
    return translate(CST);
  }

  debug(CONSTRAINTS) {
    pragma(msg, constraintFoo!C());
  }
};

class Constraint(string C, string NAME, T, S): Constraint!C
{
  T _outer;
  S _outerD;
  
  this(T t, S s, ConstraintEngine eng, string name, uint index) {
    super(eng, name, index);
    _outer = t;
    _outerD = s;
  }
  // This mixin writes out the bdd functions after parsing the
  // constraint string at compile time
  static if(NAME == "_esdl__lengthConstraint") {
    override public void applyMaxArrayLengthCst(ref bdd stageBDD, uint stage) {
      _esdl__initLengthCsts(_outerD, stageBDD, stage);
    }

    override public CstBlock getCstExpr() {
      auto cstExpr = new CstBlock;
      return cstExpr;
    }
  }
  else {
    mixin(constraintFoo!C());
  }
}

struct RandGen
{
  import std.random;
  import esdl.data.bvec;

  private Random _gen;

  private bvec!32 _bv;

  private ubyte _bi = 32;

  this(uint _seed) {
    _gen = Random(_seed);
  }

  void seed(uint _seed) {
    _gen.seed(_seed);
  }

  public bool flip() {
    if(_bi > 31) {
      _bi = 0;
      _bv = uniform!"[]"(0, uint.max, _gen);
    }
    return cast(bool) _bv[_bi++];
  }

  public double get() {
    return uniform(0.0, 1.0, _gen);
  }

  @property public T gen(T)() {
    static if(isIntegral!T) {
      T result = uniform!(T)(_gen);
      return result;
    }
    else static if(isBitVector!T) {
	T result;
	result.randomize(_gen);
	return result;
      }
      else {
	static assert(false);
      }
  }

  @property public auto gen(T1, T2)(T1 a, T2 b)
    if(isIntegral!T1 && isIntegral!T2) {
      return uniform(a, b, _gen);
    }
}


public class ConstraintEngine {
  // Keep a list of constraints in the class
  _ESDL__ConstraintBase cstList[];
  _ESDL__ConstraintBase arrayMaxLengthCst;
  // ParseTree parseList[];
  RandGen _rgen;
  Buddy _buddy;
  CstBlock _cstStmts;		// All the constraint statements from
				// the enabled constraints
  BddDomain[] _domains;

  this(uint seed) {
    _rgen.seed(seed);
  }

  public CstVecPrim[][] _randStages;

  // list of constraint statements to solve at a given stage
  public CstBddExpr[][] _cstStages;

  public void addCstStage(CstVecPrim[] vecs) {
    uint stage = cast(uint) _randStages.length;
    foreach(ref vec; vecs) {
      if(vec !is null) {
	if(vec.stage() is uint.max) {
	  if(stage >= _randStages.length) {
	    _randStages.length = stage + 1;
	  }
	  vec.stage = stage;
	  _randStages[stage] ~= vec;
	}
	if(stage != vec.stage()) { // need to merge stages
	  mergeCstStages(stage, vec.stage());
	  stage = vec.stage();
	}
      }
    }
  }

  public void mergeCstStages(uint fromStage, uint toStage) {
    if(fromStage == _randStages.length) {
      // fromStage has not been created yet
      return;
    }
    foreach(ref vec; _randStages[fromStage]) {
      vec.stage = toStage;
    }
    _randStages[toStage] ~= _randStages[fromStage];
    if(_randStages.length == fromStage + 1) {
      _randStages.length -= 1;
    }
    else {
      _randStages[fromStage] = null;
    }
  }

  void initDomains() {
    uint domIndex = 0;
    int[] domList;
    auto cstStmts = new CstBlock();	// start empty

    foreach(ref _ESDL__ConstraintBase cst; cstList) {
      cstStmts ~= cst.getCstExpr();
    }

    foreach(stmt; cstStmts._exprs) {
      foreach(vec; stmt.getPrims()) {
	if(vec.domIndex == uint.max) {
	  vec.domIndex = domIndex++;
	  domList ~= vec.bitcount;
	}
      }
    }

    _buddy.clearAllDomains();
    _domains = _buddy.extDomain(domList);

  }

  void solve() {
    import std.conv;
    // import std.stdio;
    // writeln("Solving BDD for number of contraints = ", cstList.length);

    if(_domains.length is 0) {
      initDomains();
    }

    _cstStmts = new CstBlock();	// start empty
    _cstStages = [];

    foreach(ref _ESDL__ConstraintBase cst; cstList) {
      if(cst.isEnabled()) {
	_cstStmts ~= cst.getCstExpr();
      }
    }
    foreach(stmt; _cstStmts._exprs) {
      addCstStage(stmt.getPrims());
    }

    _cstStages.length = _randStages.length;

    foreach(stmt; _cstStmts._exprs) {
      foreach(stage; stmt.getStages()) {
    	_cstStages[stage] ~= stmt;
      }
    }

    for (uint stage=0; stage!=_randStages.length; ++stage) {
      if(_randStages[stage].length !is 0) {

    	// initialize the bdd vectors
    	foreach(vec; _randStages[stage]) {
    	  if(vec.stage() == stage && vec.bddvec is null) {
	    vec.bddvec = _buddy.buildVec(_domains[vec.domIndex], vec.signed);
    	  }
    	}

    	// make the bdd tree
    	auto stmts = _cstStages[stage];

    	bdd stageBDD = _buddy.one();
    	foreach(stmt; stmts) {
	  stageBDD &= stmt.eval(stage, _buddy);
    	}

	// Apply the length constraint on the dynamic arrays wherever required
	// FIXME
	
	arrayMaxLengthCst.applyMaxArrayLengthCst(stageBDD, stage);
	
	double[uint] bddDist;
	stageBDD.satDist(bddDist);

	auto solution = stageBDD.randSatOne(this._rgen.get(),
					    bddDist);

	auto solVecs = solution.toVector();
	enforce(solVecs.length == 1,
		"Expecting exactly one solutions here; got: " ~
		to!string(solVecs.length));

	auto bits = solVecs[0];

    	foreach(vec; _randStages[stage]) {
	  vec.value = 0;	// init
	  foreach(uint i, ref j; stageBDD.getIndices(vec.domIndex)) {
	    if(bits[j] == 1) {
	      vec.value = vec.value + (1L << i);
	    }
	    if(bits[j] == -1) {
	      vec.value = vec.value + ((cast(ulong) _rgen.flip()) << i);
	    }
	  }
	  // vec.bddvec = null;
	}
	
      }
    }
  }

  void printSolution() {
    // import std.stdio;
    // writeln("There are solutions: ", _theBDD.satCount());
    // writeln("Distribution: ", dist);
    // auto randSol = _theBDD.randSat(randGen, dist);
    // auto solution = _theBDD.fullSatOne();
    // solution.printSetWith_Domains();
  }
}


template isRandomizable(T) {	// check if T is Randomizable
  import std.traits;
  import std.range;
  import esdl.data.bvec;
  static if(isArray!T) {
    enum bool isRandomizable = isRandomizable!(ElementType!T);
  }
  else
    static if(isIntegral!T || isBitVector!T) {
      enum bool isRandomizable = true;
    }
    else {
      bool isRandomizable = false;
    }
}

// Need to change this function to return only the count of @rand members
public size_t _esdl__countRands(size_t I=0, size_t C=0, T)(T t)
  if(is(T unused: RandomizableIntf)) {
    static if(I == t.tupleof.length) {
      static if(is(T B == super)
		&& is(B[0] : RandomizableIntf)
		&& is(B[0] == class)) {
	B[0] b = t;
	return _esdl__countRands!(0, C)(b);
      }
      else {
	return C;
      }
    }
    else {
      import std.traits;
      import std.range;
      // check for the integral members
      alias typeof(t.tupleof[I]) L;
      static if((isIntegral!L || isBitVector!L) &&
		findRandElemAttr!(I, t) != -1) {
	return _esdl__countRands!(I+1, C+1)(t);
      }
      else static if(isStaticArray!L && (isIntegral!(ElementType!L) ||
					 isBitVector!(ElementType!L)) &&
		     findRandElemAttr!(I, t) != -1) {
	  return _esdl__countRands!(I+1, C+1)(t);
	}
      else static if(isDynamicArray!L && (isIntegral!(ElementType!L) ||
					  isBitVector!(ElementType!L)) &&
		     findRandArrayAttr!(I, t) != -1) {
	  return _esdl__countRands!(I+1, C+1)(t);
	}
      // ToDo -- Fixme -- Add code for array randomization here
	else {
	  return _esdl__countRands!(I+1, C)(t);
	}
    }
  }

private template _esdl__randVar(string var) {
  import std.string;
  enum I = _esdl__randIndexof!(var);
  static if(I == -1) {
    enum string prefix = var;
    enum string suffix = "";
  }
  else {
    enum string prefix = var[0..I];
    enum string suffix = var[I..$];
  }
}

private template _esdl__randIndexof(string var, int index=0) {
  static if(index == var.length) {
    enum _esdl__randIndexof = -1;
  }
  else static if(var[index] == '.' ||
		 var[index] == '[' ||
		 var[index] == '(') {
      enum _esdl__randIndexof = index;
    }
    else {
      enum _esdl__randIndexof = _esdl__randIndexof!(var, index+1);
    }
}

interface RandomizableIntf
{
  static final string _esdl__randomizable() {
    return q{

      version(RBTREE) {
	import esdl.data.rbtree: RBTree;
	public RBTree!CstVecPrim _cstRands;
      }
      else {
	public CstVecPrim[] _cstRands;
      }

      public ConstraintEngine _esdl__cstEng;
      public uint _esdl__randSeed;

      public void seedRandom (int seed) {
	_esdl__randSeed = seed;
	if (_esdl__cstEng !is null) {
	  _esdl__cstEng._rgen.seed(seed);
	}
      }
      alias seedRandom srandom;	// for sake of SV like names

      public ConstraintEngine getCstEngine() {
	return _esdl__cstEng;
      }

      void pre_randomize() {}
      void post_randomize() {}

      Constraint! q{} _esdl__lengthConstraint;
    };
  }

  ConstraintEngine getCstEngine();
  void pre_randomize();
  void post_randomize();
}

class Randomizable: RandomizableIntf
{
  mixin(_esdl__randomizable());
}

T _new(T, Args...) (Args args) {
  version(EMPLACE) {
    import std.stdio, std.conv, core.stdc.stdlib;
    size_t objSize = __traits(classInstanceSize, T);
    void* tmp = core.stdc.stdlib.malloc(objSize);
    if (!tmp) throw new Exception("Memory allocation failed");
    void[] mem = tmp[0..objSize];
    T obj = emplace!(T, Args)(mem, args);
  }
  else {
    T obj = new T(args);
  }
  return obj;
}

void _delete(T)(T obj) {
  clear(obj);
  core.stdc.stdlib.free(cast(void*)obj);
}

public void _esdl__initCstEngine(T) (T t) {
  t._esdl__cstEng = new ConstraintEngine(t._esdl__randSeed);
  with(t._esdl__cstEng) {
    _buddy = _new!Buddy(400, 400);
    _esdl__initCsts(t, t);
  }
}

// I is the index within the class
// CI is the cumulative index -- starts from the most derived class
// and increases as we move up in the class hierarchy
void _esdl__initCsts(size_t I=0, size_t CI=0, T, S)(T t, S s)
  if(is(T: RandomizableIntf) && is(T == class) &&
     is(S: RandomizableIntf) && is(S == class)) {
    static if (I < t.tupleof.length) {
      _esdl__initCst!(I, CI)(t, s);
      _esdl__initCsts!(I+1, CI+1) (t, s);
    }
    else static if(is(T B == super)
		   && is(B[0] : RandomizableIntf)
		   && is(B[0] == class)) {
	B[0] b = t;
	_esdl__initCsts!(0, CI) (b, s);
      }
  }

void _esdl__initCst(size_t I=0, size_t CI=0, T, S) (T t, S s) {
  import std.traits;
  import std.conv;
  import std.string;

  auto l = t.tupleof[I];
  alias typeof(l) L;
  enum string NAME = chompPrefix (t.tupleof[I].stringof, "t.");
  static if (is (L f == Constraint!C, immutable (char)[] C)) {
    l = new Constraint!(C, NAME, T, S)(t, s, t._esdl__cstEng, NAME,
				       cast(uint) t._esdl__cstEng.cstList.length);
    static if(NAME == "_esdl__lengthConstraint") {
      t._esdl__cstEng.arrayMaxLengthCst = l;
    }
    else {
      t._esdl__cstEng.cstList ~= l;
    }
      
  }
  else {
    synchronized (t) {
      // Do nothing
    }
  }
}

// I is the index within the class
// CI is the cumulative index -- starts from the most derived class
// and increases as we move up in the class hierarchy
void _esdl__initLengthCsts(size_t I=0, size_t CI=0, size_t RI=0, T)(T t, ref bdd stageBDD, uint stage)
  if(is(T: RandomizableIntf) && is(T == class)) {
    static if (I < t.tupleof.length) {
      if(findRandAttr!(I, t)) {
	_esdl__initLengthCst!(I, RI)(t, stageBDD, stage);
	_esdl__initLengthCsts!(I+1, CI+1, RI+1) (t, stageBDD, stage);
      }
      else {
	_esdl__initLengthCsts!(I+1, CI+1, RI) (t, stageBDD, stage);
      }
    }
    else static if(is(T B == super)
		   && is(B[0] : RandomizableIntf)
		   && is(B[0] == class)) {
	B[0] b = t;
	_esdl__initLengthCsts!(0, CI) (b, stageBDD, stage);
      }
  }

void _esdl__initLengthCst(size_t I=0, size_t RI=0, T) (T t, ref bdd stageBDD, uint stage) {
  import std.traits;
  import std.conv;
  import std.string;

  auto l = t.tupleof[I];
  alias typeof(l) L;
  enum string NAME = chompPrefix (t.tupleof[I].stringof, "t.");
  static if(isDynamicArray!L) {
    enum RLENGTH = findRandArrayAttr!(I, t);
    auto cstVecPrim = t._cstRands[RI];
    if(cstVecPrim !is null && cstVecPrim.stage() == stage) {
      stageBDD &= cstVecPrim.eval(stage, stageBDD.root()).
	lte(_esdl__cstVecRand(RLENGTH, t).eval(stage, stageBDD.root()));
    }
  }
}

auto _esdl__namedApply(string VAR, alias F, size_t I=0, size_t CI=0, T)(T t)
if(is(T unused: RandomizableIntf) && is(T == class)) {
  static if (I < t.tupleof.length) {
    static if ("t."~_esdl__randVar!VAR.prefix == t.tupleof[I].stringof) {
      return F!(VAR, I, CI)(t);
    }
    else {
      return _esdl__namedApply!(VAR, F, I+1, CI+1) (t);
    }
  }
  else static if(is(T B == super)
		 && is(B[0] : RandomizableIntf)
		 && is(B[0] == class)) {
      B[0] b = t;
      return _esdl__namedApply!(VAR, F, 0, CI) (b);
    }
    else {
      static assert(false, "Can not map variable: " ~ VAR);
    }
 }

version(RBTREE) {
  void _esdl__setRands(size_t I=0, size_t CI=0, T)
    (T t, ref RBTree!CstVecPrim.Range values, ref RandGen rgen)
    if(is(T unused: RandomizableIntf) && is(T == class)) {
      static if (I < t.tupleof.length) {
	CstVecPrim value;
	if(values.empty) value = null;
	else value = values.front;
	if(value !is null && value._coordinates[0] == CI) {
	  values.popFront();
	  _esdl__setRands!(I+1, CI+1) (t, values, rgen);
	  static if(findRandElemAttr!(I, t) != -1) { // is @rand
	    alias typeof(t.tupleof[I]) L;
	    t.tupleof[I] = cast(L) value.value;
	  }
	}
	else {
	  static if(findRandElemAttr!(I, t) != -1) { // is @rand
	    alias typeof(t.tupleof[I]) L;
	    t.tupleof[I] = rgen.gen!L;
	  }
	  _esdl__setRands!(I+1, CI+1) (t, values, rgen);
	}
      }
      else static if(is(T B == super)
		     && is(B[0] : RandomizableIntf)
		     && is(B[0] == class)) {
	  B[0] b = t;
	  _esdl__setRands!(0, CI) (b, values, rgen);
	}
    }
}
 else {
   void _esdl__setRands(size_t I=0, size_t CI=0, size_t RI=0, T)
     (T t, CstVecPrim[] vecVals, ref RandGen rgen)
     if(is(T unused: RandomizableIntf) && is(T == class)) {
       import std.traits;
       static if (I < t.tupleof.length) {
	 alias typeof(t.tupleof[I]) L;
	 static if (isDynamicArray!L) {
	   enum RLENGTH = findRandArrayAttr!(I, t);
	   static if(RLENGTH != -1) { // is @rand
	     // make sure that there is only one dimension passed to @rand
	     static assert(findRandArrayAttr!(I, t, 1) == int.min);
	     // enum ATTRS = __traits(getAttributes, t.tupleof[I]);
	     // alias ATTRS[RLENGTH] ATTR;
	     auto vecVal = vecVals[RI];
	     if(vecVal is null) {
	       t.tupleof[I].length = rgen.gen(0, RLENGTH+1);
	     }
	     else {
	       t.tupleof[I].length = vecVal.value;
	     }
	     foreach(ref v; t.tupleof[I]) {
	       import std.range;
	       v = rgen.gen!(ElementType!L);
	     }
	     // t.tupleof[I] = rgen.gen!L;
	     // }
	     // else {
	     //   // t.tupleof[I] = cast(L) vecVal.value;
	     // }

	     // FIXME
	     // Once we put in foreach constraints, RI should get incremented too
	     // _esdl__setRands!(I+1, CI+1, RI) (t, vecVals, rgen);
	     _esdl__setRands!(I+1, CI+1, RI+1) (t, vecVals, rgen);
	   }
	   else {
	     _esdl__setRands!(I+1, CI+1, RI) (t, vecVals, rgen);
	   }
	 }
	 else {
	   static if(findRandElemAttr!(I, t) != -1) { // is @rand
	     static if(isStaticArray!L) {
	       foreach(ref v; t.tupleof[I]) {
		 import std.range;
		 v = rgen.gen!(ElementType!L);
	       }
	     }
	     else {
	       auto vecVal = vecVals[RI];
	       if(vecVal is null) {
		 t.tupleof[I] = rgen.gen!L;
	       }
	       else {
		 import esdl.data.bvec;
		 bvec!64 temp = vecVal.value();
		 t.tupleof[I] = cast(L) temp;
	       }
	       _esdl__setRands!(I+1, CI+1, RI+1) (t, vecVals, rgen);
	     }
	   }
	   else {
	     _esdl__setRands!(I+1, CI+1, RI) (t, vecVals, rgen);
	   }
	 }
       }
       else static if(is(T B == super)
		      && is(B[0] : RandomizableIntf)
		      && is(B[0] == class)) {
	   B[0] b = t;
	   _esdl__setRands!(0, CI, RI) (b, vecVals, rgen);
	 }
     }
 }

template findRandAttr(size_t I, alias t) {
  enum int randAttr =
    findRandElemAttrIndexed!(0, -1, __traits(getAttributes, t.tupleof[I]));
  enum int randsAttr =
    findRandArrayAttrIndexed!(0, -1, 0, __traits(getAttributes, t.tupleof[I]));
  enum bool findRandAttr = randAttr != -1 || randsAttr != -1;
}

template findRandElemAttr(size_t I, alias t) {
  enum int randAttr =
    findRandElemAttrIndexed!(0, -1, __traits(getAttributes, t.tupleof[I]));
  enum int randsAttr =
    findRandArrayAttrIndexed!(0, -1, 0, __traits(getAttributes, t.tupleof[I]));
  static assert(randsAttr == -1, "Illegal use of @rand!N");
  enum int findRandElemAttr = randAttr;
}

template findRandArrayAttr(size_t I, alias t, size_t R=0) {
  enum int randAttr =
    findRandElemAttrIndexed!(0, -1, __traits(getAttributes, t.tupleof[I]));
  enum int randsAttr =
    findRandArrayAttrIndexed!(0, -1, R, __traits(getAttributes, t.tupleof[I]));
  static assert(randAttr == -1,	"Illegal use of @rand");
  enum int findRandArrayAttr = randsAttr;
}

template findRandElemAttrIndexed(size_t C, int P, A...) {
  static if(A.length == 0) enum int findRandElemAttrIndexed = P;
  else static if(__traits(isSame, A[0], rand)) {
      static assert(P == -1, "@rand used twice in the same declaration");
      static if(A.length > 1)
	enum int findRandElemAttrIndexed = findRandElemAttrIndexed!(C+1, C, A[1..$]);
      else
	enum int findRandElemAttrIndexed = C;
    }
    else {
      enum int findRandElemAttrIndexed = findRandElemAttrIndexed!(C+1, P, A[1..$]);
    }
}

template findRandArrayAttrIndexed(size_t C, int P, size_t R, A...) {
  static if(A.length == 0) enum int findRandArrayAttrIndexed = P;
  else static if(is(A[0] unused: rand!M, M...)) {
      static assert(P == -1, "@rand used twice in the same declaration");
      static if(A.length > 1) {
	enum int findRandArrayAttrIndexed =
	  findRandArrayAttrIndexed!(C+1, C, R, A[1..$]);
      }
      else {
	static if(R < M.length && R >= 0) {
	  enum int findRandArrayAttrIndexed = M[R];
	}
	else {
	  enum int findRandArrayAttrIndexed = int.min;
	}
      }
    }
    else {
      enum int findRandArrayAttrIndexed =
	findRandArrayAttrIndexed!(C+1, P, R, A[1..$]);
    }
}

template isVarSigned(L) {
  import std.traits: isNumeric, isSigned;
  static if(isBitVector!L)
    enum bool isVarSigned = L.ISSIGNED;
  else static if(isNumeric!L)
	 enum bool isVarSigned = isSigned!L;
    else
      static assert(false, "isVarSigned: Can not determine sign of type " ~ typeid(L));
}

public bool randomize(T) (ref T t)
  if(is(T v: RandomizableIntf) &&
     is(T == class)) {
    import std.exception;
    import std.conv;

    version(RBTREE) {
      if(t._cstRands is null) {
	t._cstRands = new RBTree!CstVecPrim();
      }
    }
    else {
      if(t._cstRands.length is 0) {
	auto randCount = _esdl__countRands(t);
	t._cstRands.length = randCount;
      }
    }

    // Call the pre_randomize hook
    t.pre_randomize();

    // Initialize the constraint database if not already done
    if (t._esdl__cstEng is null) {
      _esdl__initCstEngine(t);
    }

    version(RBTREE) {
      auto values = t._cstRands[];
    }
    else {
      auto values = t._cstRands;
    }

    t._esdl__cstEng.solve();

    version(RBTREE) {
      values = t._cstRands[];
    }
    else {
      values = t._cstRands;
    }

    _esdl__setRands(t, values, t._esdl__cstEng._rgen);

    // Call the post_randomize hook
    t.post_randomize();
    return true;
  }




// All the operations that produce a BddVec
enum CstBinVecOp: byte
  {   AND,
      OR ,
      XOR,
      ADD,
      SUB,
      MUL,
      DIV,
      LSH,
      RSH,
      }

// All the operations that produce a Bdd
enum CstBinBddOp: byte
  {   LTH,
      LTE,
      GTH,
      GTE,
      EQU,
      NEQ,
      }

// proxy class for reading in the constraints lazily
// An abstract class that returns a vector on evaluation
abstract class CstVecExpr
{

  // get all the primary bdd vectors that constitute a given bdd expression
  abstract public CstVecPrim[] getPrims();

  // get the list of stages this expression should be avaluated in
  abstract public uint[] getStages();

  abstract public BddVec eval(uint stage, Buddy buddy);

  public CstVec2VecExpr opBinary(string op)(CstVecExpr other)
  {
    static if(op == "&") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.AND);
    }
    static if(op == "|") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.OR);
    }
    static if(op == "^") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.XOR);
    }
    static if(op == "+") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.ADD);
    }
    static if(op == "-") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.SUB);
    }
    static if(op == "*") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.MUL);
    }
    static if(op == "/") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.DIV);
    }
    static if(op == "<<") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.LSH);
    }
    static if(op == ">>") {
      return new CstVec2VecExpr(this, other, CstBinVecOp.RSH);
    }

  }

  public CstVec2BddExpr lth(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.LTH);
  }

  public CstVec2BddExpr lte(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.LTE);
  }

  public CstVec2BddExpr gth(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.GTH);
  }

  public CstVec2BddExpr gte(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.GTE);
  }

  public CstVec2BddExpr equ(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.EQU);
  }

  public CstVec2BddExpr neq(CstVecExpr other) {
    return new CstVec2BddExpr(this, other, CstBinBddOp.NEQ);
  }
}

class CstVecPrim: CstVecExpr
{
  abstract public long value();
  abstract public void value(long v);
  abstract public uint stage();
  abstract public void stage(uint s);
  abstract public uint domIndex();
  abstract public void domIndex(uint s);
  abstract public uint bitcount();
  abstract public bool signed();
  abstract public BddVec bddvec();
  abstract public void bddvec(BddVec b);
}

class CstVecRandArr: CstVecRand
{
  // Base class object shall be used for constraining the length part
  // of the array.

  // Also has an array of CstVecRand to map all the elements of the
  // array
  CstVecRand[] _elems;

  CstVecRand opIndex(size_t idx) {
    return _elems[idx];
  }

  void opIndexAssign(CstVecRand c, size_t idx) {
    _elems[idx] = c;
  }

  public this(string name, long value, bool signed,
	      uint bitcount, bool isRand) {
    super(name, value, signed, bitcount, isRand);
    static uint id;
    _name = name;
    _value = value;
    _signed = signed;
    _bitcount = bitcount;
    _isRand = isRand;
  }
}

class CstVecRand: CstVecPrim
{
  BddVec _bddvec;
  uint _domIndex = uint.max;
  long _value;
  uint _bitcount;
  uint _stage = uint.max;
  bool _signed;
  bool _isRand;
  version(RBTREE) {
    uint[] _coordinates;
  }
  string _name;

  version(RBTREE) {
    public this(string name, long value, bool signed,
		uint bitcount, bool isRand, uint[] coordinates) {
      static uint id;
      _name = name;
      _value = value;
      _signed = signed;
      _bitcount = bitcount;
      _isRand = isRand;
      _coordinates = coordinates;
    }

    private void setCoord(uint[] coordinates) {
      _coordinates = coordinates;
    }

  }
  else {
    public this(string name, long value, bool signed,
		uint bitcount, bool isRand) {
      static uint id;
      _name = name;
      _value = value;
      _signed = signed;
      _bitcount = bitcount;
      _isRand = isRand;
    }
  }

  public override CstVecPrim[] getPrims() {
    CstVecPrim[] _prims;
    if(_isRand) _prims = [this];
    return _prims;
  }

  public override uint[] getStages() {
    uint[] stages;
    if(_isRand) stages = [this.stage()];
    return stages;
  }

  public override BddVec eval(uint stage, Buddy buddy) {
    if(this._isRand && stage == _stage) {
      return _bddvec;
    }
    else if((! this._isRand) ||
	    this._isRand && stage < _stage) { // work with the value
      return buddy.buildVec(_value);
    }
    else {
      assert(false, "Constraint evaluation in wrong stage");
    }
  }

  public override long value() {
    return _value;
  }

  public override void value(long v) {
    _value = v;
  }

  public override uint stage() {
    return _stage;
  }

  public override void stage(uint s) {
    _stage = s;
  }

  public override uint domIndex() {
    return _domIndex;
  }

  public override void domIndex(uint s) {
    _domIndex = s;
  }

  public override uint bitcount() {
    return _bitcount;
  }

  public override bool signed() {
    return _signed;
  }

  public override BddVec bddvec() {
    return _bddvec;
  }

  public override void bddvec(BddVec b) {
    _bddvec = b;
  }

  public T to(T)()
    if(is(T == string)) {
      import std.conv;
      if(_isRand) {
	return "RAND-" ~ "#" ~ _name ~ "-" ~ _value.to!string();
      }
      else {
	return "VAL#" ~ _name ~ "-" ~ _value.to!string();
      }
    }

  public override string toString() {
    return this.to!string();
  }

  version(RBTREE) {
    public int opCmp(T)(T other) if(is(T == CstVecPrim)) {
      size_t len = min(this._coordinates.length, other._coordinates.length);
      for (size_t i = 0; i != len; ++i)
	{
	  if(this._coordinates[i] > other._coordinates[i]) return 1;
	  if(this._coordinates[i] < other._coordinates[i]) return -1;
	}
      if(this._coordinates.length > other._coordinates.length) return 1;
      if(this._coordinates.length < other._coordinates.length) return -1;
      return 0;
    }
  }

  // public BddVec eval() {
  //   return null;
  // }
}

class CstVecConst: CstVecExpr
{
  long _value;			// the value of the constant
  bool _signed;

  public this(long value, bool signed) {
    _value = value;
    _signed = signed;
  }

  public override CstVecPrim[] getPrims() {
    return [];
  }

  public override uint[] getStages() {
    return [];
  }

  public override BddVec eval(uint stage, Buddy buddy) {
    return buddy.buildVec(_value);
  }
}

// This class would hold two(bin) vector nodes and produces a vector
// only after processing those two nodes
class CstVec2VecExpr: CstVecExpr
{
  CstVecExpr _lhs;
  CstVecExpr _rhs;
  CstBinVecOp _op;

  public override CstVecPrim[] getPrims() {
    return _lhs.getPrims() ~ _rhs.getPrims();
  }

  public override uint[] getStages() {
    import std.exception;
    import std.algorithm: max;

    enforce(_lhs.getStages.length <= 1 &&
	    _rhs.getStages.length <= 1);

    if(_lhs.getStages.length is 0) return _rhs.getStages;
    else if(_rhs.getStages.length is 0) return _lhs.getStages;
    else {
      uint stage = max(_lhs.getStages[0], _rhs.getStages[0]);
      return [stage];
    }
  }

  public override BddVec eval(uint stage, Buddy buddy) {
    import std.conv;

    BddVec vec;

    auto lvec = _lhs.eval(stage, buddy);
    auto rvec = _rhs.eval(stage, buddy);

    final switch(_op) {
    case CstBinVecOp.AND: return lvec &  rvec;
    case CstBinVecOp.OR:  return lvec |  rvec;
    case CstBinVecOp.XOR: return lvec ^  rvec;
    case CstBinVecOp.ADD: return lvec +  rvec;
    case CstBinVecOp.SUB: return lvec -  rvec;
    case CstBinVecOp.MUL: return lvec *  rvec;
    case CstBinVecOp.DIV: return lvec /  rvec;
    case CstBinVecOp.LSH: return lvec << rvec;
    case CstBinVecOp.RSH: return lvec >> rvec;
    }
  }

  public this(CstVecExpr lhs, CstVecExpr rhs, CstBinVecOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
  }

}

class CstNotVecExpr: CstVecExpr
{
}

enum CstBddOp: byte
  {   AND,
      OR ,
      IMP,
      }

abstract class CstBddExpr
{

  abstract public CstVecPrim[] getPrims();

  abstract public uint[] getStages();

  abstract public bdd eval(uint stage, Buddy buddy);

  public CstBdd2BddExpr opBinary(string op)(CstBddExpr other)
  {
    static if(op == "&") {
      return new CstBdd2BddExpr(this, other, CstBddOp.AND);
    }
    static if(op == "|") {
      return new CstBdd2BddExpr(this, other, CstBddOp.OR);
    }
  }

  public CstBdd2BddExpr imp(CstBddExpr other)
  {
    return new CstBdd2BddExpr(this, other, CstBddOp.IMP);
  }

}

class CstBdd2BddExpr: CstBddExpr
{
  CstBddExpr _lhs;
  CstBddExpr _rhs;
  CstBddOp _op;

  public override CstVecPrim[] getPrims() {
    return _lhs.getPrims() ~ _rhs.getPrims();
  }

  public override uint[] getStages() {
    uint[] stages;

    foreach(lstage; _lhs.getStages) {
      bool already = false;
      foreach(stage; stages) {
	if(stage == lstage) {
	  already = true;
	}
      }
      if(! already) stages ~= lstage;
    }
    foreach(rstage; _rhs.getStages) {
      bool already = false;
      foreach(stage; stages) {
	if(stage == rstage) {
	  already = true;
	}
      }
      if(! already) stages ~= rstage;
    }

    return stages;
  }

  override public bdd eval(uint stage, Buddy buddy) {
    import std.conv;
    auto lvec = _lhs.eval(stage, buddy);
    auto rvec = _rhs.eval(stage, buddy);

    final switch(_op) {
    case CstBddOp.AND: return lvec &  rvec;
    case CstBddOp.OR:  return lvec |  rvec;
    case CstBddOp.IMP: return lvec.imp(rvec);
    }
  }

  public this(CstBddExpr lhs, CstBddExpr rhs, CstBddOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
  }
}

class CstIteBddExpr: CstBddExpr
{
}

class CstVec2BddExpr: CstBddExpr
{
  CstVecExpr _lhs;
  CstVecExpr _rhs;
  CstBinBddOp _op;

  public override uint[] getStages() {
    import std.exception;
    import std.algorithm: max;
    enforce(_lhs.getStages.length <= 1 &&
	    _rhs.getStages.length <= 1);

    if(_lhs.getStages.length is 0) return _rhs.getStages;
    else if(_rhs.getStages.length is 0) return _lhs.getStages;
    else {
      uint stage = max(_lhs.getStages[0], _rhs.getStages[0]);
      return [stage];
    }
  }

  public override CstVecPrim[] getPrims() {
    return _lhs.getPrims() ~ _rhs.getPrims();
  }

  override public bdd eval(uint stage, Buddy buddy) {
    import std.conv;
    auto lvec = _lhs.eval(stage, buddy);
    auto rvec = _rhs.eval(stage, buddy);

    final switch(_op) {
    case CstBinBddOp.LTH: return lvec.lth(rvec);
    case CstBinBddOp.LTE: return lvec.lte(rvec);
    case CstBinBddOp.GTH: return lvec.gth(rvec);
    case CstBinBddOp.GTE: return lvec.gte(rvec);
    case CstBinBddOp.EQU: return lvec.equ(rvec);
    case CstBinBddOp.NEQ: return lvec.neq(rvec);
    }
  }

  public this(CstVecExpr lhs, CstVecExpr rhs, CstBinBddOp op) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
  }
}

class CstNotBddExpr: CstBddExpr
{
}

class CstBlock: CstBddExpr
{
  CstBddExpr _exprs[];

  public override CstVecPrim[] getPrims() {
    CstVecPrim[] prims;

    foreach(expr; _exprs) {
      prims ~= expr.getPrims();
    }

    return prims;
  }

  public override uint[] getStages() {
    uint[] stages;

    foreach(expr; _exprs) {
      foreach(lstage; expr.getStages) {
	bool already = false;
	foreach(stage; stages) {
	  if(stage == lstage) {
	    already = true;
	  }
	}
	if(! already) stages ~= lstage;
      }
    }

    return stages;
  }

  override public bdd eval(uint stage, Buddy buddy) {
    assert(false, "eval not implemented for CstBlock");
  }

  public void opOpAssign(string op)(CstBddExpr other)
    if(op == "~") {
      _exprs ~= other;
    }

  public void opOpAssign(string op)(CstBlock other)
    if(op == "~") {
      foreach(expr; other._exprs) {
      _exprs ~= expr;
      }
    }
}

auto _esdl__randNamedApply(string VAR, alias F, size_t I=0,
			   size_t CI=0, size_t RI=0, T)(T t)
if(is(T unused: RandomizableIntf) && is(T == class)) {
  static if (I < t.tupleof.length) {
    static if ("t."~_esdl__randVar!VAR.prefix == t.tupleof[I].stringof) {
      return F!(VAR, I, CI, RI)(t);
    }
    else {
      static if(findRandElemAttr!(I, t) != -1) {
	return _esdl__randNamedApply!(VAR, F, I+1, CI+1, RI+1) (t);
      }
      else {
	return _esdl__randNamedApply!(VAR, F, I+1, CI+1, RI) (t);
      }
    }
  }
  else static if(is(T B == super)
		 && is(B[0] : RandomizableIntf)
		 && is(B[0] == class)) {
      B[0] b = t;
      return _esdl__randNamedApply!(VAR, F, 0, CI, RI) (b);
    }
    else {
      static assert(false, "Can not map variable: " ~ VAR);
    }
 }

private size_t _esdl__cstDelimiter(string name) {
  foreach(i, c; name) {
    if(c is '.' || c is '[') {
      return i;
    }
  }
  return name.length;
}

public CstVecExpr _esdl__cstVecRand(string VAR, T)(ref T t)
  if(is(T f: RandomizableIntf) && is(T == class)) {
    enum IDX = _esdl__cstDelimiter(VAR);
    enum LOOKUP = VAR[0..IDX];
    static if(IDX == VAR.length) {
      return _esdl__randNamedApply!(LOOKUP, _esdl__cstVecRand)(t);
    }
    else static if(VAR[IDX..$] == ".length") {
	return _esdl__randNamedApply!(LOOKUP, _esdl__cstVecRandArrLength)(t);
    }
    else static if(VAR[IDX] == '.') {
      // hierarchical constraints -- not implemented yet
    }
    else static if(VAR[IDX] == '[') {
	// hmmmm
      }
}

public CstVecExpr _esdl__cstVecRand(string VAR, size_t I,
				size_t CI, size_t RI, T)(ref T t) {
  import std.traits: isNumeric;
  static CstVecExpr dummyCstWithCoord;

  // need to know the size and sign for creating a bddvec
  alias typeof(t.tupleof[I]) L;
  static assert(isNumeric!L || isBitVector!L);

  static if(isVarSigned!L) bool signed = true;
  else                     bool signed = false;

  static if(isNumeric!L)        uint bitcount = L.sizeof * 8;
  else static if(isBitVector!L) uint bitcount = L.SIZE;
    else static assert(false, "Only numeric or bitvector expression"
		       "are allowed in constraint expressions");

  static if(findRandElemAttr!(I, t) == -1) {
    version(RBTREE) {
      uint[] coordinates = [cast(uint) CI];
      auto cstVecPrim = new CstVecRand(t.tupleof[I].stringof, t.tupleof[I],
				       signed, bitcount, false, coordinates);
    }
    else {
      auto cstVecPrim = new CstVecRand(t.tupleof[I].stringof, t.tupleof[I],
				       signed, bitcount, false);
    }
  }
  else {
    version(RBTREE) {
      uint[] coordinates = [cast(uint) CI];
      if(dummyCstWithCoord is null) {
	dummyCstWithCoord = new CstVecRand("dummyCstWithCoord", t.tupleof[I],
					   signed, bitcount, true, coordinates);
      }
      else {
	dummyCstWithCoord.setCoord(coordinates);
      }

      auto cstVecPrim = t._cstRands.find(dummyCstWithCoord);
      if(cstVecPrim is null) {
	cstVecPrim = new CstVecRand(t.tupleof[I].stringof, t.tupleof[I],
				    signed, bitcount, true, coordinates);
	t._cstRands.insert(cstVecPrim);
      }
    }
    else {
      auto cstVecPrim = t._cstRands[RI];
      if(cstVecPrim is null) {
	cstVecPrim = new CstVecRand(t.tupleof[I].stringof, t.tupleof[I],
				    signed, bitcount, true);
	t._cstRands[RI] = cstVecPrim;
      }
    }
  }
  return cstVecPrim;
}

public CstVecExpr _esdl__cstVecRandArrLength(string VAR, size_t I,
					     size_t CI, size_t RI, T)(ref T t) {
  import std.traits;
  import std.range;

  // need to know the size and sign for creating a bddvec
  alias typeof(t.tupleof[I]) L;
  static assert(isArray!L);
  alias ElementType!L E;
  static assert(isNumeric!E || isBitVector!E);
    
  bool signed = false;		// array size can not be negative

  uint bitcount = 32;		// allow arrays only upto size 4G

  enum RLENGTH = findRandArrayAttr!(I, t);

  static if(RLENGTH != -1) { // @rand!N form
    static assert(isDynamicArray!L);
    auto cstVecPrim = t._cstRands[RI];
    if(cstVecPrim is null) {
      cstVecPrim = new CstVecRandArr(t.tupleof[I].stringof, RLENGTH,
				     signed, bitcount, true);
      t._cstRands[RI] = cstVecPrim;
    }
  }
  else {
    auto cstVecPrim = new CstVecRand(t.tupleof[I].stringof, t.tupleof[I],
				     signed, bitcount, false);
  }
  return cstVecPrim;
}

public CstVecConst _esdl__cstVecRand(INT, T)(INT var, ref T t)
  if(isIntegral!INT && is(T f: RandomizableIntf) && is(T == class)) {
    return new CstVecConst(var, isVarSigned!INT);
  }
