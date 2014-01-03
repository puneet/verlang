// Written in the D programming language.

/**
Copyright: Copyright Digital Mars 2007 - 2011.
           Coverify Systems Technology 2011 - 2013
License:   Distributed under the Boost Software License, Version 1.0.
           (See accompanying file LICENSE_1_0.txt or copy at
           http://www.boost.org/LICENSE_1_0.txt)
Authors:   $(WEB digitalmars.com, Walter Bright),
	   $(WEB erdani.org, Andrei Alexandrescu),
	   Puneet Goel <puneet@coverify.com>
*/

// This file is part of esdl.

module esdl.data.bvec;

import std.conv;
import std.string;
import std.metastrings;
import std.traits;
import std.format;
import std.bitmanip;
// required for ElementType
import std.range;
import core.bitop;
import core.exception;

// required for appender
// import std.array;

// SIZE_T used as parameter int type everywhere in this module
// I made this a separate variable since DMD was earlier broken for
// size_t -- worked for int
alias size_t SIZE_T;
alias BitArray barray;

version(BVEC_NOCHECK) {
  private enum bool SAFE_BVEC=false;
  private enum bool NO_CHECK_SIZE=true;
}
 else {
   private enum bool SAFE_BVEC=true;
   private enum bool NO_CHECK_SIZE=false;
 }

template isBitVector(T) {
  static if(is(T unused == vec!(S, L, N), bool S, bool L, N...))
    enum bool isBitVector = true;
  else
    enum bool isBitVector = false;
}

template BitLength(T) {
  static if(isBitVector!T)      enum size_t BitLength = T.SIZE;
  else static if(is(T == bool)) enum size_t BitLength = 1;
    else                        enum size_t BitLength = 8 * T.sizeof;
}

public class LogicError: Error
{
  this(string err, string file, size_t line, Throwable next = null) {
    super(err, file, line);
  }
}

// gets called at compile time
private string removeDelimiters(string bits) {
  if(bits.length == 0) return "";
  if(bits.length == 1) {
    if(bits == " " || bits == "_") return "";
    else return bits;
  }
  else
    return removeDelimiters(bits[0..$/2]) ~
      removeDelimiters(bits[$/2..$]);
}

private string octalToLogic(string value) {
  string result; // = "0b";
  // chomp the first 0
  // import std.exception;
  // enforce(value[0..1] == "0");
  string v = value; // value[1..$];
  foreach(c; v) {
    switch(c) {
    case '0': result ~= "000"; break;
    case '1': result ~= "001"; break;
    case '2': result ~= "010"; break;
    case '3': result ~= "011"; break;
    case '4': result ~= "100"; break;
    case '5': result ~= "101"; break;
    case '6': result ~= "110"; break;
    case '7': result ~= "111"; break;
    case 'z':
    case 'Z': result ~= "ZZZ"; break;
    case 'x':
    case 'X': result ~= "XXX"; break;
    default: assert(false, "illegal character: " ~ c);
    }
  }
  return result;
}

package T logicToOctal(T)(T value) if(is(T == string) || is(T == char[])) {
  // zfill to a multiple of 3
  char[] v = rightJustify(value, cast(int)(3*((value.length + 2)/3)), '0');
  char[] result; //  = ['0'];
  for(size_t i = 0; i != v.length/3; ++i) {
    bool isX = false;
    bool isZ = false;
    foreach(b; v[3*i..3*(i+1)]) {
      if(b == 'Z' || b == 'z') isZ = true;
      if(b == 'X' || b == 'x') isX = true;
    }
    if(isX) result ~= 'X';
    else if(isZ)
      if(v[3*i..3*(i+1)] == "ZZZ" ||
	 v[3*i..3*(i+1)] == "zzz") result ~= 'Z';
      else result ~= 'X';
    else {
      switch(v[3*i..3*(i+1)]) {
      case "000": result ~= '0'; break;
      case "001": result ~= '1'; break;
      case "010": result ~= '2'; break;
      case "011": result ~= '3'; break;
      case "100": result ~= '4'; break;
      case "101": result ~= '5'; break;
      case "110": result ~= '6'; break;
      case "111": result ~= '7'; break;
      default: assert(false, "illegal octal sequence: " ~ v[i..i+3]);
      }
    }
  }
  return cast(T) result;
 }

// works for binary, octal and hex string literals
private bool isStr4State(string str) {
  if(str.length == 0) return false;
  if(str.length == 1)
    if(str[0] == 'x' || str[0] == 'X' || str[0] == 'z' || str[0] == 'Z')
      return true;
    else
      return false;
  else
    return isStr4State(str[0..$/2]) || isStr4State(str[$/2..$]);
}

// works for binary, octal and hex string literals
private string extractBits(bool isA, SIZE_T RADIX)(string str) {
  string v = removeDelimiters(str);
  string value = "";
  static if(RADIX == 2) { // binary
    value ~= "0b";
    foreach(char digit; v) {
      switch(digit) {
      case '0','1':
	static if(isA) value ~= digit;
	else           value ~= '0';
	break;
      case 'Z','z':
	static if(isA) value ~= '0';
	else value ~= '1';
	break;
      case 'X','x':
	static if(isA) value ~= '1';
	else value ~= '1';
	break;
      default: assert(false,
		      "Unrecognised string literal format: " ~ str);
      }
    }
    return value;
  }
  else
    static if(RADIX == 16) { // hexadecimal
      value ~= "0x";
      foreach(char digit; v) {
	switch(digit) {
	case
	  '0','1','2','3','4','5','6','7','8','9',
	  'A','B','C','D','E','F',
	  'a','b','c','d','e','f':
	  static if(isA) value ~= digit;
	  else           value ~= '0';
	  break;
	case 'Z','z':
	  static if(isA) value ~= '0';
	  else value ~= 'F';
	  break;
	case 'X','x':
	  static if(isA) value ~= 'F';
	  else value ~= 'F';
	  break;
	default: assert(false,
			"Unrecognised string literal format: " ~ str);
	}
      }
      return value;
    }
    else
      static if(RADIX == 8) { // octal
	value ~= "0";
	foreach(char digit; v) {
	  switch(digit) {
	  case
	    '0','1','2','3','4','5','6','7':
	    static if(isA) value ~= digit;
	    else           value ~= '0';
	    break;
	  case 'Z','z':
	    static if(isA) value ~= '0';
	    else value ~= '7';
	    break;
	  case 'X','x':
	    static if(isA) value ~= '7';
	    else value ~= '7';
	    break;
	  default: assert(false,
			  "Unrecognised string literal format: " ~ str);
	  }
	}
	return value;
      }
  assert(false, "Unrecognised string literal format: " ~ str);
}

// works for binary, octal and hex string literals
// Returns a string with compilable array elements.
private string stringToBits(string value,
			    size_t bytesPerWord,
			    size_t numWords) {
  string v = removeDelimiters(value);
  string code = "";

  if(v[0..2] == "0b" || v[0..2] == "0B") { // binary
    v = v[2..$];
    code ~= "[";
    for(size_t i = 0; i != numWords; ++i) {
      // If the string is shorter than numWords, fill in zeros
      if(v.length == 0) {	// all processed
	code ~= "0";
      }
      else if(v.length >= 8*bytesPerWord) {
	code ~= "cast(store_t) 0b" ~ v[$-8*bytesPerWord..$];
	v = v[0..$-8*bytesPerWord];
      }
      else {
	code ~= "cast(store_t) 0b" ~ v;
	v = "";
      }
      if(i != numWords - 1) {	// not the last word
	code ~= ", ";
      }
      else {			// last word
	code ~= "]";
      }
    }
    return code;
  }
  else if(v[0..2] == "0x" || v[0..2] == "0X") { // hexadecimal
    v = v[2..$];
    code ~= "[";
    for(size_t i = 0; i != numWords; ++i) {
      if(v.length == 0) {	// all processed
	code ~= "0";
      }
      else if(v.length >= 2*bytesPerWord) {
	code ~= "cast(store_t) 0x" ~ v[$-2*bytesPerWord..$];
	v = v[0..$-2*bytesPerWord];
      }
      else {
	code ~= "cast(store_t) 0x" ~ v;
	v = "";
      }
      if(i != numWords - 1) {	// not the last word
	code ~= ", ";
      }
      else {			// last word
	code ~= "]";
      }
    }
    return code;
  }
  else if(v[0..1] == "0") {	// octal
    return stringToBits(octalToLogic(v), bytesPerWord, numWords);
  }
  assert(false, "Unrecognised string literal format: " ~ value);
}


private SIZE_T stringBitSize(string value, SIZE_T RADIX) {
  string v = removeDelimiters(value);
  return cast(SIZE_T)(_log2(RADIX) * v.length);
}

private ubyte _log2(size_t n) {
  if(n == 1) return 0;
  else return cast(ubyte)(1 + _log2(n/2));
}

private template VecParams(SIZE_T SIZE, bool S=true) {
  static if(SIZE <= 8) {
    static if(S) {
      private alias byte StoreT;
      private alias byte ValueT;
    }
    else {
      private alias ubyte StoreT;
      private alias ubyte ValueT;
    }
  }
  else
    static if(SIZE <= 16) {
      static if(S) {
	private alias short StoreT;
	private alias short ValueT;
      }
      else {
	private alias ushort StoreT;
	private alias ushort ValueT;
      }
    }
    else
      static if(SIZE <= 32 || size_t.sizeof*8 == 32) {
	static if(SIZE <= 32) {
	  static if(S) {
	    private alias int StoreT;
	    private alias int ValueT;
	  }
	  else {
	    private alias uint StoreT;
	    private alias uint ValueT;
	  }
	}
	else {
	  static if(S) {
	    private alias int  StoreT;
	    private alias long ValueT;
	  }
	  else {
	    private alias uint  StoreT;
	    private alias ulong ValueT;
	  }
	}
      }
      else {
	static if(S) {
	  private alias long StoreT;
	  private alias long ValueT;
	}
	else {
	  private alias ulong StoreT;
	  private alias ulong ValueT;
	}
      }

  // Word Size -- bits in a word
  private enum SIZE_T WORDSIZE = 8*StoreT.sizeof;
  // _aval size(in words)
  private enum SIZE_T STORESIZE =(8*StoreT.sizeof + SIZE - 1)/(8*StoreT.sizeof);
  // Word size of the Most Significant word
  // Make sure that MSWSIZE is never 0(is equal to WORDSIZE instead)
  private enum SIZE_T MSWSIZE =((SIZE-1) % WORDSIZE) + 1;
  static if(MSWSIZE == WORDSIZE)	// All ones
    // Shift by WORDSIZE(word size) is erroneous
    {
      private enum StoreT UMASK =(cast(StoreT) -1);
      private enum StoreT SMASK = ~UMASK;
    }
  else {
    private enum StoreT UMASK =((cast(StoreT) 1) << MSWSIZE) - 1;
    private enum StoreT SMASK = ~UMASK;
  }
}

// Make sure that all the parameters are of type size_t
template CheckVecParams(N...) {
  static if(N.length > 0) {
    import std.traits;
    static if(!is(typeof(N[0]) == bool) && // do not confuse bool as size_t
	      is(typeof(N[0]) : size_t)) {
      static assert(N[0] != 0, "Can not have vectors with size 0");
      enum bool CheckVecParams = CheckVecParams!(N[1..$]);
    }
    else {
      enum bool CheckVecParams = false;
    }
  }
  else {
    enum bool CheckVecParams = true;
  }
}

template VecSize(SIZE_T L=1, N...) {
  static if(N.length > 0) {
    import std.traits;
    enum SIZE_T VecSize = VecSize!(L*N[0], N[1..$]);
  }
  else {
    enum SIZE_T VecSize = L;
  }
}

// One of the reasons for creating this function is that D operates
// A.opCmp(B) as well as B.opCmp(B) in case both are defined. We want
// to avoid that situation.
template _vecCmpT(T, U)	// return true if T >= U
if(isBitVector!T && isBitVector!U) {
  static if     (T.IS4STATE && !U.IS4STATE)  enum bool _vecCmpT = true;
  else static if(!T.IS4STATE && U.IS4STATE)  enum bool _vecCmpT = false;
  else static if(T.ISSIGNED && !U.ISSIGNED)  enum bool _vecCmpT = true;
  else static if(!T.ISSIGNED && U.ISSIGNED)  enum bool _vecCmpT = false;
  else static if(T.SIZE > U.SIZE)            enum bool _vecCmpT = true;
  else static if(T.SIZE < U.SIZE)            enum bool _vecCmpT = false;
  else static if(T.ELEMSIZE > U.ELEMSIZE)    enum bool _vecCmpT = true;
  else static if(T.ELEMSIZE < U.ELEMSIZE)    enum bool _vecCmpT = false;
  else static if(T.MULTIDIM && !U.MULTIDIM)  enum bool _vecCmpT = true;
  else static if(!T.MULTIDIM && U.MULTIDIM)  enum bool _vecCmpT = false;
  else static if(!T.MULTIDIM && !U.MULTIDIM) enum bool _vecCmpT = true;
    else enum bool _vecCmpT = true; // _vecCmpT(T.ELEMTYPE, U.ELEMTYPE);
 }

template vec(T, U, string OP)
if(isBitVector!T && isBitVector!U) {
  static if(T.ISSIGNED && U.ISSIGNED) {enum bool S = true;}
  else                                {enum bool S = false;}
  static if(T.IS4STATE || U.IS4STATE) {enum bool L = true;}
  else                                {enum bool L = false;}
  static if(OP == "|" || OP == "&" || OP == "^") {
    static if(T.SIZE > U.SIZE)        {enum SIZE_T N = T.SIZE;}
    else                              {enum SIZE_T N = U.SIZE;}
  }
  static if(OP == "COMPARE") {	// for operands of opCmp
    static if(T.SIZE > U.SIZE)        {enum SIZE_T N = T.SIZE;}
    else                              {enum SIZE_T N = U.SIZE;}
  }
  static if(OP == "+" || OP == "-") {
    static if(T.SIZE > U.SIZE)        {enum SIZE_T N = T.SIZE + 1;}
    else                              {enum SIZE_T N = U.SIZE + 1;}
  }
  static if(OP == "*")                {enum SIZE_T N = T.SIZE + U.SIZE;}
  static if(OP == "/")                {enum SIZE_T N = T.SIZE;}
  alias vec!(S, L, N) vec;
 }

struct vec(bool S, bool L, string VAL, SIZE_T RADIX) {
  enum SIZE_T SIZE = stringBitSize(VAL, RADIX);

  private alias VecParams!(SIZE,S).StoreT store_t;

  enum SIZE_T   STORESIZE  = VecParams!(SIZE,S).STORESIZE;
  enum SIZE_T   WORDSIZE   = VecParams!(SIZE,S).WORDSIZE;
  enum store_t UMASK      = VecParams!(SIZE,S).UMASK;
  enum store_t SMASK      = VecParams!(SIZE,S).SMASK;
  enum bool    IS4STATE   = L;
  enum bool    ISSIGNED   = S;

  private store_t[STORESIZE] _aval = void;
  public bool aValSigned() {
    static if(S) {
      if((this._aval[$-1] &
	  (cast(store_t) 1) <<((SIZE-1) % WORDSIZE)))
	return true;
      else
	return false;
    }
    else
      return false;
  }

  static if(L) {
    private store_t[STORESIZE] _bval = void;
    public bool bValSigned() {
      static if(S) {
	if((this._bval[$-1] &
	    (cast(store_t) 1) <<((SIZE-1) % WORDSIZE)))
	  return true;
	else
	  return false;
      }
      else
	return false;
    }
  }

  public this(int dummy) {
    // sign extension
    _aval = mixin(stringToBits(extractBits!(true, RADIX)(VAL),
			       store_t.sizeof, STORESIZE));
    static if(L) {
      _bval = mixin(stringToBits(extractBits!(false, RADIX)(VAL),
				 store_t.sizeof, STORESIZE));
      if(this.aValSigned) {
	_aval[$-1] |= SMASK;
      }
      else {
	_aval[$-1] &= UMASK;
      }
      if(this.bValSigned) {
	_bval[$-1] |= SMASK;
      }
      else {
	_bval[$-1] &= UMASK;
      }
    }
    else {
      foreach(i, n;(mixin(stringToBits(extractBits!(false, RADIX)(VAL),
				       store_t.sizeof, STORESIZE)))) {
	_aval[i] &= ~n;
      }
      if(this.aValSigned) {
	_aval[$-1] |= SMASK;
      }
      else {
	_aval[$-1] &= UMASK;
      }
    }
  }

  public T opCast(T)()
    if(isBitVector!T) {
      enum bool _L = T.IS4STATE;
      enum bool _S = T.ISSIGNED;
      T result;
      static if(T.STORESIZE <= STORESIZE) {
	for(size_t i=0; i != T.STORESIZE; ++i) {
	  result._aval[i] = cast(T.store_t) this._aval[i];
	  static if(_L) {
	    static if(L) result._bval[i] =
			   cast(T.store_t) this._bval[i];
	    else          result._bval[i] = 0;
	  }
	  else {
	    // X and Z values reduce to 0
	    static if(L) result._aval[i] &=
			   ~(cast(T.store_t) this._bval[i]);
	  }
	}
	// sign extension
	if(result.aValSigned) result._aval[$-1] |= T.SMASK;
	else                  result._aval[$-1] &= T.UMASK;
	static if(_L) {
	  if(result.bValSigned) result._bval[$-1] |= T.SMASK;
	  else                  result._bval[$-1] &= T.UMASK;
	}
      }
      static if(T.STORESIZE > STORESIZE) {
	for(size_t i=0; i != STORESIZE; ++i) {
	  result._aval[i] = cast(T.store_t) this._aval[i];
	  static if(_L) {
	    static if(L) result._bval[i] =
			   cast(T.store_t) this._bval[i];
	    else          result._bval[i] = 0;
	  }
	  else {
	    // X and Z values reduce to 0
	    static if(L) result._aval[i] &=
			   ~(cast(T.store_t) this._bval[i]);
	  }
	}
	for(size_t i=STORESIZE; i != T.STORESIZE; ++i) {
	  // if RHS is signed, extend its sign
	  if(this.aValSigned) result._aval[i] = -1;
	  else                 result._aval[i] = 0;
	  static if(_L) {
	    static if(L) {
	      if(this.bValSigned) result._bval[i] = -1;
	      else                 result._bval[i] = 0;
	    }
	    else {
	      result._bval[i] = 0;
	    }
	  }
	  else {
	    static if(L) // X/Z reduce to 0
	      if(this.bValSigned) result._aval[i] = 0;
	  }
	}
	static if(!_S) {	// If result is not signed
	  result._aval[$-1] &= T.UMASK;
	  static if(_L) {
	    result._bval[$-1] &= T.UMASK;
	  }
	}
      }
      return result;
    }

  static SIZE_T length() {
    return SIZE;
  }
}

enum ubvec!1 BIT_0   = ubvec!1(0);
enum ubvec!1 BIT_1   = ubvec!1(1);
alias BIT_0 _0;
alias BIT_1 _1;

enum ulvec!1 LOGIC_0 = ulvec!1(0);
enum ulvec!1 LOGIC_1 = ulvec!1(1);
enum ulvec!1 LOGIC_X = cast(ulvec!1)BIN!"X";
enum ulvec!1 LOGIC_Z = cast(ulvec!1)BIN!"Z";

alias LOGIC_X _X;
alias LOGIC_Z _Z;

alias LOGIC_X _x;
alias LOGIC_Z _z;

// BIN HEX and OCT could be simplyfied but for
// http://d.puremagic.com/issues/show_bug.cgi?id=9143
@property public auto BIN(string VAL)() {
  enum bool L = isStr4State(VAL);
  alias vec!(true, L, stringBitSize(VAL, 2)) vector_t;
  vector_t result = vector_t(vec!(true, L, VAL, 2)(0));
  return result;
}

@property public auto OCT(string VAL)() {
  enum bool L = isStr4State(VAL);
  alias vec!(true, L, stringBitSize(VAL, 8)) vector_t;
  vector_t result = vector_t(vec!(true, L, VAL, 8)(0));
  return result;
}

@property public auto HEX(string VAL)() {
  enum bool L = isStr4State(VAL);
  alias vec!(true, L, stringBitSize(VAL, 16)) vector_t;
  vector_t result = vector_t(vec!(true, L, VAL, 16)(0));
  return result;
}

alias vec!(false, true, 1LU) logic;

alias vec!(false, false, 1LU) bit;

template vec(T) if(is(T == bool)) {
  alias vec!(false, false, 1) vec;
 }

template vec(T) if(isIntegral!T) {
  alias vec!(isSigned!T, false, T.sizeof*8) vec;
 }

template BitVec(N...) if(CheckVecParams!N) {
  alias vec!(true, false, N) BitVec;
 }

template UBitVec(N...) if(CheckVecParams!N) {
  alias vec!(false, false, N) UBitVec;
 }

template LogicVec(N...) if(CheckVecParams!N) {
  alias vec!(true, true, N) LogicVec;
 }

template ULogicVec(N...) if(CheckVecParams!N) {
  alias vec!(false, true, N) ULogicVec;
 }


// A tightly packed fixed width vector of bits
struct vec(bool S, bool L, N...) if(CheckVecParams!N)
  {
    import esdl.base.time;
    enum SIZE_T SIZE = VecSize!(1,N);
    private alias VecParams!(SIZE,S).StoreT store_t;

    enum SIZE_T   STORESIZE  = VecParams!(SIZE,S).STORESIZE;
    enum SIZE_T   WORDSIZE   = VecParams!(SIZE,S).WORDSIZE;
    enum store_t UMASK      = VecParams!(SIZE,S).UMASK;
    enum store_t SMASK      = VecParams!(SIZE,S).SMASK;
    enum bool    IS4STATE   = L;
    enum bool    ISSIGNED   = S;

    alias SIZE opDollar;

    enum size_t ELEMSIZE = N[$-1];
    static if(N.length > 1) {
      enum bool MULTIDIM = true;
      alias vec!(S, L, N[0..$-1]) ELEMTYPE;
    }
    else {
      enum bool MULTIDIM = false;
      alias vec!(S, L, 1)         ELEMTYPE;
    }

    public static auto min() {
      alias vec!(S, L, N) _type;
      _type retval;
      static if(S) {
	retval = cast(_type) 1;
	return retval << (_type.SIZE - 1);
      }
      else {
	return retval;
      }
    }

    public static auto max() {
      alias vec!(S, L, N) _type;
      _type retval;
      static if(S) {
	retval = 1;
	retval <<= (_type.SIZE - 1);
	return ~retval;
      }
      else {
	retval = 0;
	return ~retval;
      }
    }

    public static auto ones(size_t msb, size_t lsb=0) {
      import std.algorithm;
      assert(msb <= SIZE && lsb <= SIZE);
      alias vec!(S, L, N) _type;
      _type a =(cast(_type) 1) << max(msb, lsb);
      _type b =(cast(_type) 1) << min(lsb, msb);
      a -= b;
      return a;
    }

    public static auto zeroes(size_t msb, size_t lsb=0) {
      return ~ones(msb, lsb);
    }

    static if(SIZE <= 64) {	// 32-bit size_t
      alias VecParams!(SIZE,S).ValueT value_t;
      // enum min        = VecParams!(SIZE,S).MAX;
      // enum max        = VecParams!(SIZE,S).MIN;
    }

    // Value bits
    private store_t[STORESIZE] _aval;
    // Control bits
    static if(L) {
      private store_t[STORESIZE] _bval;
    }
    else {
      private enum store_t[STORESIZE] _bval = 0;
    }


    public auto aVal() {
      // http://d.puremagic.com/issues/show_bug.cgi?id=9143
      // vec!(S, false, N) retVal;
      vec!(S, false, N) retVal;
      retVal._aval[] = this._aval[];
      return retVal;
    }

    public auto bVal() {
      vec!(S, false, N) retVal;
      retVal._aval[] = this._bval[];
      return retVal;
    }

    public void setAval(T)(T t)
    if(is(T == bool) ||
       (isIntegral!T && T.sizeof*8 <= SIZE) ||
       (isBitVector!T && T.SIZE <= SIZE && (! T.IS4STATE))) {
      static if(is(T == bool)) enum bool _S = false;
      else static if(isBitVector!T) enum bool _S = T.ISSIGNED;
	else enum bool _S = isSigned!T;
      vec!(_S, false, N) v = t;
      // http://d.puremagic.com/issues/show_bug.cgi?id=9143
      // vec!(S, false, N) retVal;
      this._aval = v._aval;
    }

    static if(L) {
      public void setBval(T)(T t)
	if(is(T == bool) ||
	   (isIntegral!T && T.sizeof*8 <= SIZE) ||
	   (isBitVector!T && T.SIZE <= SIZE && (! T.IS4STATE))) {
	  static if(is(T == bool)) enum bool _S = false;
	  else static if(isBitVector!T) enum bool _S = T.ISSIGNED;
	    else enum bool _S = isSigned!T;
	  vec!(_S, false, N) v = t;
	  // http://d.puremagic.com/issues/show_bug.cgi?id=9143
	  // vec!(S, false, N) retVal;
	  this._bval = v._aval;
	}
    }

    @property public bool isX() {
      static if(L) {
	foreach(v; _bval) {
	  if(v != 0) return true;
	}
	return false;
      }
      else return false;
    }

    public this(T)(T other)
      if((isBitVector!T ||
	  is(T unused == vec!(S_, L_, _VAL, _RADIX),
	     bool S_, bool L_, string _VAL, SIZE_T _RADIX)) &&
	 (NO_CHECK_SIZE || SIZE >= T.SIZE)) {
	this._from(other);
      }

    public this()(bool other) {
      this._from(other);
    }

    public  this(T)(T other)
      if(isIntegral!T && (NO_CHECK_SIZE || SIZE >= T.sizeof*8)) {
	this._from(other);
      }


    // public this(T)(T other)
    //   if(isFloatingPoint!T &&
    // 	 SIZE >= T.sizeof*8) {
    // 	this._from(other);
    //   }

    // public this(T)(T other)
    //   if(is(T == SimTime)) {
    // 	this._from(other);
    //   }

    // public this(T)(T other)
    //   if(is(T == SimTime)) {
    // 	this._from(other);
    //   }

    public this(T)(T ba) if(is(T == barray)) {
      this = ba;
    }

    public this(T)(T [] bits) if(is(T == bool)) {
      this = bits;
    }

    // declare this aliases -- only if SIZE <= 64
    static if(SIZE <= 64 && !L) {
      static if(SIZE <= size_t.sizeof*8) { // 32-bit size_t
	static assert(STORESIZE == 1);
      }
      else {
	static assert(STORESIZE == 2);
      }
      static if(SIZE == 1) {
	@property public bool getValue() {
	  bool value =(this._aval[0] & 1);
	  static if(L) {
	    value &= cast(bool) ~(this._bval[0] & 1); // makes X/Z as 0
	  }
	  return value;
	}
      }
      else {
	@property public value_t getValue() {
	  value_t value = this._aval[0];
	  static if(L) {
	    value &= ~(this._bval[0]); // makes X/Z as 0
	  }
	  static if(SIZE > size_t.sizeof*8) { // 32-bit machines
	    value_t value32 = this._aval[1];
	    static if(L) {
	      value32 &= ~(this._bval[1]);
	    }
	    value |=(value32 << 32);
	  }
	  // asserts to make sure that sign extension is proper
	  // static if(S && SIZE < value_t.sizeof*8) {
	  //   if((value >> (SIZE-1)) & 1) { // negative
	  //     // make sure that the returned value is sign extended
	  //     import std.string;
	  //     assert(isSigned!value_t);
	  //     assert((value >> SIZE) == -1,
	  // 	     format("value is %d, %b, %s", value, value, typeid(value_t))); // ">>" is sign-extending shift
	  //   }
	  // }
	  // else
	  //   static if(SIZE < value_t.sizeof*8) {
	  //     assert((value >> SIZE) == 0);
	  //   }
	  return value;
	}
      }
      alias getValue this;
    }

    public void randomize() {
      import std.random;
      for(size_t i=0; i!=STORESIZE; ++i) {
	this._aval[i] = uniform!store_t();
	static if(L) this._bval[i] = 0;
      }
      if(aValSigned) this._aval[$-1] |= SMASK;
      else           this._aval[$-1] &= UMASK;
      static if(L) {
	if(bValSigned) this._bval[$-1] |= SMASK;
	else           this._bval[$-1] &= UMASK;
      }
    }

    public bool aValSigned() {
      static if(S) {
	if((this._aval[$-1] &
	    (cast(store_t) 1) <<((SIZE-1) % WORDSIZE)))
	  return true;
	else
	  return false;
      }
      else
	return false;
    }

    static if(L) {
      public bool bValSigned() {
	static if(S) {
	  if((this._bval[$-1] &
	      (cast(store_t) 1) <<((SIZE-1) % WORDSIZE)))
	    return true;
	  else
	    return false;
	}
	else
	  return false;
      }
    }


    public void storeString(T)(T other)
      if(is(T unused == string)) {
	foreach(ref a; _aval) a = 0;
	static if(L) foreach(ref b; _bval) b = 0;
	foreach(size_t i, char c; other) {
	  auto j = i / store_t.sizeof;
	  auto k = i % store_t.sizeof;
	  if(j < STORESIZE) {
	    import std.traits;
	    _aval[j] |=(cast(Unsigned!store_t) c) << k*8;
	  }
	}
	if(aValSigned) this._aval[$-1] |= SMASK;
	else           this._aval[$-1] &= UMASK;
	static if(L) {
	  if(bValSigned) this._bval[$-1] |= SMASK;
	  else           this._bval[$-1] &= UMASK;
	}
      }



    public void opAssign(T)(T other)
      if(isIntegral!T && (NO_CHECK_SIZE || SIZE >= T.sizeof*8)) {
	this._from(other);
      }

    private void _from(T)(T other)
      if(isIntegral!T) {
	static if(isSigned!T) long rhs = other;
	else                  ulong rhs = other;
	_aval[0] = cast(store_t) rhs;
	static if(L) _bval[0] = 0;
	static if(STORESIZE > 1) {
	  for(size_t i=1; i != STORESIZE; ++i) {
	    rhs >>= store_t.sizeof*4; // '>>' is sign-extending shift
	    rhs >>= store_t.sizeof*4; // '>>' is sign-extending shift
	    _aval[i] = cast(store_t) rhs;
	    static if(L) _bval[i] = 0;
	  }
	}
	// In case the vector is not signed, mask the extended sign bits if eny
	static if(!S && STORESIZE*store_t.sizeof*8 > SIZE) {
	  _aval[$-1] &= UMASK;
	  static if(L) _bval[$-1] &= UMASK;
	}
      }

    // public void opAssign(T)(T other)
    //   if(is(T == bool))
    //	{
    //	  this._from(other);
    //	}

    public void opAssign()(bool other) {
      this._from(other);
    }

    private void _from(T)(T other)
      if(is(T == bool)) {
	_aval[0] = cast(store_t) other;
	static if(L) _bval[0] = 0;
	static if(STORESIZE > 1) {
	  for(size_t i=1; i != STORESIZE; ++i) {
	    _aval[i] = 0;
	    static if(L) _bval[i] = 0;
	  }
	}
      }

    // public void opAssign(T)(T other)
    //   if(isFloatingPoint!T &&
    // 	 SIZE >= T.sizeof*8) {
    // 	this._from(other);
    //   }

    // public void opAssign(T)(T other)
    //   if(is(T == SimTime) &&
    // 	 SIZE >= 72) {
    // 	this._from(other);
    //   }

    // public void opAssign(T)(T other)
    //   if(is(T == SimTime) &&
    // 	 SIZE >= 64) {
    // 	this._from(other);
    //   }

    // private void _from(T)(T other)
    //   if(is(T == SimTime) &&
    // 	 SIZE >= 72) {
    // 	for(size_t i=0; i!=STORESIZE; ++i) {
    // 	  _aval[i] = 0;
    // 	  static if(L) _bval[i] = 0;
    // 	}

    // 	this._aval[0] = other._value;
    // 	this._aval[1] = other._unit;
    //   }

    // private void _from(T)(T other)
    //   if(is(T == SimTime) &&
    // 	 SIZE >= 64) {
    // 	for(size_t i=0; i!=STORESIZE; ++i) {
    // 	  _aval[i] = 0;
    // 	  static if(L) _bval[i] = 0;
    // 	}

    // 	this._aval[0] = other.getVal();
    //   }

    public void opAssign(T)(T other)
      if((isBitVector!T ||
	  is(T unused == vec!(_S, _L, _VAL, _RADIX), bool _S, bool _L, string _VAL, SIZE_T _RADIX))
	 && (NO_CHECK_SIZE || SIZE >= T.SIZE)
	 &&(IS4STATE || !T.IS4STATE)) {
	this._from(other);
      }

    private void _from(T)(T other)
      if(isBitVector!T ||
	 is(T unused == vec!(_S, _L, _VAL, _RADIX), bool _S, bool _L, string _VAL, SIZE_T _RADIX)) {
	static assert(NO_CHECK_SIZE || SIZE >= T.SIZE);
	static assert(IS4STATE || !T.IS4STATE,
		      "Can not implicitly convert LogicVec to BitVec");
	enum bool _L = T.IS4STATE;
	for(size_t i=0; i != T.STORESIZE; ++i) {
	  this._aval[i] = cast(store_t) other._aval[i];
	  static if(L) {
	    static if(_L) this._bval[i] =
			    cast(store_t) other._bval[i];
	    else           this._bval[i] = 0;
	  }
	}
	for(size_t i=T.STORESIZE; i != STORESIZE; ++i) {
	  // if RHS is signed, extend its sign
	  if(other.aValSigned) this._aval[i] = cast(store_t) -1;
	  else                  this._aval[i] = 0;
	  static if(L) {
	    static if(_L) {
	      if(other.bValSigned) this._bval[i] = cast(store_t) -1;
	      else                  this._bval[i] = 0;
	    }
	    else {
	      this._bval[i] = 0;
	    }
	  }
	}
	if(aValSigned) this._aval[$-1] |= SMASK;
	else           this._aval[$-1] &= UMASK;
	static if(L) {
	  if(bValSigned) this._bval[$-1] |= SMASK;
	  else           this._bval[$-1] &= UMASK;
	}
      }

    version(COSIM_VERILOG) {
      import esdl.intf.vpi;

      public void opAssign()(p_vpi_vecval other) {
	this._from(other);
      }

      private void _from() (p_vpi_vecval other) {
	static if(WORDSIZE is 64) {
	  for (size_t i=0; i!=(SIZE+31)/32; ++i) {
	    ulong word = other[i].aval;
	    if(i%2 == 0) this._aval[i/2] = word;
	    else this._aval[i/2] |= word << 32;
	    static if(IS4STATE) {
	      ulong cword = other[i].bval;
	      if(i%2 == 0) this._bval[i/2] = cword;
	      else this._bval[i/2] |= cword << 32;
	    }
	  }
	}
	else static if(WORDSIZE is 32) {
	    for (size_t i=0; i!=(SIZE+31)/32; ++i) {
	      this._aval[i] = other[i].aval;
	      static if(IS4STATE) {
		this._bval[i] = other[i].bval;
	      }
	    }
	  }
	  else {
	    this._aval[0] = cast(StoreT) other[0].aval;
	    static if(IS4STATE) {
	      this._bval[0] = cast(StoreT) other[0].bval;
	    }
	  }
      }

      // It is the responsibility of the caller to make sure that
      // there is enough space available at other to write down the
      // required bits
      public void toVpiVecValue(s_vpi_vecval[] other) {
	this._to(other);
      }
      
      private void _to() (s_vpi_vecval[] other) {
	static if(WORDSIZE is 64) {
	  for (size_t i=0; i!=(SIZE+31)/32; ++i) {
	    if(i%2 == 0) other[i].aval = cast(uint) this._aval[i/2];
	    else other[i].aval = cast(uint) (this._aval[i/2] >>> 32);
	    static if(IS4STATE) {
	      if(i%2 == 0) other[i].bval = cast(uint) this._bval[i/2];
	      else other[i].bval = cast(uint) (this._bval[i/2] >>> 32);
	    }
	  }
	}
	else static if(WORDSIZE is 32) {
	    for (size_t i=0; i!=(SIZE+31)/32; ++i) {
	      other[i].aval = this._aval[i];
	      static if(IS4STATE) {
		other[i].bval = this._bval[i];
	      }
	    }
	  }
	  else {
	    other[0].aval = this._aval[0];
	    static if(IS4STATE) {
	      other[0].bval = this._bval[0];
	    }
	  }
      }
    }
    

    public void opAssign(T)(T ba) if(is(T == barray)) {
      auto numBits = ba.length;
      if(numBits > SIZE) {
	writeln("Warning: truncating barray to fit into BitVec");
      }
      auto bavoid = cast(void[]) ba;
      // number of bytes to be copied from barray
      // auto numBytes =(ba.length + ubyte.sizeof - 1)/ubyte.sizeof;
      auto baptr = cast(ubyte*) bavoid.ptr;
      auto babytes = baptr[0..(bavoid.length * void.sizeof)];
      // auto babytes = baptr[0..(numBytes * void.sizeof)];

      auto bvptr = cast(ubyte*) _aval.ptr;
      auto bvbytes = bvptr[0..STORESIZE*store_t.sizeof/ubyte.sizeof];
      for(size_t i = 0; i != bvbytes.length; ++i) {
	if(i < babytes.length) bvbytes[i] = babytes[i];
	else bvbytes[i] = 0;
      }
    }

    public void opAssign(T)(T [] bits) if(is(T == bool)) {
      static if(bits.length > SIZE) {
	writeln("Warning: truncating array of bool to fit into BitVec");
      }
      for(size_t i=0; i != STORESIZE; ++i) {
	if(i < bits.length) {
	  this[i] = bits[i];
	}
	else {
	  this[i] = false;
	}
      }
    }

    T to(T, SIZE_T RADIX = 2)() if((is(T == string) ||
				    is(T == char[]))
				   &&(RADIX == 2 ||
				      RADIX == 8 ||
				      RADIX == 16)) {
      static if(RADIX == 8) {
	return logicToOctal(this.to!(T, 2));
      }
      else {
	return toCharString!(T, RADIX);
      }
    }

    string toString() {
      return this.to!(string, 2);
    }

    void toString(scope void delegate(const(char)[]) sink, ref FormatSpec!char f) {
      char[] buff;
      switch(f.spec) {
      case 'd'     : buff = this.toDecimalString(); break;
      case 's'     :		// should print as hex when %s
      case 'h'     :		// should print as hex for %h too
      case 'x'     : buff = "0x" ~ toLower(this.to!(char[], 16)); break;
      case 'H'     :		// should print as HEX for %H
      case 'X'     : buff = "0x" ~ this.to!(char[], 16); break;
      case 'o'     : buff = this.to!(char[], 8); break;
      case 'b'     : buff = "0b" ~ this.to!(char[], 2); break;
      default      :
	throw new FormatException("Format specifier not understood: %" ~ f.spec);
      }

      assert(buff.length > 0);

      sink(buff);
    }

    private T toCharString(T, SIZE_T RADIX)() {
      char[] str;
      if(STORESIZE > 1) {
	for(size_t i = 0; i != STORESIZE-1; ++i) {
	  import std.string;
	  static if(RADIX == 2)  string fmtstr = "%b";
	  static if(RADIX == 8)  string fmtstr = "%o";
	  static if(RADIX == 16) string fmtstr = "%X";
	  char[] wstr;
	  string astr =
	    rightJustify(format(fmtstr, _aval[i]),
			 cast(int)((_log2(RADIX) - 1) +
				   8*store_t.sizeof)/_log2(RADIX), '0');
	  static if(L) {
	    string zstr =
	      rightJustify(format(fmtstr, _bval[i]),
			   cast(int)((_log2(RADIX) - 1) +
				     8*store_t.sizeof)/_log2(RADIX), '0');
	    string xstr =
	      rightJustify(format(fmtstr,(cast(store_t)
					  (_aval[i] & _bval[i]))),
			   cast(int)((_log2(RADIX) - 1) +
				     8*store_t.sizeof)/_log2(RADIX), '0');
	  }
	  foreach(j, c; astr) {
	    char s = c;
	    static if(L) {
	      if(zstr[j] != '0') {
		s = 'Z';
		if(xstr[j] != '0') s = 'X';
	      }
	    }
	    wstr ~= s;
	  }
	  str = wstr ~ str;
	}
      }

      char[] wstr;
      auto foo = cast(store_t)(_aval[$-1] & UMASK);
      static if(RADIX == 16) string fmtstr = "%X";
      static if(RADIX == 8)  string fmtstr = "%o";
      static if(RADIX == 2)  string fmtstr = "%b";

      import std.string;
      string astr =
	rightJustify(format(fmtstr, cast(store_t)(_aval[$-1] & UMASK)),
		     cast(int)((_log2(RADIX) - 1) +
			       ((SIZE-1)%(8*store_t.sizeof) + 1))
		     /_log2(RADIX), '0');
      static if(L) {
	string zstr =
	  rightJustify(format(fmtstr, cast(store_t)(_bval[$-1] & UMASK)),
		       cast(int)((_log2(RADIX) - 1) +
				 ((SIZE-1)%(8*store_t.sizeof) + 1))
		       /_log2(RADIX), '0');
	string xstr =
	  rightJustify(format(fmtstr, cast(store_t)(_aval[$-1] &
						    _bval[$-1] & UMASK)),
		       cast(int)((_log2(RADIX) - 1) +
				 ((SIZE-1)%(8*store_t.sizeof) + 1))
		       /_log2(RADIX), '0');
      }

      foreach(i, c; astr) {
	char s = c;
	static if(L) {
	  if(zstr[i] != '0') {
	    s = 'Z';
	    if(xstr[i] != '0') s = 'X';
	  }
	}
	wstr ~= s;
      }
      str = wstr ~ str;
      return cast(T) str;
    }

    char [] toDecimalString() const {
      static if(STORESIZE == 1) {
	import std.conv;
	auto val = this._aval[0];
	string str = to!string(val);
	char[] buff;
	foreach(c; str) buff ~= c;
	return buff;
      }
      else {
	uint[] data =(cast(uint[]) this._aval).dup;
	auto predictlength = 20+20*(data.length/2); // just over 19
	char [] buff = new char[predictlength];
	size_t sofar = biguintToDecimal(buff, data.dup);
	return buff[sofar..$];
      }
    }

    auto opIndex(size_t i) const
    in {
      assert(i < SIZE);
    }
    body {
      vec!(false, L, 1) retval;
      static if(STORESIZE == 1) {
	retval._aval[0] = cast(ubyte)((this._aval[0] >>> i) & 1LU);
	static if(L) {
	  retval._bval[0] = cast(ubyte)((this._bval[0] >>> i) & 1LU);
	}
      }
      else {
	retval._aval[0] = cast(ubyte) bt(cast(const(ulong*)) this._aval.ptr, i);
	static if(L) {
	  retval._bval[0] = cast(ubyte) bt(cast(const(ulong*)) this._bval.ptr, i);
	}
      }
      return retval;
    }

    unittest {
      void Fun(const BitVec!3 arr) {
	auto x = arr[0];
	assert(x == 1);
      }
      BitVec!3 a;
      a[0] = 1;
      Fun(a);
    }

    bool opIndexAssign()(bool b, size_t i)
      in {
	assert(i < SIZE);
      }
    body {
      static if(STORESIZE == 1) {
	if(b) this._aval[0] |=(1L << i);
	else   this._aval[0] &= ~(1L << i);
	static if(L) {
	  this._bval[0] &= ~(1L << i);
	}
      }
      else {
	if(b) bts((cast(ulong*) _aval.ptr), i);
	else   btr((cast(ulong*) _aval.ptr), i);
	static if(L) {
	  btr((cast(ulong*) _bval.ptr), i);
	}
      }
      return b;
    }

    auto opIndexAssign(T)(T other, size_t i)
    if(isBitVector!T && T.SIZE == 1)
      in {
	assert(i < SIZE);
      }
    body {
      static if(STORESIZE == 1) {
	static if(other.IS4STATE) {
	  static if(L) {
	    if(other.aVal) this._aval[0] |=(1L << i);
	    else this._aval[0] &= ~(1L << i);
	    if(other.bVal) this._bval[0] |=(1L << i);
	    else this._bval[0] &= ~(1L << i);
	  }
	  else {
	    if(other.aVal && !(other.bVal)) this._aval[0] |=(1L << i);
	    else this._aval[0] &= ~(1L << i);
	  }
	}
	else {
	  if(other.aVal)
	    this._aval[0] |=(1L << i);
	  else this._aval[0] &= ~(1L << i);
	  static if(L) {
	    this._bval[0] &= ~(1L << i);
	  }
	}
      }
      else {
	static if(other.IS4STATE) {
	  static if(L) {
	    if(other.aVal) bts((cast(ulong*) _aval.ptr), i);
	    else btr((cast(ulong*) _aval.ptr), i);
	    if(other.bVal) bts((cast(ulong*) _bval.ptr), i);
	    else btr((cast(ulong*) _bval.ptr), i);
	  }
	  else {
	    if(other.aVal && !(other.bVal)) bts((cast(ulong*) _aval.ptr), i);
	    else btr((cast(ulong*) _aval.ptr), i);
	  }
	}
	else {
	  if(other.aVal) bts((cast(ulong*) _aval.ptr), i);
	  else btr((cast(ulong*) _aval.ptr), i);
	  static if(L) {
	    btr((cast(ulong*) _bval.ptr), i);
	  }
	}
      }
      return other;
    }

    // And/Or/Xor
    public auto opBinary(string op, T)(T other)
    if((isBitVector!T || isIntegral!T) &&
       (op == "&" || op == "|" || op == "^")) {
      vec!(typeof(this), T, op) result = this;
      result.opOpAssign!op(other);
      return result;
    }

    // And/Or/Xor Assign
    public void opOpAssign(string op, T)(T other)
      if(isIntegral!T &&
	 (op == "&" || op == "|" || op == "^")) {
	vec!T rhs = other;
	this.opOpAssign!op(rhs);
      }

    public void opOpAssign(string op, T)(T other)
      if(isBitVector!T &&
	 (op == "&" || op == "|" || op == "^")) {
	enum bool _S = T.ISSIGNED;
	enum bool _L = T.IS4STATE;
	auto rhs = cast(vec!(_S, _L, SIZE)) other;
	for(size_t i=0; i!=STORESIZE; ++i) {
	  static if(L) {
	    static if(_L) {
	      static if(op == "|") {
		auto a =
		  this._aval[i] | rhs._aval[i] |
		  rhs._bval[i] | this._bval[i];
		auto b =
		  (~rhs._aval[i] &  this._bval[i]) |
		  ( rhs._bval[i] & ~this._aval[i]) |
		  ( rhs._bval[i] &  this._bval[i]);
		this._aval[i] = cast(store_t) a;
		this._bval[i] = cast(store_t) b;
	      }
	      static if(op == "&") {
		this._aval[i] =
		  ( rhs._aval[i] | rhs._bval[i]) &
		  (  this._aval[i] |  this._bval[i]);
		this._bval[i] =
		  this._aval[i] &
		  (this._bval[i] | rhs._bval[i]);
	      }
	      static if(op == "^") {
		this._bval[i] =
		  this._bval[i] | rhs._bval[i];
		this._aval[i] =
		  (this._aval[i] ^ rhs._aval[i]) |
		  this._bval[i];
	      }
	    }
	    else {
	      static if(op == "|") {
		auto a =
		  this._aval[i] | rhs._aval[i] | this._bval[i];
		auto b =
		  (~rhs._aval[i] &  this._bval[i]);
		this._aval[i] = cast(store_t) a;
		this._bval[i] = cast(store_t) b;
	      }
	      static if(op == "&") {
		this._aval[i] =
		  rhs._aval[i] &(this._aval[i] |  this._bval[i]);
		this._bval[i] =
		  this._aval[i] & this._bval[i];
	      }
	      static if(op == "^") {
		this._aval[i] =
		  (this._aval[i] ^ rhs._aval[i]) |
		  this._bval[i];
	      }
	    }
	  }
	  else {			// L is false
	    static if(_L) {
	      static if(op == "|") {
		this._aval[i] = cast(store_t)
		  (this._aval[i] |(rhs._aval[i] & ~rhs._bval[i]));
	      }
	      static if(op == "&") {
		this._aval[i] = cast(store_t)
		  (rhs._aval[i] & this._aval[i] & ~rhs._bval[i]);
	      }
	      static if(op == "^") {
		this._aval[i] = cast(store_t)
		  ((this._aval[i] ^ rhs._aval[i]) & ~rhs._bval[i]);
	      }
	    }
	    else {
	      static if(op == "|") {
		this._aval[i] = cast(store_t)
		  (this._aval[i] | rhs._aval[i]);
	      }
	      static if(op == "&") {
		this._aval[i] = cast(store_t)
		  (rhs._aval[i] & this._aval[i]);
	      }
	      static if(op == "^") {
		this._aval[i] = cast(store_t)
		  (this._aval[i] ^ rhs._aval[i]);
	      }
	    }
	  }
	  if(this.aValSigned) this._aval[$-1] |= SMASK;
	  else                this._aval[$-1] &= UMASK;
	  static if(L) {
	    if(this.bValSigned) this._bval[$-1] |= SMASK;
	    else                this._bval[$-1] &= UMASK;
	  }
	}
      }


    // mask out(make 0) the bits that are 1 in the argument
    public void maskOut(T)(T other)
      if(isBitVector!T
	 && !T.IS4STATE && !T.ISSIGNED) {
	auto rhs = cast(UBitVec!SIZE) other;
	for(size_t i=0; i!=STORESIZE; ++i) {
	  this._aval[i] &= ~rhs._aval[i];
	  static if(L) {
	    this._bval[i] &= ~rhs._aval[i];
	  }
	  if(this.aValSigned) this._aval[$-1] |= SMASK;
	  else                this._aval[$-1] &= UMASK;
	  static if(L) {
	    if(this.bValSigned) this._bval[$-1] |= SMASK;
	    else                this._bval[$-1] &= UMASK;
	  }
	}
      }

    // Copy all the bits that are not 0 in the argument -- assume that the
    // mask out has already taken place
    public void maskIn(T)(T other)
      if(isBitVector!T && (!T.IS4STATE ||(T.IS4STATE && IS4STATE))) {
	auto rhs = cast(vec!(false, T.IS4STATE, SIZE)) other;
	for(size_t i=0; i!=STORESIZE; ++i) {
	  this._aval[i] |= rhs._aval[i];
	  static if(T.IS4STATE && IS4STATE) {
	    this._bval[i] |= rhs._bval[i];
	  }
	  if(this.aValSigned) this._aval[$-1] |= SMASK;
	  else                this._aval[$-1] &= UMASK;
	  static if(L) {
	    if(this.bValSigned) this._bval[$-1] |= SMASK;
	    else                this._bval[$-1] &= UMASK;
	  }
	}
      }

    // void opSliceAssign(T)(T ba, size_t i, size_t j)
    //   if(is(T == barray) ||(isArray!T && is(ElementType!T == bool)))
    //     in {
    //	assert(i < SIZE && i >= 0 && j < SIZE && j >= 0);
    //	assert(i != j);
    //     }
    // body {
    //   if(i > j) {
    //     if(i - j < ba.length) {
    //	writeln("Warning: truncating barray/bool-array to fit into BitVec Slice");
    //     }
    //     for(size_t k = 0; k != i - j; ++k) {
    //	if(k < ba.length) this[j+k] = ba[k];
    //	else this[j+k] = false;
    //     }
    //   }
    //   else {			// j > i
    //     if(j - i < ba.length) {
    //	writeln("Warning: truncating  barray/bool-array to fit into BitVec Slice");
    //     }
    //     for(size_t k = 0; k != j - i; ++k) {
    //	if(k < ba.length) this[(j-1)-k] = ba[k];
    //	else this[(j-1)-k] = false;
    //     }
    //   }
    // }

    // void opSliceAssign(string VAL)(BitVec!VAL other, size_t i, size_t j)
    //   in {
    //     assert(i < SIZE && i >= 0 && j < SIZE && j >= 0);
    //     assert(i != j);
    //   }
    // body {
    //   alias BitVec!(VAL) RT;
    //   if(i > j) {
    //     if(i - j < RT.SIZE) {
    //	writeln("Warning: truncating barray/bool-array to fit into BitVec Slice");
    //     }
    //     for(size_t k = 0; k != i - j; ++k) {
    //	if(k < RT.SIZE) this[j+k] = other[k];
    //	else this[j+k] = false;
    //     }
    //   }
    //   else {			// j > i
    //     if(j - i < RT.SIZE) {
    //	writeln("Warning: truncating  barray/bool-array to fit into BitVec Slice");
    //     }
    //     for(size_t k = 0; k != j - i; ++k) {
    //	if(k < RT.SIZE) this[(j-1)-k] = other[k];
    //	else this[(j-1)-k] = false;
    //     }
    //   }
    // }

    // void opSliceAssign(T)(T other, size_t i, size_t j)
    //   if(isBitVector!T ||
    //	isIntegral!T)
    //     {
    //	import std.algorithm;
    //	import std.exception;
    //	static if(isIntegral!T)
    //	  {
    //	    alias UBitVec!(T.sizeof*8) _type;
    //	    _type rhs = other;
    //	  }
    //	else
    //	  {
    //	    alias T _type;
    //	    alias other rhs;
    //	  }

    //	enforce(i <= SIZE && j <= SIZE,
    //		 "Slice operands may not be negative");
    //	enforce(max(i,j) - min(i,j) == _type.SIZE,
    //		 "Slice size does not match with the RHS");
    //	if(i > j)		// bigendian
    //	  {
    //	    this.put(j, rhs);
    //	  }
    //	else
    //	  {
    //	    this.put(i, rhs.reverse);
    //	  }
    //     }

    // T opCast(T)() if(is(T == barray)) {
    //   barray ba;

    //   // Since dup method creates a dynamic array, hopefully the
    //   // memory is allocate on the heap
    //   // store_t[STORESIZE] bav = _aval.dup;

    //   // for(size_t i=0; i != STORESIZE; ++i) {
    //   //	writeln("dup: ", bav[i], ":", _aval[i]);
    //   // }

    //   ba.init(_aval.dup, SIZE);
    //   return ba;
    // }

    public T opCast(T)() if(isIntegral!T || is(T == bool)) {
      static if(L) {
	T value = cast(T)(this._aval[0] & ~this._bval[0]);
      }
      else {
	T value = cast(T) this._aval[0];
      }
      return value;
    }

    // public T opCast(T)()
    //   if(is(T == SimTime) &&
    // 	 SIZE >= 72) {
    // 	SimTime retval;
    // 	retval._value = _aval[0];
    // 	retval._unit  = cast(TimeUnit) _aval[1];
    // 	return retval;
    //   }

    // public T opCast(T)()
    //   if(is(T == SimTime) &&
    // 	 SIZE >= 64) {
    // 	SimTime retval = _aval[0];
    // 	return retval;
    //   }

    // public T opCast(T)()
    //   if(isFloatingPoint!T &&
    // 	 T.sizeof*8 <= SIZE) {
    // 	static if(is(T unused == real)) {
    // 	  enum WSIZE =(T.sizeof+7)/8;
    // 	  alias ulong ftype;
    // 	}
    // 	static if(is(T unused == double)) {
    // 	  enum WSIZE = 1;
    // 	  alias ulong ftype;
    // 	}
    // 	static if(is(T unused == float)) {
    // 	  enum WSIZE = 1;
    // 	  alias uint ftype;
    // 	}

    // 	union utype {
    // 	  ftype[WSIZE] b;
    // 	  T f;
    // 	}

    // 	utype u;

    // 	for(size_t i=0; i!=WSIZE; ++i) {
    // 	  u.b[i] = cast(ftype) _aval[i];
    // 	}

    // 	return	u.f;
    //   }

    public T opCast(T)()
      if(isBitVector!T) {
	enum bool _L = T.IS4STATE;
	enum bool _S = T.ISSIGNED;
	T result;
	static if(T.STORESIZE <= STORESIZE) {
	  for(size_t i=0; i != T.STORESIZE; ++i) {
	    result._aval[i] = cast(T.store_t) this._aval[i];
	    static if(_L) {
	      static if(L) result._bval[i] =
			     cast(T.store_t) this._bval[i];
	      else          result._bval[i] = 0;
	    }
	    else {
	      // X and Z values reduce to 0
	      static if(L) result._aval[i] &=
			     ~(cast(T.store_t) this._bval[i]);
	    }
	  }
	  // sign extension
	  if(result.aValSigned) result._aval[$-1] |= T.SMASK;
	  else                  result._aval[$-1] &= T.UMASK;
	  static if(_L) {
	    if(result.bValSigned) result._bval[$-1] |= T.SMASK;
	    else                  result._bval[$-1] &= T.UMASK;
	  }
	}
	static if(T.STORESIZE > STORESIZE) {
	  for(size_t i=0; i != STORESIZE; ++i) {
	    result._aval[i] = cast(T.store_t) this._aval[i];
	    static if(_L) {
	      static if(L) result._bval[i] =
			     cast(T.store_t) this._bval[i];
	      else          result._bval[i] = 0;
	    }
	    else {
	      // X and Z values reduce to 0
	      static if(L) result._aval[i] &=
			     ~(cast(T.store_t) this._bval[i]);
	    }
	  }
	  for(size_t i=STORESIZE; i != T.STORESIZE; ++i) {
	    // if RHS is signed, extend its sign
	    if(this.aValSigned) result._aval[i] = -1;
	    else                 result._aval[i] = 0;
	    static if(_L) {
	      static if(L) {
		if(this.bValSigned) result._bval[i] = -1;
		else                 result._bval[i] = 0;
	      }
	      else {
		result._bval[i] = 0;
	      }
	    }
	    else {
	      static if(L)		// X/Z reduce to 0
		if(this.bValSigned) result._aval[i] = 0;
	    }
	  }
	  static if(!_S) {		// If result is not signed
	    result._aval[$-1] &= T.UMASK;
	    static if(_L) {
	      result._bval[$-1] &= T.UMASK;
	    }
	  }
	}
	return result;
      }

    public string retrieveString() {
      string retval;
      foreach(w; _aval) {
	for(size_t i = 0; i != store_t.sizeof; ++i) {
	  if(w == 0) goto done;
	  retval ~= cast(char) w;
	  w >>= 8;
	}
      }
    done: return retval;
    }

    SIZE_T length() const {
      return SIZE;
    }

    // T opCast(T)() if(isIntegral!T) {
    //   T res = cast(T) _aval[0];
    //   return res;
    // }


    // string toString()
    //   {
    //     static if(STORESIZE == 1 && isIntegral!(typeof(_aval[0])))
    //	{
    //	  // We need to use the global to!string, and therefor the '.'
    //	  return .to!string(_aval[0], 2);
    //	}
    //     else
    //	{
    //	  static assert(false, Format!("Can not convert BitVec of size %s"
    //				       ~ " to %s", SIZE, "string"));
    //	}
    //   }

    // public barray opSlice(size_t i, size_t j)
    //   in {
    //     assert(i < SIZE && i >= 0 && j < SIZE && j >= 0);
    //     assert(i != j);
    //   }
    // body {
    //   barray ba;
    //   if(i > j) {
    //     ba.length = i - j;
    //     for(size_t k=j; k!=i; ++k) {
    //	ba[k-j] = cast(bool) this[k];
    //     }
    //   } else {			// j > i
    //     ba.length = j - i;
    //     for(size_t k=i; k!=j; ++k) {
    //	ba[(j-1)-k] = cast(bool) this[k];
    //     }
    //   }
    //   return ba;
    // }

    // operator []
    // public barray opSlice() {
    //   return this.to!barray;
    // }

    // place the bits available from the given rhs and place into the
    // specified location
    public void put(size_t COUNT, T)(size_t i, T other)
      if(isBitVector!T ||
	 (isIntegral!T && T.sizeof*8 <= COUNT)) {
	assert(i + COUNT <= SIZE);
	vec!(false, L, COUNT) rhs = other;
	auto mask =(cast(UBitVec!SIZE) UBitVec!(COUNT).max) << i;
	static if(isIntegral!T) {
	  auto value = cast(vec!(false, false, SIZE)) rhs;
	}
	else {
	  auto value = cast(vec!(false, T.IS4STATE, SIZE)) rhs;
	}
	this.maskOut(mask);
	this.maskIn(value << i);
      }

    public auto get(int COUNT)(size_t i) const
    if(COUNT <= SIZE) {
      assert(i + COUNT <= SIZE);
      vec!(S,L,N) retval = cast(typeof(this)) this;
      retval >>= i;
      return cast(vec!(S,L,COUNT)) retval;
    }

    // public BitVec!(I, J) slice(size_t I, size_t J=0)() {
    //   static assert(I < SIZE && I >= 0 && J < SIZE && J >= 0);
    //   static assert(I != J);
    //   BitVec!(I, J) bv;
    //   static if(I > J) {
    //     for(size_t k=J; k!=I; ++k) {
    //	bv[k] = this[k];
    //     }
    //   } else {			// J > I
    //     for(size_t k=I; k!=J; ++k) {
    //	bv[k] = this[k];
    //     }
    //   }
    //   return bv;
    // }

    public auto reverse() {
      typeof(this) retval;
      for(size_t i=0; i != SIZE; ++i) {
	retval[SIZE-i-1] = this[i];
      }
      return retval;
    }

    private void reportX(string file = __FILE__,
			 size_t line = __LINE__, T)(T other)
      if(isBitVector!T) {
	static if(this.IS4STATE || other.IS4STATE) {
	  if(this.isX || other.isX) {
	    throw new LogicError(format("Logic value of one of the " ~
					"operands is X, %s, %s",
					this, other), file, line);
	  }
	}
      }

    private void reportX(string file = __FILE__,
			 size_t line = __LINE__, T)(T other)
      if(isIntegral!T || is(T == bool)) {
	static if(this.IS4STATE) {
	  if(this.isX) {
	    throw new LogicError(format("Logic value of " ~
					"operand is X, %s",
					this), file, line);
	  }
	}
      }


    public int opCmp(string file = __FILE__,
		     size_t line = __LINE__, T)(T other)
      if(isBitVector!T &&
	 _vecCmpT!(typeof(this), T)) {
	reportX!(file, line)(other);
	alias vec!(typeof(this), T, "COMPARE") P;
	P lhs = this;
	P rhs = other;
	return lhs.compare(rhs);
      }

    public int opCmp(string file = __FILE__,
		     size_t line = __LINE__, T)(T other)
      if(isIntegral!T) {
	reportX!(file, line)(other);
	alias vec!(typeof(this), vec!T, "COMPARE") P;
	P lhs = this;
	P rhs = other;
	return lhs.compare(rhs);
      }

    public bool opEquals(string file = __FILE__,
			 size_t line = __LINE__, T)(T other)
      if(isIntegral!T || is(T == bool)) {
	reportX!(file, line)(other);
	alias vec!(typeof(this), vec!T, "COMPARE") P;
	P lhs = this;
	P rhs = other;
	return lhs.isEqual(rhs);
      }

    public bool opEquals(string file = __FILE__,
			 size_t line = __LINE__, T)(T other)
      if(isBitVector!T) {
	reportX!(file, line)(other);
	alias vec!(typeof(this), T, "COMPARE") P;
	P lhs = this;
	P rhs = other;
	return lhs.isEqual(rhs);
      }

    public int compare(T)(T other) // T shall have the same type as typeof(this)
      if(is(T == typeof(this))) {
	for(size_t i=1; i!=STORESIZE; ++i) {
	  if(this._aval[$-1-i] < other._aval[$-1-i]) return -1;
	  if(this._aval[$-1-i] > other._aval[$-1-i]) return 1;
	}

	if((this._aval[$-1] & UMASK) < (other._aval[$-1] & T.UMASK)) return -1;
	if((this._aval[$-1] & UMASK) > (other._aval[$-1] & T.UMASK)) return 1;

	return 0;
      }


    public bool isEqual(T)(T other)
      if(is(T == typeof(this))) {
	for(size_t i=1; i!=STORESIZE; ++i) {
	  if(this._aval[$-1-i] != other._aval[$-1-i]) return false;
	}
	if((this._aval[$-1] & UMASK) != (other._aval[$-1] & T.UMASK)) return false;
	return true;
      }

    // Bitwise Compliment
    public auto opUnary(string op)() if(op == "~") {
      // compliment every bit
      vec!(S,L,SIZE) result = this;
      for(size_t i; i != STORESIZE; ++i) {
	result._aval[i] = ~_aval[i];
      }
      // UMASK out the unused bits
      result._aval[$-1] &= UMASK;
      return result;
    }


    public void opOpAssign(string op, string file = __FILE__,
			   size_t line = __LINE__, T)(T other)
      if(isIntegral!T &&	(op == "+" || op == "-")) {
	reportX!(file, line)(other);
	vec!T rhs = other;
	this.opOpAssign!op(rhs);
      }

    public void opOpAssign(string op, string file = __FILE__,
			   size_t line = __LINE__, T)(T other)
      if(isBitVector!T && (op == "+" || op == "-")) {
	reportX!(file, line)(other);
	auto rhs = cast(typeof(this)) other;
	static if(this.SIZE <= 64) {
	  static if(op == "+") _aval[0] += rhs._aval[0];
	  else                 _aval[0] -= rhs._aval[0];
	  return;
	}
	else {
	  uint[] a = cast(uint[]) this._aval;
	  uint[] b = cast(uint[]) rhs._aval;
	  long carry = 0;
	  for(size_t i=0; i != a.length; ++i) {
	    static if(op == "+") {
	      long _r = carry + a[i] + b[i];
	      a[i] = cast(typeof(a[i])) _r;
	      carry = _r >> 32;
	    }
	    static if(op == "-") {
	      long _r = carry + a[i] - b[i];
	      a[i] = cast(typeof(a[i])) _r;
	      carry = _r >> 32;
	    }
	  }
	  if(this.aValSigned) {
	    _aval[$-1] |= SMASK;
	  }
	  else {
	    _aval[$-1] &= UMASK;
	  }
	}
      }

    public auto opUnary(string op)()
    if(op == "-") {
      typeof(this) result = 0;
      result -= this;
      return result;
    }

    public auto opBinary(string op, T)(T other)
    if(isIntegral!T &&
       (op == "+" || op == "-")) {
      vec!T rhs = other;
      return this.opBinary!op(rhs);
    }

    public auto opBinaryRight(string op, T)(T other)
    if(isIntegral!T &&
       (op == "+" || op == "-")) {
      vec!T rhs = other;
      return rhs.opBinary!op(this);
    }

    // Addition and Substraction with other BitVec
    public auto opBinary(string op, T)(T other)
    if(isBitVector!T && (op == "+" || op == "-")) {
      static if(SIZE >= T.SIZE) {
	// typeof(this) result = this;
	vec!(S, L, SIZE+1) result = this;
	static if(op == "+") result += other;
	else result -= other;
	return result;
      }
      else {
	static if(op == "+") {
	  vec!(T.ISSIGNED, T.IS4STATE, T.SIZE+1) result = other;
	}
	else {
	  vec!(T.IS4STATE, T.IS4STATE, T.SIZE+1) result = -other;
	}
	result += this;
	return result;
      }
    }

    public auto opBinary(string op)(size_t shift)
    if(op == "<<" || op == ">>" || op == ">>>") {
      typeof(this) result = this;
      result.opOpAssign!(op)(shift);
      return result;
    }

    public auto opBinary(string op, string file= __FILE__,
			 size_t line = __LINE__, T)(T other)
    if(isBitVector!T && (op == "*")) {
      reportX!(file, line)(other);
      // The result is signed only if both the operands are signed
      static if(ISSIGNED && other.ISSIGNED)
	enum bool _S = true;
      else
	enum bool _S = false;
      static if(IS4STATE && other.IS4STATE)
	enum bool _L = true;
      else
	enum bool _L = false;
      // result is addition of the SIZES
      enum SIZE_T _SIZE = SIZE + T.SIZE;

      vec!(_S,_L,_SIZE) result = 0;

      static if(result.SIZE <= 16) {
	result.store_t[] r = result._aval;
      }
      else {
	uint[] r = cast(uint[]) result._aval;
      }

      static if(this.SIZE <= 16) {
	store_t[] a = this._aval;
      }
      else {
	uint[] a = cast(uint[]) this._aval;
      }

      static if(other.SIZE <= 16) {
	other.store_t[] b = other._aval;
      }
      else {
	uint[] b = cast(uint[]) other._aval;
      }

      bool aNegative = this.aValSigned();
      bool bNegative = other.aValSigned();

      uint _a = void;
      uint _b = void;
      for(size_t i=0; i!=r.length; ++i) {
	uint carry = 0;
	if(i < a.length) {	     // initialize and sign extend if required
	  _a = a[i];
	  if(aNegative) {
	    static if(store_t.sizeof == 8) {
	      // convert 64 bit mask to 32 bits
	      static if(SIZE % 64 <= 32) {
		if(i == a.length - 2)
		  _a |= cast(uint) ~UMASK;
		if(i == a.length - 1)
		  _a = uint.max;
	      }
	      else {
		if(i == a.length - 1)
		  _a |= cast(uint) ~(UMASK >>> 32);
	      }
	    }
	    else {
	      if(i == a.length - 1)
		_a |= ~(cast(uint) UMASK);
	    }
	  }
	}
	else {
	  if(aNegative)
	    _a = uint.max;
	  else
	    if(carry == 0) break;
	    else _a = 0;
	}
	for(size_t j=0; j!=r.length-i; ++j) {
	  if(j < b.length) {     // initialize and sign extend if required
	    _b = b[j];
	    if(bNegative) {
	      static if(T.store_t.sizeof == 8) {
		// convert 64 bit mask to 32 bits
		static if(T.SIZE % 64 <= 32) {
		  if(j == b.length - 2)
		    _b |= cast(uint) ~T.UMASK;
		  if(j == b.length - 1)
		    _b = uint.max;
		}
		else {
		  if(j == b.length - 1)
		    _b |= cast(uint) ~(T.UMASK >>> 32);
		}
	      }
	      else {
		if(j == b.length - 1)
		  _b |= ~(cast(uint) T.UMASK);
	      }
	    }
	  }
	  else {
	    if(bNegative)
	      _b = uint.max;
	    else
	      if(carry == 0) break;
	      else _b = 0;
	  }

	  ulong t = cast(ulong)carry + cast(ulong)r[i+j] +
	    cast(ulong)_a * cast(ulong)_b;
	  // writefln("i: %d, j: %d, carry: %X, r[i+j]: %X, _a: %X, _b: %X",
	  //	       i, j, carry, r[i+j], _a, _b);
	  r[i+j] = cast(typeof(r[i+j])) t;
	  carry = cast(uint)(t >>> 32);
	}
      }

      result._aval[$-1] &= result.UMASK;
      return result;
    }

    // Left Shift Assign
    public void opOpAssign(string op)(size_t shift)
      if(op == "<<") {
	auto wordShift = shift / WORDSIZE;
	auto bitShift = shift % WORDSIZE;
	if(wordShift > 0) {
	  for(size_t i=STORESIZE; i!=0; --i) {
	    if(i > wordShift) _aval[i-1] = _aval[(i-1)-wordShift];
	    else _aval[i-1] = 0;
	    static if(L) {
	      if(i > wordShift) _bval[i-1] = _bval[(i-1)-wordShift];
	      else _bval[i-1] = 0;
	    }
	  }
	}
	if(bitShift != 0) {
	  for(size_t i=STORESIZE; i!=0; --i) {
	    _aval[i-1] <<= bitShift;
	    if(i > 1) {
	      _aval[i-1] |= _aval[i-2] >>>(WORDSIZE - bitShift);
	    }
	    static if(L) {
	      _bval[i-1] <<= bitShift;
	      if(i > 1) {
		_bval[i-1] |= _bval[i-2] >>>(WORDSIZE - bitShift);
	      }
	    }
	  }
	}
	// UMASK out the unused bits
	if(this.aValSigned) _aval[$-1] |= SMASK;
	else		     _aval[$-1] &= UMASK;
	static if(L) {
	  if(this.bValSigned) _bval[$-1] |= SMASK;
	  else		     _bval[$-1] &= UMASK;
	}
      }

    // Right Shift Assign
    public void opOpAssign(string op)(size_t shift)
      if(op == ">>" ||		// sign extended
	 op == ">>>") {		// normal
	auto wordShift = shift / WORDSIZE;
	auto bitShift = shift % WORDSIZE;
	if(wordShift > 0) {
	  for(size_t i=0; i!=STORESIZE; ++i) {
	    if(i+wordShift < STORESIZE) _aval[i] = _aval[i+wordShift];
	    else {
	      static if(op == ">>") {
		if(this.aValSigned) _aval[i] = ~0;
		else                 _aval[i] = 0;
	      }
	      static if(op == ">>>") {
		_aval[i] = 0;
	      }
	    }
	    static if(L) {
	      if(i+wordShift < STORESIZE) _bval[i] = _bval[i+wordShift];
	      else {
		static if(op == ">>") {
		  if(this.bValSigned) _bval[i] = ~0;
		  else                 _bval[i] = 0;
		}
		static if(op == ">>>") {
		  _bval[i] = 0;
		}
	      }
	    }
	  }
	}
	if(bitShift != 0) {
	  for(size_t i=0; i!=STORESIZE; ++i) {
	    if(i < STORESIZE-1) {
	      _aval[i] >>>= bitShift;
	      _aval[i] |= _aval[i+1] <<(WORDSIZE - bitShift);
	      static if(L) {
		_bval[i] >>>= bitShift;
		_bval[i] |= _bval[i+1] <<(WORDSIZE - bitShift);
	      }
	    }
	    else {
	      static if(op == ">>") _aval[i] >>= bitShift;
	      static if(op == ">>>") _aval[i] >>>= bitShift;
	      if(this.aValSigned) _aval[$-1] |= SMASK;
	      else                 _aval[$-1] &= UMASK;
	      static if(L) {
		static if(op == ">>") _bval[i] >>= bitShift;
		static if(op == ">>>") _bval[i] >>>= bitShift;
		if(this.bValSigned) _bval[$-1] |= SMASK;
		else                 _bval[$-1] &= UMASK;
	      }
	    }
	  }
	}
      }


    // Concatenation
    public auto opBinary(string op, T)(T other)
    if(isIntegral!T &&
       (op == "~")) {
      vec!T rhs = other;
      return this ~ rhs;
    }

    public auto opBinary(string op, T)(T other)
    if(isBitVector!T && (op == "~")) {
      BitVec!(SIZE+T.SIZE) result = this;
      result <<= T.SIZE;
      result |= other;
      return result;
    }

    // int opApply(scope int delegate(ref bool) dg)
    // int opApply(scope int delegate(ref bit) dg) {
    //   int result = 0;
    //   for(size_t i = 0; i < SIZE; i++) {
    //     bit b = this.opIndex(i);
    //     result = dg(b);
    //     this[i] = b;
    //     if(result) break;
    //   }
    //   return result;
    // }

    /** ditto */
    // int opApply(scope int delegate(ref size_t, ref bool) dg)
    // int opApply(int delegate(ref size_t, ref bit) dg) {
    //   int result = 0;
    //   for(size_t i = 0; i < SIZE; i++) {
    //     bit b = this.opIndex(i);
    //     result = dg(i, b);
    //     this[i] = b;
    //     if(result) break;
    //   }
    //   return result;
    // }


}

// Utility functions
private uint multibyteDivAssign(uint [] dest, uint divisor, uint overflow) {
  ulong c = cast(ulong)overflow;
  for(ptrdiff_t i = dest.length-1; i>= 0; --i) {
    c =(c<<32) + cast(ulong)(dest[i]);
    uint q = cast(uint)(c/divisor);
    c -= divisor * q;
    dest[i] = q;
  }
  return cast(uint)c;
}

private void itoaZeroPadded(char[] output, uint value, int radix = 10) {
  ptrdiff_t x = output.length - 1;
  for( ; x >= 0; --x) {
    output[x]= cast(char)(value % radix + '0');
    value /= radix;
  }
}

package size_t biguintToDecimal(char [] buff, uint [] data) {
  ptrdiff_t sofar = buff.length;
  // Might be better to divide by(10^38/2^32) since that gives 38 digits for
  // the price of 3 divisions and a shr; this version only gives 27 digits
  // for 3 divisions.
  while(data.length>1) {
    uint rem = multibyteDivAssign(data, 10_0000_0000, 0);
    itoaZeroPadded(buff[sofar-9 .. sofar], rem);
    sofar -= 9;
    if(data[$-1] == 0 && data.length > 1) {
      data.length = data.length - 1;
    }
  }
  itoaZeroPadded(buff[sofar-10 .. sofar], data[0]);
  sofar -= 10;
  // and strip off the leading zeros
  while(sofar!= buff.length-1 && buff[sofar] == '0')
    sofar++;
  return sofar;
}

private void toHexZeroPadded(char[] output, uint value) {
  ptrdiff_t x = output.length - 1;
  static immutable string hexDigits = "0123456789ABCDEF";
  for( ; x>=0; --x) {
    output[x] = hexDigits[value & 0xF];
    value >>= 4;
  }
}

public auto toBits(T)(T val) {
  static if(T.sizeof >= 8) {
    enum WSIZE = (T.sizeof+7)/8;
    alias ulong U;
  }
  else static if(T.sizeof >= 4) {
      enum WSIZE = (T.sizeof+3)/4;
      alias uint U;
    }
  else static if(T.sizeof >= 2) {
      enum WSIZE = (T.sizeof+1)/2;
      alias ushort U;
    }
  else static if(T.sizeof == 1) {
      enum WSIZE = T.sizeof;
      alias ubyte U;
    }

  union utype {
    U[WSIZE] b;
    T f;
  }

  utype u;
  u.f = val;

  alias UBitVec!(8*T.sizeof) V;
  V retval;

  for(size_t i=0; i!=V.STORESIZE; ++i) {
    retval._aval[i] = u.b[i];
  }
  return retval;
}

public void fromBits(T, B)(ref T val, B bv)
  if(isBitVector!B && 8*T.sizeof == B.SIZE) {
    static if(B.IS4STATE) {
      assert(bv.bVal == 0);
    }
    static if(T.sizeof >= 8) {
      enum WSIZE = (T.sizeof+7)/8;
      alias ulong U;
    }
    else static if(T.sizeof >= 4) {
	enum WSIZE = (T.sizeof+3)/4;
	alias uint U;
      }
    else static if(T.sizeof >= 2) {
	enum WSIZE = (T.sizeof+1)/2;
	alias ushort U;
      }
    else static if(T.sizeof == 1) {
	enum WSIZE = T.sizeof;
	alias ubyte U;
      }

    union utype {
      U[WSIZE] b;
      T f;
    }

    utype u;

    for(size_t i=0; i!=B.STORESIZE; ++i) {
      u.b[i] = bv._aval[i];
    }

    val = u.f;
  }


alias BitVec bvec;
alias UBitVec ubvec;
alias LogicVec lvec;
alias ULogicVec ulvec;




/*    */
unittest {
  import std.random ;
  import std.math ;
  import std.stdio ;
  for(ulong k = 1 ; k < 64 ; ++k){
    static ubvec!64     a ; 
    static ubvec!64     b ;  
    for(uint i = 0 ; i < 1000 ; ++i){
      auto a_1 = uniform(0, (pow(2,k)-1)); 
      auto b_1 = uniform(0, (pow(2,k)-1)); 
      a = a_1 ;
      b = b_1 ;
      auto y = cast(ubvec!64) (a + b) ;
      try {
	assert(y == (a_1 + b_1));
      } catch (core.exception.AssertError) {
	writefln(" Err :: Assertion failed for addition, bitwidth = %d",k);
      }
    }
  }

}


unittest {
  import std.random ;
  import std.math ;
  import std.stdio ;
  for(ulong k = 1 ; k < 64 ; ++k){
    static ubvec!64     a ; 
    static ubvec!64     b ;  
    for(uint i = 0 ; i < 1000 ; ++i){
      auto a_1 = uniform(0, (pow(2,k)-1)); 
      auto b_1 = uniform(0, (pow(2,k)-1)); 
      a = a_1 ;
      b = b_1 ;
      auto y = cast(ubvec!64) (a - b);
      try {
	assert(y == (a_1 - b_1));
      } catch (core.exception.AssertError) {
	writefln(" Err :: Assertion failed for subtraction, bitwidth = %d",k);
      }
    }
  }

  ubvec!8  a = cast(byte) 0xff ;
  ubvec!8  b = cast(byte) 0xff ;
  ubvec!9  y = a + b;
  assert(y == 510) ;

  y = cast(ubvec!9)(a + b) ;
  assert(y == 510) ;

}


unittest {
  import std.random ;
  import std.math ;
  import std.stdio ;
  for(ulong k = 1 ; k != 33 ; ++k){
    static ubvec!64     a ; 
    static ubvec!64     b ;  
    // static ubvec!64     y ;
    for(uint i = 0 ; i != 1000; ++i){
      auto a_1 = uniform(0, (pow(2,k)-1)); 
      auto b_1 = uniform(0, (pow(2,k)-1)); 
      a = a_1 ;
      b = b_1 ;
      auto y = cast(ubvec!64) (a * b) ;
      try {
	assert(y == (a_1 * b_1));
      } catch (core.exception.AssertError) {
	writefln(" Err :: Assertion failed for multiplication, bitwidth = %d",k);
      }
    }
  }

}

unittest {
  import std.random ;
  import std.math ;
  import std.stdio ;

  ubvec!63[]   mubvec ;
  ubvec!63[]   nubvec ;
  ubvec!63[16] pubvec ;
  mubvec.length = 16 ;
  nubvec.length = 16 ;
}


unittest {
  import std.random ;
  import std.math ;
  import std.stdio ;
  for(ulong k = 1 ; k < 15 ; ++k){
    static bvec!63     a ; 
    static bvec!63     b ;  
    for(uint i = 0 ; i < 1000 ; ++i){
      auto a_1 = uniform(-1000, 1000); 
      auto b_1 = uniform(-1000, 1000); 
      a = a_1 ;
      b = b_1 ;
      auto y = a + b ;
      try {
	assert(y == (a_1 + b_1));
      } catch (core.exception.AssertError) {
	writefln(" Err :: Assertion failed for addition, bitwidth = %d",k);
      }
    }
  }

}




unittest {
  import std.random ;
  import std.math ;
  import std.stdio ;
  for(ulong k = 1 ; k < 15 ; ++k){
    static bvec!63     a ; 
    static bvec!63     b ;  
    for(uint i = 0 ; i < 1000 ; ++i){
      auto a_1 = uniform(-1000, 1000); 
      auto b_1 = uniform(-1000, 1000); 
      a = a_1 ;
      b = b_1 ;
      auto y = a - b ;
      try {
	assert(y == (a_1 - b_1));
      } catch (core.exception.AssertError) {
	writefln(" Err :: Assertion failed for subtraction, bitwidth = %d",k);
      }
    }
  }

  bvec!8  a = cast(byte) 0xff ;
  bvec!8  b = cast(byte) 0xff ;
  auto z = cast(bvec!9)a + cast(bvec!9)b;
  bvec!9  y = a + b;
  assert(y == -2) ;

  y = cast(bvec!9)(a + b) ;
  assert(y == -2) ;

}

unittest {
  import std.random ;
  import std.math ;
  import std.stdio ;
  for(ulong k = 1 ; k != 4 ; ++k){
    static bvec!63     a ; 
    static bvec!63     b ;  
    // static bvec!64     y ;
    for(uint i = 0 ; i != 4; ++i){
      auto a_1 = uniform(-1000, 1000); 
      auto b_1 = uniform(-1000, 1000); 
      a = a_1 ;
      b = b_1 ;
      auto y = a * b ;
      try {
	assert(y == (a_1 * b_1));
      } catch (core.exception.AssertError) {
	writefln(" Err :: Assertion failed for multiplication, bitwidth = %d, a = %d, b = %d, y = %b, y1 = %b", k, a, b, y, a_1*b_1);
	writeln(typeid(typeof(y)));
      }
    }
  }

}
