// Written in the D programming language.

/**
Copyright: Copyright Digital Mars 2007 - 2011.
           Coverify Systems Technology 2012 - 2013
License:   Distributed under the Boost Software License, Version 1.0.
           (See accompanying file LICENSE_1_0.txt or copy at
           http://www.boost.org/LICENSE_1_0.txt)
Authors:   $(WEB digitalmars.com, Walter Bright),
	   $(WEB erdani.org, Andrei Alexandrescu),
	   Jonathan M Davis,
	   Alex Rønne Petersen,
	   Damian Ziemba
	   Puneet Goel <puneet@coverify.com>
*/

module esdl.data.bstr;

import std.conv;
import std.string;
import std.metastrings;
import std.traits;
import std.format;
import std.bitmanip;
import core.bitop;
import esdl.data.bvec;

alias BitString!true  lstr;
alias BitString!false bstr;

private ubyte _log2(size_t n) {
  if(n == 1) return 0;
  else return cast(ubyte)(1 + _log2(n/2));
}

struct BitString(bool L)
{

  size_t len;

  alias len opDollar;

  size_t* aptr;
  static if(L)
    {
      size_t* bptr;
    }

  enum bitsPerSizeT = size_t.sizeof * 8;

  /**********************************************
   * Gets the amount of native words backing this $(D BitArray).
   */
  @property const size_t dim() {
    return (len + (bitsPerSizeT-1)) / bitsPerSizeT;
  }

  /**********************************************
   * Gets the amount of bits in the $(D BitArray).
   */
  @property const size_t length() {
    return len;
  }

  /**********************************************
   * Sets the amount of bits in the $(D BitArray).
   */
  @property size_t length(size_t newlen) {
    if (newlen != len) {
      size_t olddim = dim;
      size_t newdim = (newlen + (bitsPerSizeT-1)) / bitsPerSizeT;

      if (newdim != olddim) {
        // Create a fake array so we can use D's realloc machinery
        auto a = aptr[0 .. olddim];
        a.length = newdim;                // realloc
        aptr = a.ptr;
        static if(L) {
          auto b = bptr[0 .. olddim];
          b.length = newdim;                // realloc
          bptr = b.ptr;
        }
        if (newlen & (bitsPerSizeT-1)) {   // Set any pad bits to 0
          aptr[newdim - 1] &= ~(~0L << (newlen & (bitsPerSizeT-1)));
          static if(L) {
            bptr[newdim - 1] &= ~(~0L << (newlen & (bitsPerSizeT-1)));
          }
        }
      }
      len = newlen;
    }
    return len;
  }

  /**********************************************
   * Gets the $(D i)'th bit in the $(D BitArray).
   */
  auto opIndex(size_t i) const
  in {
    assert(i < len);
  }
  body {
    static if(L) {
      if(bt(bptr, i)) {
        if(bt(aptr, i)) return LOGIC_X;
        else            return LOGIC_Z;
      }
      else {
        if(bt(aptr, i)) return LOGIC_1;
        else            return LOGIC_0;
      }
    }
    else {
      if(bt(aptr, i)) return BIT_1;
      else            return BIT_0;
    }
  }

  //unittest
  //{
  //  void Fun(const lstr arr)
  //  {
  //    auto x = arr[0];
  //    assert(x == LOGIC_X);
  //  }
  //  LogicArray a;
  //  a.length = 3;
  //  a[0] = LOGIC_X;
  //  Fun(a);
  //}

  /**********************************************
   * Sets the $(D i)'th bit in the $(D BitArray).
   */
  bool opIndexAssign()(bool b, size_t i)
    in {
      assert(i < len);
    }
  body {
    static if(L) btr(bptr, i);
    if (b)       bts(aptr, i);
    else         btr(aptr, i);
    return b;
  }

  auto opIndexAssign(T)(T b, size_t i)
  if(isBitVector!T && T.SIZE == 1 && (L || (! T.IS4STATE)))
    in {
      assert(i < len);
    }
  body {
    static if(L) {
      if (b.bVal) bts(bptr, i);
      else        btr(bptr, i);
    }
    if (b.aVal)   bts(aptr, i);
    else          btr(aptr, i);
    return b;
  }

  /**********************************************
   * Duplicates the $(D BitArray) and its contents.
   */
  @property BitString!L dup() const {
    BitString!L ba;
    auto a = aptr[0 .. dim].dup;
    ba.aptr = a.ptr;
    static if(L) {
      auto b = bptr[0 .. dim].dup;
      ba.bptr = b.ptr;
    }
    ba.len = len;
    return ba;
  }

  unittest
  {
    bstr a;
    bstr b;
    int i;

    debug(bitstream) printf("BitArray.dup.unittest\n");

    a.length = 3;
    a[0] = true; a[1] = false; a[2] = true;
    b = a.dup;
    assert(b.length is 3);
    for (i = 0; i < 3; i++)
      {   debug(bitarray) printf("b[%d] = %d\n", i, b[i]);
        assert(b[i] == (((i ^ 1) & 1) ? true : false));
      }
  }

  // Will include later -- not required for mow.
  // /**********************************************
  //  * Support for $(D foreach) loops for $(D BitArray).
  //  */
  // static if(L)
  //   {
  //     int opApply(scope int delegate(ref ulvec!1) dg)
  //     {
  //    int result;

  //    for (size_t i = 0; i < len; i++)
  //      {
  //        auto b = opIndex(i);
  //        result = dg(b);
  //        this[i] = b;
  //        if (result)
  //          break;
  //      }
  //    return result;
  //     }

  //     /** ditto */
  //     int opApply(scope int delegate(ulvec!1) dg) const
  //     {
  //    int result;

  //    for (size_t i = 0; i < len; i++)
  //      {
  //        auto b = opIndex(i);
  //        result = dg(b);
  //        if (result)
  //          break;
  //      }
  //    return result;
  //     }

  //     /** ditto */
  //     int opApply(scope int delegate(ref size_t, ref ulvec!1) dg)
  //     {
  //    int result;

  //    for (size_t i = 0; i < len; i++)
  //      {
  //        auto b = opIndex(i);
  //        result = dg(i, b);
  //        this[i] = b;
  //        if (result)
  //          break;
  //      }
  //    return result;
  //     }

  //     /** ditto */
  //     int opApply(scope int delegate(size_t, ulvec!1) dg) const
  //     {
  //    int result;

  //    for (size_t i = 0; i < len; i++)
  //      {
  //        auto b = opIndex(i);
  //        result = dg(i, b);
  //        if (result)
  //          break;
  //      }
  //    return result;
  //     }
  //   }
  //   {
  //     int opApply(scope int delegate(ref bool) dg)
  //     {
  //    int result;

  //    for (size_t i = 0; i < len; i++)
  //      {
  //        bool b = opIndex(i);
  //        result = dg(b);
  //        this[i] = b;
  //        if (result)
  //          break;
  //      }
  //    return result;
  //     }

  //     /** ditto */
  //     int opApply(scope int delegate(bool) dg) const
  //     {
  //    int result;

  //    for (size_t i = 0; i < len; i++)
  //      {
  //        bool b = opIndex(i);
  //        result = dg(b);
  //        if (result)
  //          break;
  //      }
  //    return result;
  //     }

  //     /** ditto */
  //     int opApply(scope int delegate(ref size_t, ref bool) dg)
  //     {
  //    int result;

  //    for (size_t i = 0; i < len; i++)
  //      {
  //        bool b = opIndex(i);
  //        result = dg(i, b);
  //        this[i] = b;
  //        if (result)
  //          break;
  //      }
  //    return result;
  //     }

  //     /** ditto */
  //     int opApply(scope int delegate(size_t, bool) dg) const
  //     {
  //    int result;

  //    for (size_t i = 0; i < len; i++)
  //      {
  //        bool b = opIndex(i);
  //        result = dg(i, b);
  //        if (result)
  //          break;
  //      }
  //    return result;
  //     }
  //   }
  // unittest
  // {
  //   debug(bitstring) printf("BitString.opApply unittest\n");

  //   static bool[] ba = [1,0,1];

  //   BitArray a; a.init(ba);

  //   int i;
  //   foreach (b;a)
  //     {
  //    switch (i)
  //      {
  //      case 0: assert(b is true); break;
  //      case 1: assert(b is false); break;
  //      case 2: assert(b is true); break;
  //      default: assert(0);
  //      }
  //    i++;
  //     }

  //   foreach (j,b;a)
  //     {
  //    switch (j)
  //      {
  //      case 0: assert(b is true); break;
  //      case 1: assert(b is false); break;
  //      case 2: assert(b is true); break;
  //      default: assert(0);
  //      }
  //     }
  // }


  /**********************************************
   * Reverses the bits of the $(D BitArray).
   */
  @property BitString!L reverse()
  out (result) {
    assert(result == this);
  }
  body {
    if (len >= 2) {
      size_t lo, hi;
      lo = 0;
      hi = len - 1;
      for (; lo < hi; ++lo, --hi) {
        auto t = this[lo];
        this[lo] = this[hi];
        this[hi] = t;
      }
    }
    return this;
  }

  //unittest
  //{
  //  debug(bitstring) printf("BitString.reverse.unittest\n");

  //  BitString b;
  //  static bool[5] data = [1,0,1,1,0];
  //  int i;

  //  b.init(data);
  //  b.reverse;
  //  for (i = 0; i < data.length; i++)
  //    {
  //      assert(b[i] == data[4 - i]);
  //    }
  //}


  // /***************************************
  //  * Support for operators == and != for $(D BitArray).
  //  */
  // const bool opEquals(const ref BitArray a2)
  // {
  //   int i;

  //   if (this.length != a2.length)
  //     return 0;                // not equal
  //   byte *p1 = cast(byte*)this.ptr;
  //   byte *p2 = cast(byte*)a2.ptr;
  //   auto n = this.length / 8;
  //   for (i = 0; i < n; i++)
  //     {
  //    if (p1[i] != p2[i])
  //      return 0;                // not equal
  //     }

  //   ubyte mask;

  //   n = this.length & 7;
  //   mask = cast(ubyte)((1 << n) - 1);
  //   //printf("i = %d, n = %d, mask = %x, %x, %x\n", i, n, mask, p1[i], p2[i]);
  //   return (mask == 0) || (p1[i] & mask) == (p2[i] & mask);
  // }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opEquals unittest\n");

  //   static bool[] ba = [1,0,1,0,1];
  //   static bool[] bb = [1,0,1];
  //   static bool[] bc = [1,0,1,0,1,0,1];
  //   static bool[] bd = [1,0,1,1,1];
  //   static bool[] be = [1,0,1,0,1];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);
  //   BitArray c; c.init(bc);
  //   BitArray d; d.init(bd);
  //   BitArray e; e.init(be);

  //   assert(a != b);
  //   assert(a != c);
  //   assert(a != d);
  //   assert(a == e);
  // }

  // /***************************************
  //  * Supports comparison operators for $(D BitArray).
  //  */
  // int opCmp(BitArray a2) const
  // {
  //   uint i;

  //   auto len = this.length;
  //   if (a2.length < len)
  //     len = a2.length;
  //   ubyte* p1 = cast(ubyte*)this.ptr;
  //   ubyte* p2 = cast(ubyte*)a2.ptr;
  //   auto n = len / 8;
  //   for (i = 0; i < n; i++)
  //     {
  //    if (p1[i] != p2[i])
  //      break;                // not equal
  //     }
  //   for (uint j = i * 8; j < len; j++)
  //     {
  //    ubyte mask = cast(ubyte)(1 << j);
  //    int c;

  //    c = cast(int)(p1[i] & mask) - cast(int)(p2[i] & mask);
  //    if (c)
  //      return c;
  //     }
  //   return cast(int)this.len - cast(int)a2.length;
  // }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opCmp unittest\n");

  //   static bool[] ba = [1,0,1,0,1];
  //   static bool[] bb = [1,0,1];
  //   static bool[] bc = [1,0,1,0,1,0,1];
  //   static bool[] bd = [1,0,1,1,1];
  //   static bool[] be = [1,0,1,0,1];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);
  //   BitArray c; c.init(bc);
  //   BitArray d; d.init(bd);
  //   BitArray e; e.init(be);

  //   assert(a >  b);
  //   assert(a >= b);
  //   assert(a <  c);
  //   assert(a <= c);
  //   assert(a <  d);
  //   assert(a <= d);
  //   assert(a == e);
  //   assert(a <= e);
  //   assert(a >= e);
  // }

  // /***************************************
  //  * Support for hashing for $(D BitArray).
  //  */
  // size_t toHash() const pure nothrow
  // {
  //   size_t hash = 3557;
  //   auto n  = len / 8;
  //   for (int i = 0; i < n; i++)
  //     {
  //    hash *= 3559;
  //    hash += (cast(byte*)this.ptr)[i];
  //     }
  //   for (size_t i = 8*n; i < len; i++)
  //     {
  //    hash *= 3571;
  //    hash += bt(this.ptr, i);
  //     }
  //   return hash;
  // }

  // /***************************************
  //  * Set this $(D BitArray) to the contents of $(D ba).
  //  */
  // void init(bool[] ba)
  // {
  //   length = ba.length;
  //   foreach (i, b; ba)
  //     {
  //    this[i] = b;
  //     }
  // }


  // /***************************************
  //  * Map the $(D BitArray) onto $(D v), with $(D numbits) being the number of bits
  //  * in the array. Does not copy the data.
  //  *
  //  * This is the inverse of $(D opCast).
  //  */
  // void init(void[] v, size_t numbits)
  //   in
  //     {
  //       assert(numbits <= v.length * 8);
  //       assert((v.length & 3) == 0);
  //     }
  // body
  //   {
  //     ptr = cast(size_t*)v.ptr;
  //     len = numbits;
  //   }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.init unittest\n");

  //   static bool[] ba = [1,0,1,0,1];

  //   BitArray a; a.init(ba);
  //   BitArray b;
  //   void[] v;

  //   v = cast(void[])a;
  //   b.init(v, a.length);

  //   assert(b[0] == 1);
  //   assert(b[1] == 0);
  //   assert(b[2] == 1);
  //   assert(b[3] == 0);
  //   assert(b[4] == 1);

  //   a[0] = 0;
  //   assert(b[0] == 0);

  //   assert(a == b);
  // }

  // /***************************************
  //  * Convert to $(D void[]).
  //  */
  // void[] opCast(T : void[])()
  // {
  //   return cast(void[])ptr[0 .. dim];
  // }

  // /***************************************
  //  * Convert to $(D size_t[]).
  //  */
  // size_t[] opCast(T : size_t[])()
  // {
  //   return ptr[0 .. dim];
  // }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opCast unittest\n");

  //   static bool[] ba = [1,0,1,0,1];

  //   BitArray a; a.init(ba);
  //   void[] v = cast(void[])a;

  //   assert(v.length is a.dim * size_t.sizeof);
  // }

  // /***************************************
  //  * Support for unary operator ~ for $(D BitArray).
  //  */
  // BitArray opCom()
  // {
  //   auto dim = this.dim;

  //   BitArray result;

  //   result.length = len;
  //   for (size_t i = 0; i < dim; i++)
  //     result.ptr[i] = ~this.ptr[i];
  //   if (len & (bitsPerSizeT-1))
  //     result.ptr[dim - 1] &= ~(~0 << (len & (bitsPerSizeT-1)));
  //   return result;
  // }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opCom unittest\n");

  //   static bool[] ba = [1,0,1,0,1];

  //   BitArray a; a.init(ba);
  //   BitArray b = ~a;

  //   assert(b[0] is 0);
  //   assert(b[1] is 1);
  //   assert(b[2] is 0);
  //   assert(b[3] is 1);
  //   assert(b[4] is 0);
  // }


  // /***************************************
  //  * Support for binary operator & for $(D BitArray).
  //  */
  // BitArray opAnd(BitArray e2)
  //   in
  //     {
  //       assert(len is e2.length);
  //     }
  // body
  //   {
  //     auto dim = this.dim;

  //     BitArray result;

  //     result.length = len;
  //     for (size_t i = 0; i < dim; i++)
  //    result.ptr[i] = this.ptr[i] & e2.ptr[i];
  //     return result;
  //   }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opAnd unittest\n");

  //   static bool[] ba = [1,0,1,0,1];
  //   static bool[] bb = [1,0,1,1,0];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);

  //   BitArray c = a & b;

  //   assert(c[0] is 1);
  //   assert(c[1] is 0);
  //   assert(c[2] is 1);
  //   assert(c[3] is 0);
  //   assert(c[4] is 0);
  // }


  // /***************************************
  //  * Support for binary operator | for $(D BitArray).
  //  */
  // BitArray opOr(BitArray e2) const
  //   in
  //     {
  //       assert(len is e2.length);
  //     }
  // body
  //   {
  //     auto dim = this.dim;

  //     BitArray result;

  //     result.length = len;
  //     for (size_t i = 0; i < dim; i++)
  //    result.ptr[i] = this.ptr[i] | e2.ptr[i];
  //     return result;
  //   }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opOr unittest\n");

  //   static bool[] ba = [1,0,1,0,1];
  //   static bool[] bb = [1,0,1,1,0];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);

  //   BitArray c = a | b;

  //   assert(c[0] is 1);
  //   assert(c[1] is 0);
  //   assert(c[2] is 1);
  //   assert(c[3] is 1);
  //   assert(c[4] is 1);
  // }


  // /***************************************
  //  * Support for binary operator ^ for $(D BitArray).
  //  */
  // BitArray opXor(BitArray e2) const
  //   in
  //     {
  //       assert(len is e2.length);
  //     }
  // body
  //   {
  //     auto dim = this.dim;

  //     BitArray result;

  //     result.length = len;
  //     for (size_t i = 0; i < dim; i++)
  //    result.ptr[i] = this.ptr[i] ^ e2.ptr[i];
  //     return result;
  //   }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opXor unittest\n");

  //   static bool[] ba = [1,0,1,0,1];
  //   static bool[] bb = [1,0,1,1,0];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);

  //   BitArray c = a ^ b;

  //   assert(c[0] is 0);
  //   assert(c[1] is 0);
  //   assert(c[2] is 0);
  //   assert(c[3] is 1);
  //   assert(c[4] is 1);
  // }


  // /***************************************
  //  * Support for binary operator - for $(D BitArray).
  //  *
  //  * $(D a - b) for $(D BitArray) means the same thing as $(D a &amp; ~b).
  //  */
  // BitArray opSub(BitArray e2) const
  //   in
  //     {
  //       assert(len is e2.length);
  //     }
  // body
  //   {
  //     auto dim = this.dim;

  //     BitArray result;

  //     result.length = len;
  //     for (size_t i = 0; i < dim; i++)
  //    result.ptr[i] = this.ptr[i] & ~e2.ptr[i];
  //     return result;
  //   }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opSub unittest\n");

  //   static bool[] ba = [1,0,1,0,1];
  //   static bool[] bb = [1,0,1,1,0];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);

  //   BitArray c = a - b;

  //   assert(c[0] is 0);
  //   assert(c[1] is 0);
  //   assert(c[2] is 0);
  //   assert(c[3] is 0);
  //   assert(c[4] is 1);
  // }


  // /***************************************
  //  * Support for operator &= for $(D BitArray).
  //  */
  // BitArray opAndAssign(BitArray e2)
  //   in
  //     {
  //       assert(len is e2.length);
  //     }
  // body
  //   {
  //     auto dim = this.dim;

  //     for (size_t i = 0; i < dim; i++)
  //    ptr[i] &= e2.ptr[i];
  //     return this;
  //   }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opAndAssign unittest\n");

  //   static bool[] ba = [1,0,1,0,1];
  //   static bool[] bb = [1,0,1,1,0];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);

  //   a &= b;
  //   assert(a[0] is 1);
  //   assert(a[1] is 0);
  //   assert(a[2] is 1);
  //   assert(a[3] is 0);
  //   assert(a[4] is 0);
  // }


  // /***************************************
  //  * Support for operator |= for $(D BitArray).
  //  */
  // BitArray opOrAssign(BitArray e2)
  //   in
  //     {
  //       assert(len is e2.length);
  //     }
  // body
  //   {
  //     auto dim = this.dim;

  //     for (size_t i = 0; i < dim; i++)
  //    ptr[i] |= e2.ptr[i];
  //     return this;
  //   }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opOrAssign unittest\n");

  //   static bool[] ba = [1,0,1,0,1];
  //   static bool[] bb = [1,0,1,1,0];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);

  //   a |= b;
  //   assert(a[0] is 1);
  //   assert(a[1] is 0);
  //   assert(a[2] is 1);
  //   assert(a[3] is 1);
  //   assert(a[4] is 1);
  // }

  // /***************************************
  //  * Support for operator ^= for $(D BitArray).
  //  */
  // BitArray opXorAssign(BitArray e2)
  //   in
  //     {
  //       assert(len is e2.length);
  //     }
  // body
  //   {
  //     auto dim = this.dim;

  //     for (size_t i = 0; i < dim; i++)
  //    ptr[i] ^= e2.ptr[i];
  //     return this;
  //   }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opXorAssign unittest\n");

  //   static bool[] ba = [1,0,1,0,1];
  //   static bool[] bb = [1,0,1,1,0];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);

  //   a ^= b;
  //   assert(a[0] is 0);
  //   assert(a[1] is 0);
  //   assert(a[2] is 0);
  //   assert(a[3] is 1);
  //   assert(a[4] is 1);
  // }

  // /***************************************
  //  * Support for operator -= for $(D BitArray).
  //  *
  //  * $(D a -= b) for $(D BitArray) means the same thing as $(D a &amp;= ~b).
  //  */
  // BitArray opSubAssign(BitArray e2)
  //   in
  //     {
  //       assert(len is e2.length);
  //     }
  // body
  //   {
  //     auto dim = this.dim;

  //     for (size_t i = 0; i < dim; i++)
  //    ptr[i] &= ~e2.ptr[i];
  //     return this;
  //   }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opSubAssign unittest\n");

  //   static bool[] ba = [1,0,1,0,1];
  //   static bool[] bb = [1,0,1,1,0];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);

  //   a -= b;
  //   assert(a[0] is 0);
  //   assert(a[1] is 0);
  //   assert(a[2] is 0);
  //   assert(a[3] is 0);
  //   assert(a[4] is 1);
  // }

  /***************************************
   * Support for operator ~= for $(D BitArray).
   */

  auto opCatAssign(bool BIGENDIAN=false, T)(T t)
  if(is(T == bool) || isIntegral!T ||
     (isBitVector!T && (L || (! T.IS4STATE)))) {
    static if(is(T == bool)) {
      alias ubvec!(1) V;
      V v = t;
    }
    else static if(isIntegral!T) {
        alias ubvec!(T.sizeof*8) V;
        V v = t;
      }
      else {
        alias T V;
        alias t v;
      }
    length = len + V.SIZE;
    static if(BIGENDIAN) {
      for (size_t i=0; i != V.SIZE; ++i) {
        this[$-1-i] = v[i];
      }
    }
    else {
      for (size_t i=0; i != V.SIZE; ++i) {
        this[$-1-i] = v[$-1-i];
      }
    }
    return this;
  }

  void pushBack(T)(T t, bool bigEndian=false)
    if(is(T == bool) || isIntegral!T ||
       (isBitVector!T && (L || (! T.IS4STATE)))) {
      if(bigEndian) {
        this.opCatAssign!true(t);
      }
      else {
        this.opCatAssign(t);
      }
    }

  void getFront(T)(out T t, size_t index, bool bigEndian=false)
    if(((! L) && (is(T == bool) || isIntegral!T)) ||
       (isBitVector!T && (T.IS4STATE || (! L)))) {
      static if(is(T == bool))     alias ubvec!(1) V;
      else static if(isIntegral!T) alias ubvec!(T.sizeof*8) V;
        else                       alias T V;
      assert(len-index >= V.SIZE);
      V v;
      if(bigEndian) {
        for (size_t i=0; i != V.SIZE; ++i) {
          v[$-i-1] = this[index+i];
        }
      }
      else {
        for (size_t i=0; i != V.SIZE; ++i) {
          v[i] = this[index+i];
        }
      }
      t = v;
    }

  void getBack(T)(out T t, size_t index, bool bigEndian=false)
    if(((! L) && (is(T == bool) || isIntegral!T)) ||
       (isBitVector!T && (T.IS4STATE || (! L)))) {
      static if(is(T == bool))     alias ubvec!(1) V;
      else static if(isIntegral!T) alias ubvec!(T.sizeof*8) V;
        else                       alias T V;
      getFront(t, index-V.SIZE, bigEndian);
    }

  void popBack(T)(out T t, bool bigEndian=false)
    if(((! L) && (is(T == bool) || isIntegral!T)) ||
       (isBitVector!T && (T.IS4STATE || (! L)))) {
      static if(is(T == bool))     alias ubvec!(1) V;
      else static if(isIntegral!T) alias ubvec!(T.sizeof*8) V;
        else                       alias T V;
      getBack(t, len, bigEndian);
      length = len - V.SIZE;
    }

  static if(! L) {
    void popBackA(T)(ref T t, bool bigEndian=false)
      if(isBitVector!T && T.IS4STATE) {
        alias vec!(T.ISSIGNED, false, T.SIZE) V;
        assert(len >= V.SIZE);
        V v;
        if(bigEndian) {
          for (size_t i=0; i != V.SIZE; ++i) {
            v[i] = this[$-1-i];
          }
        }
        else {
          for (size_t i=0; i != V.SIZE; ++i) {
            v[$-1-i] = this[$-1-i];
          }
        }
        length = len - V.SIZE;
        t.setAval(v);
      }

    void popBackB(T)(ref T t, bool bigEndian=false)
      if(isBitVector!T && T.IS4STATE) {
        alias vec!(T.ISSIGNED, false, T.SIZE) V;
        assert(len >= V.SIZE);
        V v;
        if(bigEndian) {
          for (size_t i=0; i != V.SIZE; ++i) {
            v[i] = this[$-1-i];
          }
        }
        else {
          for (size_t i=0; i != V.SIZE; ++i) {
            v[$-1-i] = this[$-1-i];
          }
        }
        length = len - V.SIZE;
        t.setBval(v);
      }
  }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opCatAssign unittest\n");

  //   static bool[] ba = [1,0,1,0,1];

  //   BitArray a; a.init(ba);
  //   BitArray b;

  //   b = (a ~= true);
  //   assert(a[0] is 1);
  //   assert(a[1] is 0);
  //   assert(a[2] is 1);
  //   assert(a[3] is 0);
  //   assert(a[4] is 1);
  //   assert(a[5] is 1);

  //   assert(b is a);
  // }

  // /***************************************
  //  * ditto
  //  */

  auto opCatAssign(T)(T b)
  if(is(T unused == BitString!_L, bool _L)
     && (L || (! _L))) {
    auto istart = len;
    length = len + b.length;
    for (auto i = istart; i < len; i++) {
      this[i] = b[i - istart];
    }
    return this;
  }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opCatAssign unittest\n");

  //   static bool[] ba = [1,0];
  //   static bool[] bb = [0,1,0];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);
  //   BitArray c;

  //   c = (a ~= b);
  //   assert(a.length is 5);
  //   assert(a[0] is 1);
  //   assert(a[1] is 0);
  //   assert(a[2] is 0);
  //   assert(a[3] is 1);
  //   assert(a[4] is 0);

  //   assert(c is a);
  // }

  // /***************************************
  //  * Support for binary operator ~ for $(D BitArray).
  //  */

  auto opCat(T)(T t) const
  if(is(T == bool) || isIntegral!T ||
     (isBitVector!T && (L || (! T.IS4STATE)))) {
    BitString!L r = this.dup;
    r ~= t;
  }

  /** ditto */
  auto opCat_r(T)(T t) const
  if(is(T == bool) || isIntegral!T ||
     (isBitVector!T && (L || (! T.IS4STATE)))) {
    static if(is(T == bool)) {
      alias ubvec!(1) V;
      V v = t;
    }
    else static if(isIntegral!T) {
        alias ubvec!(T.sizeof*8) V;
        V v = t;
      }
      else {
        alias T V;
        alias t v;
      }
    BitString!L r;

    r.length = len + V.SIZE;
    for (size_t i=0; i!=V.SIZE; ++i) {
      r[i] = v[i];
    }
    for (size_t i = 0; i < len; i++) {
      r[V.SIZE + i] = this[i];
    }
    return r;
  }

  // /** ditto */
  auto opCat(T)(T v) const
  if(is(T unused == BitString!_L, bool _L) && (L || (! _L))) {
    auto r = this.dup;
    r ~= v;
    return r;
  }

  // unittest
  // {
  //   debug(bitarray) printf("BitArray.opCat unittest\n");

  //   static bool[] ba = [1,0];
  //   static bool[] bb = [0,1,0];

  //   BitArray a; a.init(ba);
  //   BitArray b; b.init(bb);
  //   BitArray c;

  //   c = (a ~ b);
  //   assert(c.length is 5);
  //   assert(c[0] is 1);
  //   assert(c[1] is 0);
  //   assert(c[2] is 0);
  //   assert(c[3] is 1);
  //   assert(c[4] is 0);

  //   c = (a ~ true);
  //   assert(c.length is 3);
  //   assert(c[0] is 1);
  //   assert(c[1] is 0);
  //   assert(c[2] is 1);

  //   c = (false ~ a);
  //   assert(c.length is 3);
  //   assert(c[0] is 0);
  //   assert(c[1] is 1);
  //   assert(c[2] is 0);
  // }

  T to(T, size_t RADIX = 2)() if((is(T == string) ||
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
    case 's'     :              // should print as hex when %s
    case 'h'     :              // should print as hex for %h too
    case 'x'     : buff = "0x" ~ toLower(this.to!(char[], 16)); break;
    case 'H'     :              // should print as HEX for %H
    case 'X'     : buff = "0x" ~ this.to!(char[], 16); break;
    case 'o'     : buff = this.to!(char[], 8); break;
    case 'b'     : buff = "0b" ~ this.to!(char[], 2); break;
    default      :
      throw new FormatException("Format specifier not understood: %" ~ f.spec);
    }

    assert(buff.length > 0);

    sink(buff);
  }

  private T toCharString(T, size_t RADIX)() {
    char[] str;
    if(dim > 1) {
      for(size_t i = 0; i != dim-1; ++i) {
        import std.string;
        static if(RADIX == 2)  string fmtstr = "%b";
        static if(RADIX == 8)  string fmtstr = "%o";
        static if(RADIX == 16) string fmtstr = "%X";
        char[] wstr;
        string astr =
          rightJustify(format(fmtstr, aptr[i]),
                       cast(int)((_log2(RADIX) - 1) +
                                 8*size_t.sizeof)/_log2(RADIX), '0');
        static if(L) {
          string zstr =
            rightJustify(format(fmtstr, bptr[i]),
                         cast(int)((_log2(RADIX) - 1) +
                                   8*size_t.sizeof)/_log2(RADIX), '0');
          string xstr =
            rightJustify(format(fmtstr,(cast(size_t)
                                        (aptr[i] & bptr[i]))),
                         cast(int)((_log2(RADIX) - 1) +
                                   8*size_t.sizeof)/_log2(RADIX), '0');
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
    size_t umask = 1;
    umask <<= (len % (size_t.sizeof * 8));
    umask -= 1;
    if(umask is 0) umask = size_t.max;
    auto foo = cast(size_t)(aptr[len-1] & umask);
    static if(RADIX == 16) string fmtstr = "%X";
    static if(RADIX == 8)  string fmtstr = "%o";
    static if(RADIX == 2)  string fmtstr = "%b";

    import std.string;
    string astr =
      rightJustify(format(fmtstr, aptr[dim-1] & umask),
                   cast(int)((_log2(RADIX) - 1) +
                             ((len-1)%(8*size_t.sizeof) + 1))
                   /_log2(RADIX), '0');
    static if(L) {
      string zstr =
        rightJustify(format(fmtstr, bptr[dim-1] & umask),
                     cast(int)((_log2(RADIX) - 1) +
                               ((len-1)%(8*size_t.sizeof) + 1))
                     /_log2(RADIX), '0');
      string xstr =
        rightJustify(format(fmtstr, aptr[dim-1] & bptr[dim-1] & umask),
                     cast(int)((_log2(RADIX) - 1) +
                               ((len-1)%(8*size_t.sizeof) + 1))
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
    if(dim is 1) {
      import std.conv;
      auto val = this.aptr[0];
      string str = to!string(val);
      char[] buff;
      foreach(c; str) buff ~= c;
      return buff;
    }
    else {
      uint[] data =(cast(uint[]) this.toArrA).dup;
      auto predictlength = 20+20*(data.length/2); // just over 19
      char [] buff = new char[predictlength];
      size_t sofar = biguintToDecimal(buff, data.dup);
      return buff[sofar..$];
    }
  }

  private size_t[] toArrA() const {
    size_t arr[];
    arr.length = dim;
    for(size_t i=0; i != dim; ++i) {
      arr[i] = aptr[i];
    }
    return arr;
  }

  static if(L) {
    private size_t[] toArrB() const {
      size_t arr[];
      arr.length = dim;
      for(size_t i=0; i != dim; ++i) {
        arr[i] = bptr[i];
      }
      return arr;
    }
  }

  public void clear() {
    this.length = 0;
  }

  public bool isEmpty() {
    if(len is 0) return true;
    else return false;
  }

  public void fromArray(bool BIGENDIAN=false, T)(T[] arr)
    if((! L) && (isIntegral!T || is(T == bool) || is(T == bit) ||
		 isSomeChar!T)) {
      assert(isEmpty());
      // special case, no array element is going to spread across bstr elements boundry
      foreach(a; arr) {
        auto shft = len & (bitsPerSizeT-1);
        static if(is(T == bool)) length = len + 1;
        else length = len + T.sizeof*8;
        static if(BIGENDIAN) {
	  size_t v = (cast(size_t) (a.reverse)) << shft;
	}
        else {
	  size_t v = (cast(size_t) a) << shft;
	}
        aptr[dim-1] |= v;
      }
      // mask out unused portion of the MSW
      auto shft = (arr.length * BitLength!T) & (bitsPerSizeT - 1);
      if(shft != 0) {
	aptr[dim-1] &= ~(~0L << shft);
      }
    }

  public void fromStr(bool BIGENDIAN=false)(string str) if((! L)) {
    fromArray!BIGENDIAN(str);
  }

  public void toArray(bool BIGENDIAN=false, T)(out T[] arr)
    if((! L) && (isIntegral!T || is(T == bool) || is(T == bit) ||
		 isSomeChar!T)) {
      for (size_t i=0; i*BitLength!T < len; ++i) {
        auto v = aptr[(i*BitLength!T)/bitsPerSizeT];
        static if(BIGENDIAN) arr ~= (cast(T) (v >> ((i*BitLength!T) & (bitsPerSizeT-1)))).reverse;
        else arr ~= cast(T) (v >> ((i*BitLength!T) & (bitsPerSizeT-1)));
      }
    }

  public void toStr(bool BIGENDIAN=false, T)(out string str) if((! L)) {
    char[] cstr;

  }
}


ulong reverse(ulong x) {
  x = (((x & 0xaaaaaaaaaaaaaaaa) >> 1)  | ((x & 0x5555555555555555) << 1));
  x = (((x & 0xcccccccccccccccc) >> 2)  | ((x & 0x3333333333333333) << 2));
  x = (((x & 0xf0f0f0f0f0f0f0f0) >> 4)  | ((x & 0x0f0f0f0f0f0f0f0f) << 4));
  x = (((x & 0xff00ff00ff00ff00) >> 8)  | ((x & 0x00ff00ff00ff00ff) << 8));
  x = (((x & 0xffff0000ffff0000) >> 16) | ((x & 0x0000ffff0000ffff) << 16));
  return((x >> 32) | (x << 32));
}

uint reverse(uint x) {
  x = (((x & 0xaaaaaaaa) >> 1) | ((x & 0x55555555) << 1));
  x = (((x & 0xcccccccc) >> 2) | ((x & 0x33333333) << 2));
  x = (((x & 0xf0f0f0f0) >> 4) | ((x & 0x0f0f0f0f) << 4));
  x = (((x & 0xff00ff00) >> 8) | ((x & 0x00ff00ff) << 8));
  return((x >> 16) | (x << 16));
}

ushort reverse(ushort x) {
  x = cast(ushort)(((x & 0xaaaa) >> 1) | ((x & 0x5555) << 1));
  x = cast(ushort)(((x & 0xcccc) >> 2) | ((x & 0x3333) << 2));
  x = cast(ushort)(((x & 0xf0f0) >> 4) | ((x & 0x0f0f) << 4));
  return cast(ushort)((x >> 8) | (x << 8));
}

ubyte reverse(ubyte x) {
  x = cast(ubyte) (((x & 0xaa) >> 1) | ((x & 0x55) << 1));
  x = cast(ubyte) (((x & 0xcc) >> 2) | ((x & 0x33) << 2));
  return cast(ubyte)((x >> 4) | (x << 4));
}
