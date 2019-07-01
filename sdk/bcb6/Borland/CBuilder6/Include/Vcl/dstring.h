// DSTRING.H - Support for delphi strings in C++
//            (AnsiString and template<sz> SmallString)
// $Revision:   1.35.1.0.1.0  $
// $Date:   25 Jan 2002 12:54:18  $
//
// Copyright (c) 1997, 2002 Borland Software Corporation

#ifndef DSTRING_H
#define DSTRING_H

#pragma delphiheader begin

#include <sysmac.h>
#include <stdarg.h>


namespace System
{
  class                  TVarRec;
  class RTL_DELPHIRETURN Currency;
  class RTL_DELPHIRETURN WideString;

  /////////////////////////////////////////////////////////////////////////////
  // AnsiString: String class compatible with Delphi's Native 'string' type
  /////////////////////////////////////////////////////////////////////////////
  class RTL_DELPHIRETURN AnsiString
  {
    friend AnsiString __fastcall operator +(const char*, const AnsiString& rhs);
  public:
    // the TStringFloatFormat enum is used by FloatToStrF
    enum TStringFloatFormat
    {sffGeneral, sffExponent, sffFixed, sffNumber, sffCurrency};
    static AnsiString __fastcall StringOfChar(char ch, int count);
    static AnsiString __fastcall LoadStr(int ident);
    static AnsiString __fastcall LoadStr(HINSTANCE hInstance, int ident);
    static AnsiString __fastcall FmtLoadStr(int ident, const TVarRec *args,
                        int size);

    AnsiString& __fastcall       LoadString(HINSTANCE hInstance, int ident);

    // Delphi style 'Format'
    //
    static AnsiString __fastcall Format(const AnsiString& format,
                      const TVarRec *args, int size);

    // C style 'sprintf' (NOTE: Target buffer is the string)
    //
    AnsiString& __cdecl         sprintf(const char* format, ...); // Returns *this
    int         __cdecl          printf(const char* format, ...); // Returns formatted length
    int         __cdecl         vprintf(const char* format, va_list); // Returns formatted length


    // Like above, but appends to the string rather than overwrite
    AnsiString& __cdecl     cat_sprintf(const char* format, ...); // Returns *this
    int         __cdecl     cat_printf(const char* format, ...); // Returns formatted length
    int         __cdecl     cat_vprintf(const char* format, va_list); // Returns formatted length

    static AnsiString __fastcall FormatFloat(const AnsiString& format,
                         const long double& value);
    static AnsiString __fastcall FloatToStrF(long double value,
                         TStringFloatFormat format, int precision, int digits);
    static AnsiString __fastcall IntToHex(int value, int digits);
    static AnsiString __fastcall CurrToStr(Currency value);
    static AnsiString __fastcall CurrToStrF(Currency value,
                        TStringFloatFormat format, int digits);

    // Constructors
    __fastcall AnsiString(): Data(0) {}
    __fastcall AnsiString(const char* src);
    __fastcall AnsiString(const AnsiString& src);
//    __fastcall AnsiString(const char* src, unsigned char len);
    __fastcall AnsiString(const char* src, unsigned int len);
    __fastcall AnsiString(const wchar_t* src);
    __fastcall AnsiString(char src);
    __fastcall AnsiString(short);
    __fastcall AnsiString(unsigned short);
    __fastcall AnsiString(int src);
    __fastcall AnsiString(unsigned int);
    __fastcall AnsiString(long);
    __fastcall AnsiString(unsigned long);
    __fastcall AnsiString(__int64);
    __fastcall AnsiString(unsigned __int64);
    __fastcall AnsiString(double src);
    __fastcall AnsiString(const WideString &src);

    // Destructor
    __fastcall ~AnsiString();

    // Assignments
    AnsiString& __fastcall operator =(const AnsiString& rhs);
    AnsiString& __fastcall operator +=(const AnsiString& rhs);

    // Comparisons
    bool __fastcall operator ==(const AnsiString& rhs) const;
    bool __fastcall operator !=(const AnsiString& rhs) const;
    bool __fastcall operator <(const AnsiString& rhs) const;
    bool __fastcall operator >(const AnsiString& rhs) const;
    bool __fastcall operator <=(const AnsiString& rhs) const;
    bool __fastcall operator >=(const AnsiString& rhs) const;
    int  __fastcall AnsiCompare(const AnsiString& rhs) const;
    int  __fastcall AnsiCompareIC(const AnsiString& rhs) const; //ignorecase

    // Accessing character at specified index

    char __fastcall operator [](const int idx) const
    {
      ThrowIfOutOfRange(idx);   // Should Range-checking be optional to avoid overhead ??
      return Data[idx-1];
    }

#if defined(ANSISTRING_USE_PROXY_FOR_SUBSCRIPT)

    // The use of a proxy class optimizes the case where Unique() must be called
    // when accessing the string via the subscript operator. However, the use of
    // of the proxy class has some drawbacks. First, it breaks code that apply
    // operators to the return value. For example, &MyString[i]. Second, it
    // fails in cases where a implicit conversion was relied upon. For example,
    //       callFuncThatTakesAnObjectWithACharCtr(MyString[i]);
    // In that case, two implicit conversions would be required...
    // The first issue can be remedied by enhancing the proxy class to support
    // all valid operators. The second issue can be lessened but not completely
    // eliminated. Hence, the use of the PROXY class is not the default!
    //
  private:
    class  TCharProxy;
    friend TCharProxy;
    class  TCharProxy
    {
      public:
        TCharProxy(AnsiString& strRef, int index) : m_Ref(strRef), m_Index(index) {}
        TCharProxy& operator=(char c) { m_Ref.Unique(); m_Ref.Data[m_Index-1] = c; return *this; }
        operator char() const         { return m_Ref.Data[m_Index-1]; }

      protected:
        AnsiString&         m_Ref;
        int                 m_Index;
    };

  public:
    TCharProxy __fastcall operator [](const int idx)
    {
      ThrowIfOutOfRange(idx);   // Should Range-checking be optional to avoid overhead ??
      return TCharProxy(*this, idx);
    }

#else

    char& __fastcall operator [](const int idx)
    {
      ThrowIfOutOfRange(idx);   // Should Range-checking be optional to avoid overhead ??
      Unique();                 // Ensure we're not ref-counted
      return Data[idx-1];
    }

#endif

    // Concatenation
    AnsiString __fastcall operator +(const AnsiString& rhs) const;

    // C string operator
    char* __fastcall c_str() const        { return (Data)? Data: "";}

    // Read access to raw Data ptr.  Will be NULL for an empty string.
    const void* __fastcall data() const   { return Data; }

    // Query attributes of string
    int  __fastcall Length()  const;
    bool __fastcall IsEmpty() const { return Data == NULL; }

    // Make string unique (refcnt == 1)
    AnsiString&  __fastcall Unique();

    // Modify string
    AnsiString&  __fastcall Insert(const AnsiString& str, int index);
    AnsiString&  __fastcall Delete(int index, int count);
    AnsiString&  __fastcall SetLength(int newLength);

    int __fastcall Pos(const AnsiString& subStr) const;
    AnsiString   __fastcall LowerCase() const;
    AnsiString   __fastcall UpperCase() const;
    AnsiString   __fastcall Trim() const;
    AnsiString   __fastcall TrimLeft() const;
    AnsiString   __fastcall TrimRight() const;
    AnsiString   __fastcall SubString(int index, int count) const;

    int          __fastcall ToInt() const;
    int          __fastcall ToIntDef(int defaultValue) const;
    double       __fastcall ToDouble() const;

    // Convert to Unicode
    int          __fastcall WideCharBufSize() const;
    wchar_t*     __fastcall WideChar(wchar_t* dest, int destSize) const;

    // MBCS support
    enum TStringMbcsByteType
    {mbSingleByte, mbLeadByte, mbTrailByte};

    TStringMbcsByteType __fastcall ByteType(int index) const;
    bool         __fastcall IsLeadByte(int index) const;
    bool         __fastcall IsTrailByte(int index) const;
    bool         __fastcall IsDelimiter(const AnsiString& delimiters, int index) const;
    bool         __fastcall IsPathDelimiter(int index) const;
    int          __fastcall LastDelimiter(const AnsiString& delimiters) const;
    int          __fastcall AnsiPos(const AnsiString& subStr) const;
    char*        __fastcall AnsiLastChar() const;

  protected:
  void __cdecl ThrowIfOutOfRange(int idx) const;
//    void  ThrowIfOutOfRange(int idx) const;

// protected:
//  void < __cdecl > ThrowIfOutOfRange(int idx) const;

    struct StrRec {
      int allocSiz;
      int refCnt;
      int length;
    };

    const StrRec &GetRec() const;
    StrRec &GetRec();

  private:
    // assert(offsetof(AnsiString, Data) == 0);
    char *Data;
  };

  extern AnsiString __fastcall operator +(const char*, const AnsiString&);

#if defined(VCL_IOSTREAM)
  // see <sysclass.h>
  ostream& operator << (ostream& os, const AnsiString& arg);
  istream& operator >> (istream& is, AnsiString& arg);
#endif

#if !defined(__CODEGUARD__)

  // Codeguard is not very happy about our "reverse indexing" of the
  // Data pointer.  We'll address this by violating the ODR:  when
  // Codeguard compile checks are enabled, these methods will not be
  // inlined.  When building dstring.cpp, __DSTRING_INLINE will be
  // defined to generate out-of-line implementations of these methods.

  #if !defined(__DSTRING_INLINE)
  #define __DSTRING_INLINE inline
  #endif

  __DSTRING_INLINE const AnsiString::StrRec &AnsiString::GetRec() const
  {
    return reinterpret_cast<const StrRec *>(Data)[-1];
  }

  __DSTRING_INLINE AnsiString::StrRec &AnsiString::GetRec()
  {
    return reinterpret_cast<StrRec *>(Data)[-1];
  }

  __DSTRING_INLINE int __fastcall AnsiString::Length() const
  {
    return (Data)? GetRec().length : 0;
  }

#undef __DSTRING_INLINE
#endif // !defined(__CODEGUARD__)

  /////////////////////////////////////////////////////////////////////////////
  // SmallStringBase
  /////////////////////////////////////////////////////////////////////////////
  template <unsigned char sz> class SmallStringBase
  {
  protected:
    unsigned char Len;
    char Data[sz];
  };


  /////////////////////////////////////////////////////////////////////////////
  // SmallString
  /////////////////////////////////////////////////////////////////////////////
  template <unsigned char sz> class SmallString : SmallStringBase<sz>
  {

  public:
    __fastcall SmallString() { Len = 0; }
    __fastcall SmallString(const SmallString& src);
    __fastcall SmallString(const char* src);

    __fastcall SmallString(const AnsiString& src)
    {
      long len = src.Length();
      Len = (unsigned char)((len > sz)? sz: len);
      strncpy(Data, src.c_str(), Len);
    }

    char& __fastcall operator [](const unsigned char idx)
    {return Data[idx-1];}

    SmallString& __fastcall operator =(const SmallString& rhs);

    __fastcall operator AnsiString() const;
  };

  // used when SmallStrings are in unions (can't have a ctor)
  // must cast DummySmallString to SmallString to do anything useful

  template<unsigned char sz> __fastcall
  SmallString<sz>::SmallString(const char* src)
  {
    long len = strlen(src);
    Len = (unsigned char)((len > sz)? sz: len);
    strncpy(Data, src, Len);
  }

  template<unsigned char sz> __fastcall
  SmallString<sz>::SmallString(const SmallString& src)
  {
    Len = src.Len;
    for (int i = 0; i < Len; i++)
      Data[i] = src.Data[i];
  }

  template<unsigned char sz> SmallString<sz>& __fastcall
  SmallString<sz>::operator =(const SmallString& rhs)
  {
    if (this != &rhs)
    {
      Len = rhs.Len;
      for (int i = 0; i < Len; i++)
        Data[i] = rhs.Data[i];
    }
    return *this;
  }

  template<unsigned char sz>
  inline __fastcall SmallString<sz>::operator AnsiString() const
  {
    return AnsiString(Data, Len);
  }

#if defined(VCL_IOSTREAM)
  // see sysclass.h
  template<unsigned char sz>
  ostream& operator <<(ostream& os, const SmallString<sz>& arg);

  template<unsigned char sz>
  istream& operator >>(istream& is, SmallString<sz>& arg);
#endif

}
using namespace System;

// The following is provided for backward compatibility.
// Otherwise, the new IntToStr(__int64) causes ambiguity for old code
// that used other integral types.
//
namespace Sysutils
{
  extern PACKAGE AnsiString __fastcall IntToStr(int Value)/* overload */;
  extern PACKAGE AnsiString __fastcall IntToStr(__int64 Value)/* overload */;
}

#pragma option push -w-inl

inline AnsiString __fastcall IntToStr(bool value)
{
  return Sysutils::IntToStr(int(value));
}
inline AnsiString __fastcall IntToStr(unsigned int value)
{
  return Sysutils::IntToStr(int(value));
}
inline AnsiString __fastcall IntToStr(long value)
{
  return Sysutils::IntToStr(int(value));
}
inline AnsiString __fastcall IntToStr(unsigned long value)
{
  return Sysutils::IntToStr(int(value));
}

#pragma option pop

#pragma delphiheader end.

#endif  // DSTRING_H


