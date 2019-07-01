
/*******************************************************************************
 * Copyright 1993-2008 NVIDIA Corporation.  All rights reserved.
 *
 * NOTICE TO USER:   
 *
 * This source code is subject to NVIDIA ownership rights under U.S. and 
 * international Copyright laws.  
 * 
 * This software and the information contained herein is PROPRIETARY and 
 * CONFIDENTIAL to NVIDIA and is being provided under the terms and conditions 
 * of a Non-Disclosure Agreement.  Any reproduction or disclosure to any third 
 * party without the express written consent of NVIDIA is prohibited.     
 *
 * NVIDIA MAKES NO REPRESENTATION ABOUT THE SUITABILITY OF THIS SOURCE CODE FOR 
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY OF 
 * ANY KIND.  NVIDIA DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOURCE CODE, 
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY, NONINFRINGEMENT, AND 
 * FITNESS FOR A PARTICULAR PURPOSE.   IN NO EVENT SHALL NVIDIA BE LIABLE FOR 
 * ANY SPECIAL, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,  WHETHER IN AN 
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,  ARISING OUT OF OR 
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOURCE CODE.  
 *
 * U.S. Government End Users.   This source code is a "commercial item" as that 
 * term is defined at  48 C.F.R. 2.101 (OCT 1995), consisting  of "commercial 
 * computer  software"  and "commercial computer software documentation" as such 
 * terms are  used in 48 C.F.R. 12.212 (SEPT 1995) and is provided to the U.S. 
 * Government only as a commercial end item.  Consistent with 48 C.F.R.12.212 
 * and 48 C.F.R. 227.7202-1 through 227.7202-4 (JUNE 1995), all U.S. Government 
 * End Users acquire the source code with only those rights set forth herein. 
 *
 ******************************************************************************/


#include <sstream>
#include <oac/TextLogger.h>
#include <assert.h>

#ifdef WIN32
# pragma warning(disable:4018)
# pragma warning(disable:4996)
#endif

using namespace std;

oacTextLogger::oacTextLogger(int max_columns)
{
  pMaxColumns = max_columns;
}

oacTextLogger::~oacTextLogger()
{
}

static const char *Basename(const char *cmd)
{
  int Len = (int)strlen(cmd);

  if(Len <= 0)
    return(cmd);

  for(int i=Len-1; i > 0; --i)
    if(cmd[i] == '/' || cmd [i] == '\\')
    {
      if(i + 1 < Len)
        return(&cmd[i+1]);
      break;
    }

  return(cmd);
}

static string FormatFileLine(const string &file, int line)
{
  if(file.size() < 1)
    return(string(""));

  ostringstream Str;
  Str << Basename(file.data()) << ", " << line << ": ";
  return(Str.str());
}

static bool IsWhiteSpace(char c)
{
  return(c == ' ' || c == '\t');
}

static char *FindNewLinePoint(char *str, size_t last_index)
{
  assert(last_index > 0);
  char *Ptr = str + last_index;

  for(int i=0; i < last_index; ++i)
    if(str[i] == '\n')
      return(str + i + 1);

  Ptr = str + last_index;
  for(; Ptr > str && !IsWhiteSpace(*Ptr); --Ptr);

  if(IsWhiteSpace(*Ptr))
  {
    for(; Ptr - 1 > str && IsWhiteSpace(Ptr[-1]); --Ptr);

    if(((Ptr - str) * 1000) / last_index >= 750)
      return(Ptr);
  }

  return(str + last_index);
}

static ostringstream &FormatParagraph(const string &src, 
                                      const string &indent, 
                                      ostringstream &dst,
                                      int max_columns) 
{
  assert(max_columns > indent.size());

  char *TmpSrc = strdup(src.data());

  size_t SrcLen = src.size();

  bool FirstFlag = true;

  char *Ptr = TmpSrc;

  while(SrcLen > 0)	
  {
    while(SrcLen > 0 && IsWhiteSpace(*Ptr))
    {
      Ptr++;
      SrcLen--;
    }

    if(SrcLen == 0) 
      break;

    size_t IndentSize = indent.size();

    if(FirstFlag)
      IndentSize = dst.str().size();
    else
      dst << indent;

    if(IndentSize + SrcLen <= max_columns)
    {
      dst << Ptr;
      break;
    }
 
    char *NewLinePoint = FindNewLinePoint(Ptr, max_columns - IndentSize);
    char Tmp = *NewLinePoint;
    *NewLinePoint = 0;
    
    size_t PtrLen = strlen(Ptr); 

    dst << Ptr;

    assert(PtrLen > 0);
    if(Ptr[PtrLen - 1] != '\n')
     dst << "\n";

    *NewLinePoint = Tmp;

    SrcLen -= (NewLinePoint - Ptr);
    Ptr = NewLinePoint;

    FirstFlag = false;
  }


  free(TmpSrc);

  return(dst);
}

void oacTextLogger::Emit(const oacLog::Entry &entry, int group_level)
{
  ostringstream Str;
  string IndentStr(".");
  int i;

#if 0
char Buf[1024];
sprintf(Buf, "%d: ", group_level);
IndentStr = Buf;
#endif
  for(i=0; i < group_level; ++i)
    IndentStr +=  "..";

  IndentStr += " ";

  Str << IndentStr;

  switch(entry.Type)
  {
    case oacLog::TYPE_WARNING:
      Str << "Warning: " << entry.Msg;
      IndentStr += "         ";
      break;

    case oacLog::TYPE_ERROR:
      Str << "ERROR: " << FormatFileLine(entry.File, entry.Line);
      IndentStr += "       ";
      break;

    case oacLog::TYPE_TEST_COND:
      if(entry.TestPassed)
        return;

      Str << "FAILED: ";
      IndentStr += "  !! ";
      Str << FormatFileLine(entry.File, entry.Line)
          << entry.TestCond << "\n" << entry.Msg;
      break;

    case oacLog::TYPE_MESSAGE:
      Str << entry.Msg;
      IndentStr += "  ";
      break;
      
    case oacLog::TYPE_GROUP_START:
      Str << "Test group: \"" << oacLog::CurrentGroupName() << "\"";
      IndentStr += "  ";
      break;

    case oacLog::TYPE_GROUP_END:
      Str << oacLog::TestsSummary();
      break;

    default:
      assert("Shouldn't be here!!!" == NULL);
  }

  ostringstream FormattedStr;
  FormatParagraph(Str.str(), 
                  IndentStr, 
                  FormattedStr, 
                  pMaxColumns);

  EmitLine(FormattedStr.str().data());

  switch(entry.Type)
  {
    case oacLog::TYPE_GROUP_START:
      break;
  }
}



