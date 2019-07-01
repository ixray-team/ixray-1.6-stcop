
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


#include <assert.h>
#include <stdlib.h>
#include <oac/Log.h>
#include <oac/Logger.h>

using namespace std;

//******************************************************************************
//*** class oacLog::Entry 
//******************************************************************************

oacLog::Entry::Entry(EntryType type,
                     int verbose_level,
                     const char *msg,
                     const char *file,
                     int line,
                     bool test_passed,
                     const char *test_cond,
                     const char *group_name)
{
  Type = type;
  VerboseLevel = verbose_level;
  Msg = (msg == NULL) ? "" : msg;
  File = (file == NULL) ? "" : file;
  Line = line;
  TestPassed = test_passed;
  TestCond = (test_cond == NULL) ? "" : test_cond;
  GroupName = (group_name == NULL) ? "" : group_name;
}

//******************************************************************************
//*** class oacLog
//******************************************************************************

vector<oacLogger *> oacLog::pLoggers;
int oacLog::pGroupLevel = 0;
std::vector<std::string> oacLog::pGroupNameStack;
std::vector<long> oacLog::pTotalTestsStack;
std::vector<long> oacLog::pTotalTestsPassedStack;
std::vector<long> oacLog::pTotalWarningsStack;
std::string oacLog::pCurrentGroupName("");

void oacLog::AddLogger(oacLogger *logger)
{
  assert(logger != NULL);

  Initialize();

  pLoggers.push_back(logger);
}

void oacLog::Initialize(void)
{
  static bool Initialized = false;

  if(Initialized)
    return;

  pTotalTestsStack.push_back(0);
  pTotalTestsPassedStack.push_back(0);
  pTotalWarningsStack.push_back(0);

  Initialized = true;
}

void oacLog::Cleanup(void)
{
  vector<oacLogger *>::iterator Iter = pLoggers.begin();
  for(; Iter != pLoggers.end(); ++Iter)
    delete *Iter;

  pLoggers.clear();
}

void oacLog::Emit(const Entry &entry)
{
  Initialize();

  switch(entry.Type)
  {
    case TYPE_GROUP_START:
      pGroupNameStack.push_back(entry.GroupName);
      pTotalTestsStack.push_back(0);
      pTotalTestsPassedStack.push_back(0);
      pTotalWarningsStack.push_back(0);
      ConstructCurrentGroupName();
      break;

    case TYPE_TEST_COND:
      pTotalTestsStack.back()++;
      if(entry.TestPassed)
        pTotalTestsPassedStack.back()++;
       break;

    case TYPE_WARNING:
      pTotalWarningsStack.back()++;
      break;
  }

  vector<oacLogger *>::iterator Iter = pLoggers.begin();
  for(; Iter != pLoggers.end(); ++Iter)
    (*Iter)->Emit(entry, pGroupLevel);

  long Tmp;
  switch(entry.Type)
  {
    case TYPE_GROUP_START:
      pGroupLevel++;
      break;


    case TYPE_GROUP_END:
      assert(pGroupLevel > 0);
      pGroupLevel--;
      pGroupNameStack.pop_back();

      Tmp = pTotalTestsStack.back();
      pTotalTestsStack.pop_back();
      pTotalTestsStack.back() += Tmp;

      Tmp = pTotalTestsPassedStack.back();
      pTotalTestsPassedStack.pop_back();
      pTotalTestsPassedStack.back() += Tmp;

      Tmp = pTotalWarningsStack.back();
      pTotalWarningsStack.pop_back();
      pTotalWarningsStack.back() += Tmp;

      ConstructCurrentGroupName();
      break;

    case TYPE_ERROR:
      exit(-1);
      break;
  }
}

const std::string &oacLog::CurrentGroupName(void)
{
  Initialize();

  return(pCurrentGroupName);
}

long oacLog::TotalGroupTests(void)
{
  Initialize();
  return(pTotalTestsStack.back());
}

long oacLog::TotalGroupTestsPassed(void)
{
  Initialize();
  return(pTotalTestsPassedStack.back());
}

long oacLog::TotalGroupTestsFailed(void)
{
  return(TotalGroupTests() - TotalGroupTestsPassed());
}

long oacLog::TotalWarnings(void)
{
  Initialize();
  return(pTotalWarningsStack.back());
}

std::string oacLog::TestsSummary(void)
{
  long Total = TotalGroupTests();
  long Passed = TotalGroupTestsPassed();
  long Warnings = TotalWarnings();

  ostringstream Ret;

  assert(Total >= Passed);

  if(Total == Passed)
    Ret << Total << " tests passed";
  else
    Ret << (Total - Passed) << "/" << Total << " tests FAILED"; 

  if(Warnings > 0)
    Ret << ", " << Warnings << " warnings";

  Ret << ".";

   
  return(Ret.str());
}

void oacLog::ConstructCurrentGroupName(void)
{
  Initialize();

  if(pGroupNameStack.size() == 0)
  {
    pCurrentGroupName = "";
    return;
  }

  vector<string>::const_iterator Iter = pGroupNameStack.begin();

  pCurrentGroupName = *Iter;
  Iter++;

  for(; Iter != pGroupNameStack.end(); Iter++)
  {
    pCurrentGroupName += ".";
    pCurrentGroupName += *Iter;
  }
}


