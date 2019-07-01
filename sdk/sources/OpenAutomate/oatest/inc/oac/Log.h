
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

#ifndef _oac_Log_h
#define _oac_Log_h

#include <string>
#include <vector>
#include <sstream>

// Should be used within ExecuteTest() and the callbacks.  Works like assert.
#define OAC_TEST(cond) \
  { \
    bool CondResult = (cond); \
    oacLog::Emit(oacLog::Entry(oacLog::TYPE_TEST_COND, \
                 0, \
                 NULL, \
                 __FILE__, \
                 __LINE__, \
                 CondResult, \
                 #cond, \
                 NULL)); \
  }
    
#define OAC_TEST_MSG(cond, msg) \
   { \
    ostringstream Msg; \
    Msg << msg; \
    bool CondResult = (cond); \
    oacLog::Emit(oacLog::Entry(oacLog::TYPE_TEST_COND, \
                 0, \
                 Msg.str().data(), \
                 __FILE__, \
                 __LINE__, \
                 CondResult, \
                 #cond, \
                 NULL)); \
   }

#define OAC_MSG(msg) \
   { \
    ostringstream Msg; \
    Msg << msg; \
    oacLog::Emit(oacLog::Entry(oacLog::TYPE_MESSAGE, \
                 0, \
                 Msg.str().data(), \
                 __FILE__, \
                 __LINE__)); \
   }

#define OAC_WARN(msg) \
   { \
    ostringstream Msg; \
    Msg << msg; \
    oacLog::Emit(oacLog::Entry(oacLog::TYPE_WARNING, \
                 0, \
                 Msg.str().data(), \
                 __FILE__, \
                 __LINE__));\
   }

#define OAC_COND_WARN(cond, msg) \
   { \
    ostringstream Msg; \
    Msg << msg; \
    bool CondResult = (cond); \
    if(!CondResult) \
      oacLog::Emit(oacLog::Entry(oacLog::TYPE_WARNING, \
                   0, \
                   Msg.str().data(), \
                   __FILE__, \
                   __LINE__, \
                   CondResult, \
                   #cond, \
                   NULL)); \
   }

#define OAC_ERROR(msg) \
   { \
    ostringstream Msg; \
    Msg << msg; \
    oacLog::Emit(oacLog::Entry(oacLog::TYPE_ERROR, \
                 0, \
                 Msg.str().data(), \
                 __FILE__, \
                 __LINE__));\
   }

#define OAC_START_TEST_GROUP(name)  \
  oacLog::Emit(oacLog::Entry(oacLog::TYPE_GROUP_START, \
               0, \
               "", \
               __FILE__, \
               __LINE__, \
               false, \
               NULL, \
               name)); 

#define OAC_END_TEST_GROUP  \
  oacLog::Emit(oacLog::Entry(oacLog::TYPE_GROUP_END, \
               0, \
               "", \
               __FILE__, \
               __LINE__, \
               false, \
               NULL, \
               NULL)); 


class oacLogger;

class oacLog
{
public:
  
  enum EntryType
  {
    // Non-fatal warnings 
    TYPE_WARNING = 1,

    // Fatal errors 
    TYPE_ERROR,

    // Test condition passed or failed 
    TYPE_TEST_COND,

    // Log message 
    TYPE_MESSAGE,

    // Start a new group of tests/messages/...  with the name group_name.
    // Groups may be hierarchical 
    TYPE_GROUP_START,

    // Closes the group 
    TYPE_GROUP_END,
  };

  struct Entry
  {
    Entry(EntryType type,
          int verbose_level,
          const char *msg,
          const char *file,
          int line,
          bool test_passed = false,
          const char *test_cond = NULL,
          const char *group_name = NULL);
          
    EntryType Type;
    int VerboseLevel;
    std::string Msg;
    bool TestPassed;
    std::string TestCond;
    std::string GroupName;
    std::string File;
    int Line;
  };

  static void AddLogger(oacLogger *logger);
  static void Cleanup(void);


  // Low-level calls that usually shouldn't be used directly.  Instead use the 
  // OAC_XXX macros above.
  static void Emit(const Entry &entry);

  static const std::string &CurrentGroupName(void);

  static long TotalGroupTests(void);
  static long TotalGroupTestsPassed(void);
  static long TotalGroupTestsFailed(void);
  static long TotalWarnings(void);
  static std::string TestsSummary(void);

private:

  static void Initialize(void);

  static void ConstructCurrentGroupName(void);
  
  static std::vector<oacLogger *> pLoggers;
  static std::vector<std::string> pGroupNameStack;
  static std::vector<long> pTotalTestsStack;
  static std::vector<long> pTotalTestsPassedStack;
  static std::vector<long> pTotalWarningsStack;
  static std::string pCurrentGroupName;
  static int pGroupLevel;
};

#endif
