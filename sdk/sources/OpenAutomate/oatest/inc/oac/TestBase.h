
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

#ifndef _oac_TestBase_h
#define _oac_TestBase_h

#include <stdio.h>
#include <vector>
#include <string>
#include <sstream>
#include <OpenAutomate.h>
#include <oac/Context.h>

#include <oac/Log.h>


#define OAC_TEST_CONSTRUCTOR_NAME(name) oacConstructTestModule_##name

#define OAC_TEST_DEFINE_CONSTRUCTOR(name) \
  oacTestBase * OAC_TEST_CONSTRUCTOR_NAME(name) (void) \
  { \
    return(new oacTestModule_##name ()); \
  } 

#define OAC_TEST_DECLARE_CLASS_HEAD(name, \
                                    description, \
                                    planned_tests, \
                                    fully_automated) \
  class oacTestModule_##name : public oacTestBase \
  { \
  public: \
    oacTestModule_##name () { } \
    virtual ~oacTestModule_##name() { } \
    virtual Info *GetInfo(void) \
    { \
      Info *Ret = new Info(#name, \
                           description, \
                           planned_tests,\
                           fully_automated); \
      return(Ret); \
    } 
  
#define OAC_TEST_DECLARE_CLASS_TAIL(name) \
    }; \
    \
    OAC_TEST_DEFINE_CONSTRUCTOR(name) 
  

#define OAC_SIMPLE_TEST_HEAD(name, \
                             description, \
                             planned_tests, \
                             fully_automated) \
  OAC_TEST_DECLARE_CLASS_HEAD(name, \
                              description, \
                              planned_tests, \
                              fully_automated) \
  virtual void Run(void); \
  OAC_TEST_DECLARE_CLASS_TAIL(name)\
  \
  void oacTestModule_##name ::Run(void) \
  {

#define OAC_SIMPLE_TEST_TAIL }
                         
//******************************************************************************
//*** class oacTestBase
//******************************************************************************
class oacTestBase
{
public:

  typedef oacContext::OptionValue OptionValue;

  //****************************************************************************
  //*** class oacTestBase::Info
  //****************************************************************************
  struct Info
  {
    Info(const oaChar *name,
         const oaChar *description,
         int planned_tests,
         bool fully_automated);

    ~Info();

    oaChar *Name;
    oaChar *Description;
    int NumPlannedTests;
    bool FullyAutomated;
  };
  
   
  oacTestBase();
  virtual ~oacTestBase();

  // This is main() for the test.  
  virtual void Run(void) = 0;

  // Returns all relevant info regarding the test.  The returned object must
  // be deleted by the caller.
  virtual Info *GetInfo(void) = 0;

 
  // This will generally be called directly by oacContext when the the test
  // object is added to it.
  void SetContext(oacContext *context);


  //***************************************************************************
  //*** OA Callbacks
  //***************************************************************************

  virtual oaNamedOption* GetNextOption(void);
  virtual void AddOption(const oaNamedOption *option);
  virtual void AddOptionValue(const oaChar *name,
                              oaOptionDataType value_type,
                              const oaValue *value);
  virtual void AddBenchmark(const oaChar *benchmark_name);
  virtual oaBool SendSignal(oaSignalType signal, void *param);
  virtual void StartBenchmark(void);
  virtual void DisplayFrame(oaFloat t);
  virtual void EndBenchmark(void);
  virtual void AddResultValue(const oaChar *name, 
                              oaOptionDataType value_type,
                              const oaValue *value);
  virtual void AddFrameValue(const oaChar *name, 
                             oaOptionDataType value_type,
                             const oaValue *value);


protected:

  oacContext *Context(void) { return(pContext); }

  double Rand(void) { return(Context()->Rand()); }

private:

  // Should be used from within Run()

  oacContext *pContext;
};

#endif
