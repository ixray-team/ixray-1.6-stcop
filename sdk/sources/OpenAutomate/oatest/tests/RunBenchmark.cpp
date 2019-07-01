
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


#include <oac/TestBase.h>
#include <vector>
#include <map>

using namespace std;


OAC_TEST_DECLARE_CLASS_HEAD(RunBenchmark, 
                            "Basic sanity tests for OA_CMD_RUN_BENCHMARK",
                            1, 
                            true) 

  enum CallType
  {
    CALL_TYPE_START_BENCHMARK,
    CALL_TYPE_END_BENCHMARK,
    CALL_TYPE_DISPLAY_FRAME,
    CALL_TYPE_ADD_RESULT_VALUE,
    CALL_TYPE_ADD_FRAME_VALUE,
  };

  struct Call
  {
    Call(CallType type,
         oaFloat display_frame_time = -1.0)
    {
      Type = type;
      DisplayFrameTime = display_frame_time;
      ValueType = OA_TYPE_INVALID;
    }

    Call(CallType type,
         oaOptionDataType value_type,
         const oaValue *value)
    {
      Type = type;
      ValueType = value_type;
      Value = *value;
      if(ValueType == OA_TYPE_STRING || ValueType == OA_TYPE_ENUM)
      {
        Value.String = NULL;
        Value.Enum = NULL;
        StringValue = value->String;
      }
    }
         
    CallType Type;
    oaFloat DisplayFrameTime;
    oaOptionDataType ValueType;
    oaValue Value;
    string StringValue;
  };


  vector<Call> pCalls; 
  map<string,int> pFrameValueMap;
  map<string,int> pResultValueMap;
  string pCurBenchmarkName;
  bool pTimeIsAlwaysGreater;
  oaFloat pLastTime;

  unsigned long pNumStartBenchmarkCalls;
  unsigned long pNumEndBenchmarkCalls;
  unsigned long pNumDisplayFrameCalls;
  unsigned long pNumAddResultValueCalls;
  unsigned long pNumAddFrameValueCalls;


  virtual void Run(void)
  {
    const vector<string> &Benchmarks = Context()->GetBenchmarks();

    vector<string>::const_iterator BMName = Benchmarks.begin();
    for(; BMName !=  Benchmarks.end(); BMName++)
    {
      Clear();

      pCurBenchmarkName = *BMName;

      oaCommand Command;
      oaInitCommand(&Command);
      Command.Type = OA_CMD_RUN_BENCHMARK;
      Command.BenchmarkName = BMName->data();

      OAC_MSG("running benchmark: " << *BMName)

      Context()->IssueCommand(&Command);

      OAC_COND_WARN(pTimeIsAlwaysGreater,
        "oaDisplayFrame() was called with a 't' value that was <= to "
        "a 't' value from a previous call to oaDisplayFrame().  This may "
        " be a bug.");

      OAC_MSG("done running benchmark: " << *BMName)

      CheckSanityOFCalls();
    }

    Clear();
  }

  void Clear(void)
  {
    pCalls.clear();
    pFrameValueMap.clear();
    pResultValueMap.clear();
    pNumStartBenchmarkCalls = 0;
    pNumEndBenchmarkCalls = 0;
    pNumDisplayFrameCalls = 0;
    pNumAddResultValueCalls = 0;
    pNumAddFrameValueCalls = 0;
    pTimeIsAlwaysGreater = true;
  }

  void CheckSanityOFCalls(void )
  {
    OAC_TEST_MSG(pNumStartBenchmarkCalls == 1,
      "oaStartBenchmark() should be called exactly once for each benchmark "
      "run.  The benchmark named '" << pCurBenchmarkName << "' called "
      "oaStartBenchmark() " << pNumStartBenchmarkCalls << " times.")

    OAC_TEST_MSG(pNumEndBenchmarkCalls == 1,
      "oaEndBenchmark() should be called exactly once for each benchmark "
      "run.  The benchmark named '" << pCurBenchmarkName << "' called "
      "oaEndBenchmark() " << pNumEndBenchmarkCalls << " times.")
      
    OAC_COND_WARN(pNumDisplayFrameCalls > 0,
      "oaDisplayFrame() was not called.  This may be a result of a bug.");
  }

  virtual void StartBenchmark(void)
  {
    pCalls.push_back(Call(CALL_TYPE_START_BENCHMARK));
    pNumStartBenchmarkCalls++;
  }

  virtual void DisplayFrame(oaFloat t)
  {
    if(pNumDisplayFrameCalls > 0 && t <= pLastTime)
      pTimeIsAlwaysGreater = false;

    pCalls.push_back(Call(CALL_TYPE_DISPLAY_FRAME, t));
    pNumDisplayFrameCalls++;
    pLastTime = t;
  }

  virtual void EndBenchmark(void)
  {
    pCalls.push_back(Call(CALL_TYPE_END_BENCHMARK));
    pNumEndBenchmarkCalls++;
  }

  virtual void AddResultValue(const oaChar *name, 
                              oaOptionDataType value_type,
                              const oaValue *value)
  {
    pCalls.push_back(Call(CALL_TYPE_ADD_RESULT_VALUE, value_type, value));
    pNumAddResultValueCalls++;
  }

  virtual void AddFrameValue(const oaChar *name, 
                             oaOptionDataType value_type,
                             const oaValue *value)
  {
    pCalls.push_back(Call(CALL_TYPE_ADD_FRAME_VALUE, value_type, value));
    pNumAddFrameValueCalls++;
  }

OAC_TEST_DECLARE_CLASS_TAIL(RunBenchmark)
