
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

#ifndef _oac_Context_h
#define _oac_Context_h

#include <vector>
#include <map>
#include <string>

#include <OpenAutomate.h>
#include <OpenAutomate_Internal.h>

#include <oaRPC.h>

class oacTestBase;


//******************************************************************************
//*** class oacContext
//******************************************************************************
class oacContext
{
public:

  class ClientApp
  {
    public:
      virtual ~ClientApp();

      virtual bool Run(void) = 0;
      virtual long WaitForExit(void) = 0;
  };


  oacContext(oaRPCServer *server,
             ClientApp *client_app);

  ~oacContext();

  struct OptionValue
  {
    OptionValue();
    OptionValue(const OptionValue &val);
    ~OptionValue();

    void Set(const oaChar *name,
             oaOptionDataType value_type,
             const oaValue *value);

    oaChar *Name;
    oaOptionDataType Type;
    oaValue Value;
  };
    
  // Makes this context the current one for all calls to the static OA 
  // functions, and runs the client.  Returns false if there's a failure
  bool Run(void);

  // Adds a test module to the list
  void AddTest(oacTestBase *test);
 
  // Returns the function table containing pointers to static OA functions.
  // This table should be returned by the Init() function of an OA plugin.
  static const oaiFunctionTable *GetOAFuncTable(void);

  //****************************************************************************
  //*** Methods to be used from oaTestBase::Run()
  //****************************************************************************

  // Sends an OA command to the application.  All subsequent calls made by
  // the application will be passed to the matching callbacks in the current
  // test until GetNextCommand() is called.  
  void IssueCommand(oaCommand *command);

  // Sends the OA_CMD_EXIT command to the application, waits for it to exit,
  // and restarts it.  This is useful for testing that requires persistence
  // on the application's part between runs.  
  void IssueExitAndRestart(void);

  // Returns an array of all the available options.  If the command 
  // OA_CMD_GET_ALL_OPTIONS was never issued or force_cmd = true, it will 
  // issue it  to the and cache the results for future calls to GetAllOptions().
  const std::vector<oaNamedOption> &GetAllOptions(bool force_cmd = false);
  
  // Returns a map where the key is the key is the option name, and value
  // is a vector of oaNamedOptions.  This is useful when dealing with enum
  // options, since there will be multiple oaNamedOption objects for each
  // enum option.
  typedef std::vector<oaNamedOption> OptionVec;
  const std::map<std::string, OptionVec *> &
	GetAllOptionsMap(bool force_cmd = false);

  // Returns the specified option by name.  Semantics are the same as 
  // GetAllOptions().  Returns NULL if no option with that name exists.
  const OptionVec *GetOption(const oaChar *name, bool force_cmd = false);
  const OptionVec *GetOption(const std::string &name, bool force_cmd = false);

  // Similar to GetAllOptions(), but for OA_CMD_GET_CURRENT_OPTIONS
  const std::vector<OptionValue *> &GetCurrentOptions(bool force_cmd = false);

  const std::map<std::string, OptionValue *> &
    GetCurrentOptionsMap(bool force_cmd = false);

  // Returns the specified option by name.  Semantics are the same as 
  // GetCurrentOptions. Returns NULL if no option with that name exists.
  const OptionValue *GetOptionValue(const oaChar *name, 
                                    bool force_cmd = false);
  const OptionValue *GetOptionValue(const std::string &name, 
                                    bool force_cmd = false);

  // Similar to GetAllOptions(), but for OA_CMD_GET_BENCHMARKS
  const std::vector<std::string> &GetBenchmarks(bool force_cmd = false);

  // returns a random value from 0.0 to 1.0.  Tests should use this random
  // number generator, to ensure determinism in the test results.
  double Rand(void);

  void SRand(unsigned long seed);


private:

  enum StateType
  {
    STATE_INVALID = 0,
    STATE_INIT,
    STATE_RUNNING_TEST,
    STATE_ISSUING_COMMAND,
    STATE_FINISHED,
  };

  enum CallbackType
  {
#define OAC_DECLARE_CALLBACK(func_name, func_id) \
    OAC_FUNC_##func_name = func_id,
#include <oac/OACallbackDefs.h>
  };

  void RunTests(void);

  StateType State() const;
  void PushState(StateType state);
  void PopState(void);
  void ChangeState(StateType state);

  std::vector<oacTestBase *> pTests;
  std::vector<oacTestBase *>::iterator pCurrentTest;

  bool pHaveBenchmarks;
  std::vector<std::string> pBenchmarks;
  void ClearBenchmarks(void);

  bool pHaveAllOptions;
  std::vector<oaNamedOption> pAllOptions;
  std::map<std::string, OptionVec *> pAllOptionsMap;
  void ClearAllOptions(void);

  bool pHaveCurrentOptions;
  std::vector<OptionValue *> pCurrentOptions;
  std::map<std::string, OptionValue *> pCurrentOptionsMap;
  void ClearCurrentOptions(void);


  std::vector<StateType> pState;
  oaCommandType pCurrentCommand;

  oaRPCServer *pServer;
  ClientApp *pClientApp;

  unsigned long pRandSeed;

  static oacContext *pCurrent;
  static oaiFunctionTable pFuncTable;
  static oaInt pCallCount;

  //****************************************************************************
  //*** OA callbacks
  //****************************************************************************

  static oaCommandType GetNextCommand(oaCommand *command);
  static oaNamedOption* GetNextOption(void);
  static void AddOption(const oaNamedOption *option);
  static void AddOptionValue(const oaChar *name,
                             oaOptionDataType value_type,
                             const oaValue *value);
  static void AddBenchmark(const oaChar *benchmark_name);
  static oaBool SendSignal(oaSignalType signal, void *param);
  static void StartBenchmark(void);
  static void DisplayFrame(oaFloat t);
  static void EndBenchmark(void);
  static void AddResultValue(const oaChar *name, 
                             oaOptionDataType value_type,
                             const oaValue *value);
  static void AddFrameValue(const oaChar *name, 
                             oaOptionDataType value_type,
                             const oaValue *value);
};

#endif
