
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
#include <stdio.h>
#include <string.h>
#include <oac/Context.h>
#include <oac/TestBase.h>
#include <oac/OAUtils.h>
#include <oac/Log.h>

using namespace std;

//******************************************************************************
//*** oacContext static varables
//******************************************************************************

oacContext *oacContext::pCurrent = NULL;
oaiFunctionTable oacContext::pFuncTable;
oaInt oacContext::pCallCount = 0;

//******************************************************************************
//*** oacContext::OptionValue methods
//******************************************************************************

oacContext::OptionValue::OptionValue()
{
  Name = NULL;
  Type = OA_TYPE_INVALID;
  memset(&Value, 0, sizeof(Value));
}

oacContext::OptionValue::OptionValue(const OptionValue &val)
{
  Name = NULL;
  Type = OA_TYPE_INVALID;
  memset(&Value, 0, sizeof(Value));
  Set(val.Name, val.Type, &val.Value);
}

void oacContext::OptionValue::Set(const oaChar *name,
                                  oaOptionDataType value_type,
                                  const oaValue *value)
{
  if(Name != NULL)
    free(Name);

  switch(Type)
  {
    case OA_TYPE_STRING:
      if(Value.String)
        free(Value.String);
      break;

    case OA_TYPE_ENUM:
      if(Value.Enum)
        free(Value.Enum);
      break;
  }

  Name = (name == NULL) ? NULL : strdup(name);
  Type = value_type;

  if(value != NULL)
  {
    switch(Type)
    {
      case OA_TYPE_STRING:
        if(value->String != NULL)
          Value.String = strdup(value->String);
        break;

      case OA_TYPE_ENUM:
        if(value->Enum != NULL)
          Value.Enum = strdup(value->Enum);
        break;

      default:
        Value = *value;
    }
  }
  else
    memset(&Value, 0, sizeof(Value));
}
             
oacContext::OptionValue::~OptionValue()
{
  Set(NULL, OA_TYPE_INVALID, NULL);
}


//******************************************************************************
//*** oacContext::ClientApp methods
//******************************************************************************

oacContext::ClientApp::~ClientApp()
{
}

//******************************************************************************
//*** oacContext methods
//******************************************************************************

oacContext::oacContext(oaRPCServer *server,
                       ClientApp *client_app)
{
  assert(server != NULL);

  pServer = server;
  pClientApp = client_app;

  pRandSeed = 69;

  pHaveBenchmarks = false;
  pHaveAllOptions = false;
  pHaveCurrentOptions = false;


  pState.push_back(STATE_INIT);

  oaiInitFuncTable(&pFuncTable);
  pFuncTable.GetNextCommand = GetNextCommand;
  pFuncTable.GetNextOption = GetNextOption;
  pFuncTable.AddOption = AddOption;
  pFuncTable.AddOptionValue = AddOptionValue;
  pFuncTable.AddBenchmark = AddBenchmark;
  pFuncTable.SendSignal = SendSignal;
  pFuncTable.StartBenchmark = StartBenchmark;
  pFuncTable.EndBenchmark = EndBenchmark;
  pFuncTable.DisplayFrame = DisplayFrame;
  pFuncTable.AddResultValue = AddResultValue;
  pFuncTable.AddFrameValue = AddFrameValue;
}

oacContext::~oacContext()
{
  vector<oacTestBase *>::iterator Iter = pTests.begin();
  for(; Iter != pTests.end(); Iter++)
    delete *Iter;

  ClearBenchmarks();
  ClearAllOptions();
  ClearCurrentOptions();
}

bool oacContext::Run(void)
{
  assert(pClientApp != NULL);

  pCurrent = this;

  bool Ret = pClientApp->Run();

  return(Ret);
}

void oacContext::AddTest(oacTestBase *test)
{
  assert(test != NULL);
  
  // All tests must be added before the tests are run
  assert(State() == STATE_INIT); 

  test->SetContext(this);

  pTests.push_back(test);
}

const oaiFunctionTable *oacContext::GetOAFuncTable(void)
{
  return(&pFuncTable);
}

void oacContext::IssueCommand(oaCommand *command)
{
  assert(command != NULL);

  PushState(STATE_ISSUING_COMMAND);
  pCurrentCommand = command->Type;

  switch(pCurrentCommand)
  {
    case OA_CMD_GET_BENCHMARKS:
      ClearBenchmarks();
      pHaveBenchmarks = true;
      break;

    case OA_CMD_GET_ALL_OPTIONS:
      ClearAllOptions();
      pHaveAllOptions = true;
      break;
      
    case OA_CMD_GET_CURRENT_OPTIONS:
      ClearCurrentOptions();
      pHaveCurrentOptions = true;
      break;
  }

  oaBool Ret = oaRPCServerNestCommand(pServer, command);
  assert(Ret == OA_TRUE);

  PopState();
}
static oaBool PostExitFunc(void *data)
{
  assert(data != NULL);

  oacContext::ClientApp *ClientApp = (oacContext::ClientApp *)data;

  long ExitCode = ClientApp->WaitForExit();

  if(ExitCode != 0)
    OAC_ERROR("Client exited with non-zero exit code " << ExitCode << ".")

  if(!ClientApp->Run())
    OAC_ERROR("Restarting client application failed.")

  return(OA_TRUE);
}

void oacContext::IssueExitAndRestart(void)
{
  oaCommand Command;

  oaInitCommand(&Command);
  Command.Type = OA_CMD_EXIT;

  
  if(oaRPCServerSendExitCommand(pServer, PostExitFunc, pClientApp) != OA_TRUE)
    OAC_ERROR("Could not restart client application.");
}

void oacContext::ClearBenchmarks(void)
{
  pBenchmarks.clear();
}

const vector<string> &oacContext::GetBenchmarks(bool force_cmd)
{
  if(force_cmd || !pHaveBenchmarks)
  {
    oaCommand Command;
    oaInitCommand(&Command);
    Command.Type = OA_CMD_GET_BENCHMARKS;
    IssueCommand(&Command);
  }

  return(pBenchmarks);
}

double oacContext::Rand(void)
{
  assert(sizeof(pRandSeed) == 4);
  pRandSeed = (1366 * pRandSeed + 150889) % 714025;
  return((double)pRandSeed  / 714025.0);
}

void oacContext::SRand(unsigned long seed)
{
  pRandSeed = seed;
}

static void FreeOption(oaNamedOption &option)
{
  assert(option.Name != NULL);
  free((void *)option.Name);

  switch(option.DataType)
  {
    case OA_TYPE_STRING:
      free(option.Value.String);
      break;

    case OA_TYPE_ENUM:
      free(option.Value.Enum);
      break;
  }
  if(option.Dependency.ParentName)
    free((void *)option.Dependency.ParentName);
}

void oacContext::ClearAllOptions(void)
{
  vector<oaNamedOption>::iterator Iter = pAllOptions.begin();
  for(; Iter != pAllOptions.end(); Iter++)
    FreeOption(*Iter);

  map<string, OptionVec *>::iterator MapIter = pAllOptionsMap.begin();
  for(; MapIter != pAllOptionsMap.end(); MapIter++)
    delete MapIter->second;

  pAllOptions.clear();
  pAllOptionsMap.clear();
}

const vector<oaNamedOption> &oacContext::GetAllOptions(bool force_cmd)
{
  if(force_cmd || !pHaveAllOptions)
  {
    oaCommand Command;
    oaInitCommand(&Command);
    Command.Type = OA_CMD_GET_ALL_OPTIONS;
    IssueCommand(&Command);
  }

  return(pAllOptions);
}

const std::map<std::string, oacContext::OptionVec *> &
  oacContext::GetAllOptionsMap(bool force_cmd)
{
  GetAllOptions(force_cmd);  // ensure we have the map filled.
  return(pAllOptionsMap);
}

const oacContext::OptionVec *
  oacContext::GetOption(const oaChar *name, bool force_cmd)
{
  return(GetOption(std::string(name), force_cmd));  
}

const oacContext::OptionVec *
  oacContext::GetOption(const std::string &name, bool force_cmd)
{
  const std::map<std::string, OptionVec *> &Map =
    GetAllOptionsMap(force_cmd);

  std::map<string, OptionVec *>::const_iterator Found = Map.find(name);

  if(Found == Map.end())
    return(NULL);

  return(Found->second);
}

void oacContext::ClearCurrentOptions(void)
{
  vector<OptionValue *>::iterator Iter = pCurrentOptions.begin();
  for(; Iter != pCurrentOptions.end(); Iter++)
    delete *Iter;

  pCurrentOptions.clear();
  pCurrentOptionsMap.clear();
}

const vector<oacContext::OptionValue *> &
  oacContext::GetCurrentOptions(bool force_cmd)
{
  if(force_cmd || !pHaveCurrentOptions)
  {
    oaCommand Command;
    oaInitCommand(&Command);
    Command.Type = OA_CMD_GET_CURRENT_OPTIONS;
    IssueCommand(&Command);
  }

  return(pCurrentOptions);
}

const std::map<std::string, oacContext::OptionValue *> &
  oacContext::GetCurrentOptionsMap(bool force_cmd)
{
  GetCurrentOptions(force_cmd); // ensure we have the map filled.
  return(pCurrentOptionsMap);
}

const oacContext::OptionValue *
  oacContext::GetOptionValue(const oaChar *name, 
                             bool force_cmd)
{
  return(GetOptionValue(std::string(name), force_cmd));  
}

const oacContext::OptionValue *
  oacContext::GetOptionValue(const std::string &name,
                             bool force_cmd)
{
  const std::map<std::string, OptionValue *> &Map =
    GetCurrentOptionsMap(force_cmd);

  std::map<string, OptionValue *>::const_iterator Found = Map.find(name);

  if(Found == Map.end())
    return(NULL);

  return(Found->second);
}


void oacContext::RunTests(void)
{
  assert(State() == STATE_INIT);

  PushState(STATE_RUNNING_TEST);

  pCurrentTest = pTests.begin();
  for(int i=1; pCurrentTest != pTests.end();pCurrentTest++, i++)
  {
    oacTestBase *Test = *pCurrentTest;
    
    oacTestBase::Info *Info = Test->GetInfo();

    OAC_START_TEST_GROUP(Info->Name)
    Test->Run();
    OAC_END_TEST_GROUP

    delete Info;
  }

  PopState();
  ChangeState(STATE_FINISHED);

  OAC_MSG("Final summary: " << oacLog::TestsSummary())
}
  
oacContext::StateType oacContext::State(void) const 
{ 
  return(pState.back()); 
}

void oacContext::PushState(StateType state) 
{ 
  pState.push_back(state); 
}

void oacContext::ChangeState(StateType state) 
{ 
  assert(pState.size() > 0);
  pState.back() = state; 
}

void oacContext::PopState(void) 
{ 
  assert(pState.size() > 1);
  pState.pop_back(); 
}

//******************************************************************************
//*** OA callback methods
//******************************************************************************

#define INC_CALL_COUNT(is_get_next_command) \
  pCallCount++; \
  if(!is_get_next_command) \
  { \
    OAC_TEST_MSG(pCallCount != 1, \
      "The first OA call made by the application must be oaGetNextCommand()."); \
  }

oaCommandType oacContext::GetNextCommand(oaCommand *command)
{
  INC_CALL_COUNT(true);

  assert(pCurrent != NULL);

  assert(command != NULL);
  assert(command->StructSize == sizeof(oaCommand));
  assert(pCurrent->State() == STATE_INIT);


  if(pCurrent)
    pCurrent->RunTests();

  if(command != NULL)
    command->Type = OA_CMD_EXIT;

  return(OA_CMD_EXIT);
}

#define TEST_CURRENT_CMD(cmd, func_name) \
  { \
    const char TmpMsg[] =  "The command " #cmd " must be issued before " \
      #func_name "() is called."; \
    OAC_TEST_MSG(pCurrent->State() == STATE_ISSUING_COMMAND, TmpMsg); \
    OAC_TEST_MSG(pCurrent->pCurrentCommand == cmd, TmpMsg); \
  }

oaNamedOption *oacContext::GetNextOption(void)
{
  INC_CALL_COUNT(false);

  TEST_CURRENT_CMD(OA_CMD_SET_OPTIONS, oaGetNextOption)

  if(pCurrent->State() == STATE_ISSUING_COMMAND)
    return((*pCurrent->pCurrentTest)->GetNextOption());

  return(NULL);
}

static void CopyOption(const oaNamedOption *src, oaNamedOption &dst)
{
  oaInitOption(&dst);
  memcpy(&dst, src, sizeof(oaNamedOption));
  dst.Name = strdup(src->Name);

  switch(src->DataType)
  {
    case OA_TYPE_STRING:
      dst.Value.String = strdup(src->Value.String);
      break;

    case OA_TYPE_ENUM:
      dst.Value.Enum = strdup(src->Value.Enum);
      break;
  }

  if(src->Dependency.ParentName)
    dst.Dependency.ParentName = strdup(src->Dependency.ParentName);
}

void oacContext::AddOption(const oaNamedOption *option)
{
  INC_CALL_COUNT(false);
  TEST_CURRENT_CMD(OA_CMD_GET_ALL_OPTIONS , oaAddOption)

  string Name(option->Name);

  oaNamedOption NewOption;
  CopyOption(option, NewOption);
  pCurrent->pAllOptions.push_back(NewOption);

  const OptionVec *FoundOption = pCurrent->GetOption(Name);
  if(FoundOption != NULL)
  {
    assert(FoundOption->size() > 0);

    oaOptionDataType FoundType = FoundOption->back().DataType;

    OAC_TEST_MSG(FoundType == OA_TYPE_ENUM,
     "Option named '" << Name << "' was added with oaAddOption() more than "
     "once.  This is only allowed for options of enum type.  The option "
     " already added is of type '" << oacOAUtils::TypeToStr(FoundType) << "'.");

    OAC_TEST_MSG(NewOption.DataType == OA_TYPE_ENUM,
     "Option named '" << Name << "' was added with oaAddOption() more than "
     "once.  This is only allowed for options of enum type.  The option "
     " being added is of type '" << oacOAUtils::TypeToStr(NewOption.DataType) <<
     "'.");
  }
  else
    pCurrent->pAllOptionsMap[Name] = new OptionVec;
    
  pCurrent->pAllOptionsMap[Name]->push_back(NewOption);
  

  if(pCurrent->State() == STATE_ISSUING_COMMAND)
    (*pCurrent->pCurrentTest)->AddOption(option);
}

void oacContext::AddOptionValue(const oaChar *name,
                                oaOptionDataType value_type,
                                const oaValue *value)
{
  INC_CALL_COUNT(false);
  TEST_CURRENT_CMD(OA_CMD_GET_CURRENT_OPTIONS , oaAddOptionValue)

  string Name(name);

  OptionValue *NewOption = new OptionValue;
  NewOption->Set(name, value_type, value);

  pCurrent->pCurrentOptions.push_back(NewOption);

  map<string, OptionValue *>::const_iterator FoundOption;
  FoundOption = pCurrent->pCurrentOptionsMap.find(Name);
  OAC_TEST_MSG(FoundOption == pCurrent->pCurrentOptionsMap.end(),
   "Option named '" << Name << "' was added with oaAddOptionValue() more than "
   "once.");

  pCurrent->pCurrentOptionsMap[Name] = NewOption;

  if(pCurrent->State() == STATE_ISSUING_COMMAND)
    (*pCurrent->pCurrentTest)->AddOptionValue(name, value_type, value);
}

void oacContext::AddBenchmark(const oaChar *benchmark_name)
{
  INC_CALL_COUNT(false);
  TEST_CURRENT_CMD(OA_CMD_GET_BENCHMARKS , oaAddBenchmark)

  pCurrent->pBenchmarks.push_back(string(benchmark_name));

  if(pCurrent->State() == STATE_ISSUING_COMMAND)
    (*pCurrent->pCurrentTest)->AddBenchmark(benchmark_name);
}

oaBool oacContext::SendSignal(oaSignalType signal, void *param)
{
  INC_CALL_COUNT(false);

  if(pCurrent->State() == STATE_ISSUING_COMMAND)
    (*pCurrent->pCurrentTest)->SendSignal(signal, param);

  return(OA_FALSE);
}

void oacContext::StartBenchmark(void)
{
  INC_CALL_COUNT(false);
  TEST_CURRENT_CMD(OA_CMD_RUN_BENCHMARK , oaStartBenchmark)

  if(pCurrent->State() == STATE_ISSUING_COMMAND)
    (*pCurrent->pCurrentTest)->StartBenchmark();
}

void oacContext::DisplayFrame(oaFloat t)
{
  INC_CALL_COUNT(false);
  TEST_CURRENT_CMD(OA_CMD_RUN_BENCHMARK , oaDisplayFrame)

  if(pCurrent->State() == STATE_ISSUING_COMMAND)
    (*(pCurrent->pCurrentTest))->DisplayFrame(t);
}

void oacContext::EndBenchmark(void)
{
  INC_CALL_COUNT(false);
  TEST_CURRENT_CMD(OA_CMD_RUN_BENCHMARK , oaEndBenchmark)

  if(pCurrent->State() == STATE_ISSUING_COMMAND)
    (*pCurrent->pCurrentTest)->EndBenchmark();
}

void oacContext::AddResultValue(const oaChar *name, 
                                oaOptionDataType value_type,
                                const oaValue *value)
{
  INC_CALL_COUNT(false);
  TEST_CURRENT_CMD(OA_CMD_RUN_BENCHMARK , oaAddResultValue)

  if(pCurrent->State() == STATE_ISSUING_COMMAND)
    (*pCurrent->pCurrentTest)->AddResultValue(name, value_type, value);
}

void oacContext::AddFrameValue(const oaChar *name, 
                               oaOptionDataType value_type,
                               const oaValue *value)
{
  INC_CALL_COUNT(false);
  TEST_CURRENT_CMD(OA_CMD_RUN_BENCHMARK , oaAddFrameValue)

  if(pCurrent->State() == STATE_ISSUING_COMMAND)
    (*pCurrent->pCurrentTest)->AddFrameValue(name, value_type, value);
}


