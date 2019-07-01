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

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <OpenAutomate_Internal.h>
#include <windows.h>
#include <vector>
#include <map>
#include <time.h>

#ifdef __cplusplus
extern "C"
{
#endif

#define DLLEXPORT __declspec(dllexport)

using namespace std;

#define PRINT(var) \
    cerr << __FILE__ << "," << __LINE__ << ": " #var " = " << (var) << endl;

static FILE *LogFile = stderr;


BOOL APIENTRY DllMain(HANDLE hModule, 
                      DWORD  ul_reason_for_call, 
                      LPVOID lpReserved)
{

  return TRUE;
}


static oaiFunctionTable FuncTable;
static int CurrentStage = 0;
static oaChar BenchmarkName[1024];
static int NumFrames = 0;
static time_t StartTime;


oaCommandType GetNextCommand(oaCommand *command)
{
  CurrentStage = CurrentStage % 6;

  switch(CurrentStage)
  {
    case 0:
      CurrentStage++;
      return(OA_CMD_GET_ALL_OPTIONS);

    case 1:
      CurrentStage++;
      return(OA_CMD_GET_CURRENT_OPTIONS);

    case 2:
      CurrentStage++;
      return(OA_CMD_SET_OPTIONS);

    case 3:
      CurrentStage++;
      return(OA_CMD_GET_BENCHMARKS);

    case 4:
      CurrentStage++;
      command->BenchmarkName = BenchmarkName;
      return(OA_CMD_RUN_BENCHMARK);

    case 5:
      CurrentStage++;
      return(OA_CMD_EXIT);
  }

  return(OA_CMD_EXIT);
}

void AddOption(const oaNamedOption *option)
{
  fprintf(LogFile, "IN AddOption: '%s'\n", option->Name);

}

void AddOptionValue(const OA_CHAR *name, oaOptionDataType value_type, const oaValue *value)
{
  switch(value_type)
  {
    case OA_TYPE_STRING:
      fprintf(LogFile, "Current option (string)'%s' = %s\n", name, value->String);
      break;

    case OA_TYPE_INT:
      fprintf(LogFile, "Current option (int)'%s' = %d\n", name, value->Int);
      break;

    case OA_TYPE_FLOAT:
      fprintf(LogFile, "Current option (float)'%s' = %f\n", name, value->Float);
      break;

    case OA_TYPE_ENUM:
      fprintf(LogFile, "Current option (enum)'%s' = %s\n", name, value->Enum);
      break;

    case OA_TYPE_BOOL:
      fprintf(LogFile, "Current option (bool)'%s' = %d\n", name, value->Bool);
      break;

    default:
      fprintf(LogFile, "Current option (unknown type)'%s' = ???", name);
  }
}

void AddBenchmark(const oaChar *benchmark_name)
{
  fprintf(LogFile, "IN AddBenchmark: %s\n", benchmark_name);
  OA_STRNCPY(BenchmarkName, benchmark_name, sizeof(BenchmarkName));
}

oaNamedOption *GetNextOption(void)
{
  return(NULL);
}

static void StartBenchmark(void)
{
  NumFrames = 0;
  StartTime = time(NULL);

  fprintf(LogFile, "Benchmark started\n", BenchmarkName);
}

static void DisplayFrame(oaFloat t)
{
  NumFrames++;
}

static void EndBenchmark(void)
{
  time_t TotalTime = time(NULL) - StartTime;
  float AvgFPS;  


  if(TotalTime == 0)
    TotalTime = 1;

  AvgFPS = (float)NumFrames / (float)TotalTime;
  
  fprintf(LogFile, "Benchmark ended\n");
  fprintf(LogFile, "  Total time = %ds\n", TotalTime);
  fprintf(LogFile, "  Avg. FPS = %f\n", AvgFPS);
}

static void AddResultValue(const OA_CHAR *name, 
                           oaOptionDataType value_type,
                           const oaValue *value)
{
  switch(value_type)
  {
    case OA_TYPE_STRING:
      fprintf(LogFile, "Result value (string)'%s' = %s\n", name, value->String);
      break;

    case OA_TYPE_INT:
      fprintf(LogFile, "Result value (int)'%s' = %d\n", name, value->Int);
      break;

    case OA_TYPE_FLOAT:
      fprintf(LogFile, "Result value (float)'%s' = %f\n", name, value->Float);
      break;

    case OA_TYPE_ENUM:
      fprintf(LogFile, "Result value (enum)'%s' = %s\n", name, value->Enum);
      break;

    case OA_TYPE_BOOL:
      fprintf(LogFile, "Result value (bool)'%s' = %d\n", name, value->Bool);
      break;


    default:
      fprintf(LogFile, "Result value (unknown type)'%s' = ???", name);
  }
}

DLLEXPORT oaiFunctionTableStruct *oaPluginInit(const char *init_str, 
                                               oaVersion *version)
{
  OA_INIT_VERSION_STRUCT(*version)

  
  oaiInitFuncTable(&FuncTable);
  FuncTable.GetNextCommand = GetNextCommand;
  FuncTable.AddOption = AddOption;
  FuncTable.AddOptionValue = AddOptionValue;
  FuncTable.AddBenchmark = AddBenchmark;
  FuncTable.GetNextOption = GetNextOption;
  FuncTable.StartBenchmark = StartBenchmark;
  FuncTable.DisplayFrame = DisplayFrame;
  FuncTable.EndBenchmark = EndBenchmark;
  FuncTable.AddResultValue = AddResultValue;

  return(&FuncTable);
}

#ifdef __cplusplus
}

#endif


