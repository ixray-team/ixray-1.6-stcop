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
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <vector>
#include <map>
#include <string>
#include <OpenAutomate.h>

using namespace std;

#if WIN32
# include <windows.h>
# define SLEEP(ms) Sleep(ms)
#else
# include <unistd.h>
# define SLEEP(ms) usleep(1000 * (ms))
#endif

static void ParseArgs(int argc, char *argv[]);
static void Error(const char *fmt, ...);
static void OAMainLoop(const char *opt);
static void RunApp(void);
static void InitOptions(void);
static void SetOptionValue(const char *name, 
                           oaOptionDataType type,
                           const oaValue *value);
static void WriteOptionsFile(void);
static void ReadOptionsFile(void);

static void *Alloc(size_t n);
static char *StrDup(const char *str);
static void Cleanup(void);

static int OAModeFlag = 0;
static char *OAOpt = NULL;

static oaNamedOption Options[128];
static oaInt NumOptions = 0;

static char OptionsFileName[] = ".simple_app_options.txt";

struct OptionValue
{
  const char *Name;
  oaOptionDataType Type;
  oaValue Value;
};

static map<string, OptionValue> OptionValueMap;
static vector<void *> AllocBlocks;

const oaChar *Benchmarks[] = {
                               "forest",
                               "crates",
                               "map1",
                               NULL
                             };
oaInt NumBenchmarks = -1;

int main(int argc, char *argv[])
{
  ParseArgs(argc, argv);

  if(OAModeFlag)
  {
    OAMainLoop(OAOpt);

    Cleanup();
    return(0);
  } 

  /* Run as normal if not in oa mode */
  RunApp();

  return(0);
}

void GetAllOptions(void)
{
  for(oaInt i=0; i < NumOptions; ++i)
    oaAddOption(&Options[i]);
}

void GetCurrentOptions(void)
{
  map<string, OptionValue>::const_iterator Iter = OptionValueMap.begin();

  for(; Iter != OptionValueMap.end(); ++Iter)
  {
    // Skip "User/Music Enabled" option of "User/Sound" is disabled
    if(!strcmp(Iter->second.Name, "User/Music Enabled") &&
       OptionValueMap["User/Sound"].Value.Bool == OA_FALSE)
      continue;

    oaAddOptionValue(Iter->second.Name,
                     Iter->second.Type,
                     &Iter->second.Value);
  }
}

void SetOptions(void)
{
  oaNamedOption *Option;

  while((Option = oaGetNextOption()) != NULL)
  {
    /*
     * Set option value to persist for subsequent runs of the game 
     * to the given value.  Option->Name will be the name of the value, 
     * and Option->Value will contain the appropriate value.
     */

    SetOptionValue(Option->Name, Option->DataType, &Option->Value);
  }

  WriteOptionsFile();
}

void GetBenchmarks(void)
{
  /* foreach known available benchmark call oaAddBenchmark() with a unique string
     identifying it */
  for(oaInt i=0; i < NumBenchmarks; ++i)
    oaAddBenchmark(Benchmarks[i]);
}

void RunBenchmark(const oaChar *benchmark_name)
{
  oaValue Val;
  int i;

  bool FoundBenchmark = false;
  for(i=0; i < NumBenchmarks; ++i)
    if(!strcmp(Benchmarks[i], benchmark_name))
    {
      FoundBenchmark = true;
      break;
    }

  /* Check if the requested benchark is valid */
  if(!FoundBenchmark)
  {
    char Msg[1024];
    sprintf(Msg, "'%s' is not a valid benchmark.", benchmark_name);
    OA_RAISE_ERROR(INVALID_BENCHMARK, benchmark_name);
  }


  /* Setup everything to run the benchmark */

  /* oaStartBenchmark() must be called right before the first frame */ 
  oaStartBenchmark();

  /* 
   * Run the benchmark, and call oaDisplayFrame() right before the final
   * present call for each frame
   */
  for(i=0; i < 50; ++i)
   {
    SLEEP(20);
    oaDisplayFrame((oaFloat)(i * 20) / (oaFloat)1000);
   }


  /* Return some result values */
  Val.Int = 18249;
  oaAddResultValue("Score", OA_TYPE_INT, &Val);

  Val.Float = 29.14;
  oaAddResultValue("Some other score", OA_TYPE_FLOAT, &Val);

  /* oaStartBenchmark() must be called right after the last frame */ 
  oaEndBenchmark();
}

void RunApp(void)
{
  /* Run the application as it normally would run without -openautomate mode */
  fprintf(stderr, "- Running application as normal.\n");
}

#define MSG(msg) \
  fprintf(stderr, "- %s\n", msg);

void OAMainLoop(const char *opt)
{
  oaCommand Command;

  oaVersion Version;
  if(!oaInit((const oaString)OAOpt, &Version))
   Error("OA did not initialize properly.");

  InitOptions();

  while(1)
   {
    oaInitCommand(&Command);
    switch(oaGetNextCommand(&Command))
     {
      /* No more commands, exit program */
      case OA_CMD_EXIT: 
       return;

      /* Run as normal */
      case OA_CMD_RUN: 
       RunApp();
       return;

      /* Enumerate all in-game options */
      case OA_CMD_GET_ALL_OPTIONS: 
       GetAllOptions();
       break;

      /* Return the option values currently set */
      case OA_CMD_GET_CURRENT_OPTIONS:
       GetCurrentOptions();
       break;

      /* Set all in-game options */
      case OA_CMD_SET_OPTIONS: 
       SetOptions();
       break;

      /* Enumerate all known benchmarks */
      case OA_CMD_GET_BENCHMARKS: 
       GetBenchmarks();
       break;

      /* Run benchmark */
      case OA_CMD_RUN_BENCHMARK: 
       RunBenchmark(Command.BenchmarkName);
       break;
     }
   }
}

void ParseArgs(int argc, char *argv[])
{
  for(int i=1;  i < argc; ++i)
   {
     if(!strcmp(argv[i], "-openautomate"))
      {
       i++;
       if(i >= argc)
        Error("-openautomate option must have an argument.\n");

       OAModeFlag = 1;
       OAOpt = argv[i];
       continue;
      }
     
     Error("Unknown option '%s'\n", argv[i]);
   }
}

void InitOptions(void)
{
  //****************************************************************************
  //*** Init Options
  //****************************************************************************
  {
    oaNamedOption *Option;
    

    Option = &Options[NumOptions++];
    oaInitOption(Option);
    Option->Name = "User/Resolution";
    Option->DataType = OA_TYPE_ENUM;
    Option->Value.Enum = "640x480";

    Options[NumOptions] = *Option;
    Option = &Options[NumOptions++];
    Option->Value.Enum = "1024x768";

    Options[NumOptions] = *Option;
    Option = &Options[NumOptions++];
    Option->Value.Enum = "1600x1200";

    /* AA (enum) */
    Option = &Options[NumOptions++];
    oaInitOption(Option);
    Option->Name = "User/AA";
    Option->DataType = OA_TYPE_ENUM;

    Option->Value.Enum = "Off";

    Options[NumOptions] = *Option;
    Option = &Options[NumOptions++];
    Option->Value.Enum = "2X";

    Options[NumOptions] = *Option;
    Option = &Options[NumOptions++];
    Option->Value.Enum = "4X";

    /* Sound (bool) */
    Option = &Options[NumOptions++];
    oaInitOption(Option);
    Option->Name = "User/Sound";
    Option->DataType = OA_TYPE_BOOL;

    /* Music Enabled (bool) */
    Option = &Options[NumOptions++];
    oaInitOption(Option);
    Option->Name = "User/Music Enabled";
    Option->DataType = OA_TYPE_BOOL;
    Option->Dependency.ParentName = "User/Sound";
    Option->Dependency.ComparisonOp = OA_COMP_OP_EQUAL;
    Option->Dependency.ComparisonValType = OA_TYPE_BOOL;
    Option->Dependency.ComparisonVal.Bool = OA_ON;

    /* Enemy Density (int) */
    Option = &Options[NumOptions++];
    oaInitOption(Option);
    Option->Name = "User/Enemy Density";
    Option->DataType = OA_TYPE_INT;
    Option->MinValue.Int = 0;
    Option->MaxValue.Int = 100;
    Option->NumSteps = 1;

    /* Compression Level (int) */
    Option = &Options[NumOptions++];
    oaInitOption(Option);
    Option->Name = "Compression Level";
    Option->DataType = OA_TYPE_INT;
    Option->MinValue.Int = 10;
    Option->MaxValue.Int = 0;
    Option->NumSteps = 10;

    /* Texture Quality (float) */
    Option = &Options[NumOptions++];
    oaInitOption(Option);
    Option->Name = "Texture Quality";
    Option->DataType = OA_TYPE_FLOAT;
    Option->MinValue.Float = 0.0;
    Option->MaxValue.Float = 100.0;
    Option->NumSteps = 201;

    /* Texture Size (enum) */
    Option = &Options[NumOptions++];
    oaInitOption(Option);
    Option->Name = "Texture Size";
    Option->DataType = OA_TYPE_ENUM;
    Option->Value.Enum = "128";

    Options[NumOptions] = *Option;
    Option = &Options[NumOptions++];
    Option->Value.Enum = "256";

    Options[NumOptions] = *Option;
    Option = &Options[NumOptions++];
    Option->Value.Enum = "512";
  }

  //****************************************************************************
  //*** Init OptionValues
  //****************************************************************************

  
  // Initialize default options
  for(int i=0; i < NumOptions; ++i)
  {
    string Name(Options[i].Name);
    OptionValueMap[Name].Name = Options[i].Name;
    OptionValueMap[Name].Type = Options[i].DataType;
    OptionValueMap[Name].Value = Options[i].Value;
  }

  oaValue Value;

  Value.Enum = "1024x768";
  SetOptionValue("User/Resolution", OA_TYPE_ENUM, &Value);

  Value.Enum = "4x";
  SetOptionValue("User/AA", OA_TYPE_ENUM, &Value);

  Value.Bool = OA_OFF;
  SetOptionValue("User/Sound", OA_TYPE_BOOL, &Value);

  Value.Int = 5;
  SetOptionValue("Compression Level", OA_TYPE_INT, &Value);

  Value.Int = 5;
  SetOptionValue("User/Enemy Density", OA_TYPE_INT, &Value);

  Value.Float = 20.5;
  SetOptionValue("Texture Quality", OA_TYPE_FLOAT, &Value);

  Value.Enum = "256";
  SetOptionValue("Texture Size", OA_TYPE_ENUM, &Value);


  // Load any persistent options if they've been previously set
  ReadOptionsFile();

  //****************************************************************************
  //*** Init Benchmarks
  //****************************************************************************

  for(NumBenchmarks=0; Benchmarks[NumBenchmarks]; ++NumBenchmarks);
}

void *Alloc(size_t n)
{
  void *Ret = malloc(n);
  assert(n != NULL);

  AllocBlocks.push_back(Ret);
  return(Ret);
}

char *StrDup(const char *str)
{
  assert(str != NULL);
  char *Ret = strdup(str);

  AllocBlocks.push_back(Ret);
  return(Ret);
}

void Cleanup(void)
{
  vector<void *>::iterator Iter = AllocBlocks.begin();
  for(; Iter != AllocBlocks.end(); Iter++)
    free(*Iter);

  AllocBlocks.clear();
}

static int HexCharToInt(unsigned char c)
{
  if(c >= '0' && c <= '9')
    return((int)(c - '0'));

  c = tolower(c);
  if(c >= 'a' && c <= 'f')
    return((int)(c - 'a' + 10));

  return(0);
}

static int Hex2BytesToInt(const unsigned char *buf)
{
  return(HexCharToInt(buf[0]) << 4 | HexCharToInt(buf[1]));
}

static oaFloat StrToFloat(const char *buf)
{
  size_t Len = strlen(buf);
  if(Len != sizeof(oaFloat) * 2)
    return(0.0);

  oaFloat Ret;
  unsigned char *Buf = (unsigned char *)&Ret;
  for(int i=0; i < sizeof(oaFloat); ++i)
  {
    Buf[i] = Hex2BytesToInt((const unsigned char *)buf);
    buf += 2;
  }
  
  return(Ret);
}

static void WriteFloat(FILE *fp, oaFloat val)
{
  unsigned char *Buf = (unsigned char *)&val;
  for(int i=0;  i < sizeof(oaFloat); ++i)
    fprintf(fp, "%02x", Buf[i]);
}

void SetOptionValue(const char *name, 
                    oaOptionDataType type,
                    const oaValue *value)
{
  string Name(name);
  assert(OptionValueMap.find(Name) != OptionValueMap.end());
  assert(OptionValueMap[Name].Type == type);

  switch(type)
  {
    case OA_TYPE_STRING:
      OptionValueMap[Name].Value.String = StrDup(value->String);
      break;

    case OA_TYPE_ENUM:
      OptionValueMap[Name].Value.Enum = StrDup(value->Enum);
      break;

    default:
      OptionValueMap[Name].Value = *value;
  }
}

void WriteOptionsFile(FILE *fp)
{
  map<string, OptionValue>::const_iterator Iter = OptionValueMap.begin();
  for(; Iter != OptionValueMap.end(); ++Iter)
  {
    fprintf(fp, "%s\t", Iter->second.Name);
    switch(Iter->second.Type)
    {
      case OA_TYPE_INT:
        fprintf(fp, "%d", Iter->second.Value.Int);
        break;

      case OA_TYPE_BOOL:
        fprintf(fp, "%d", (int)Iter->second.Value.Bool);
        break;

      case OA_TYPE_FLOAT:
        WriteFloat(fp, Iter->second.Value.Float);
        break;

      case OA_TYPE_STRING:
        fprintf(fp, "%s", Iter->second.Value.String);
        break;

      case OA_TYPE_ENUM:
        fprintf(fp, "%s", Iter->second.Value.Enum);
        break;

    }

    fprintf(fp, "\n");
  }
}

void WriteOptionsFile(void)
{
  fprintf(stderr, "simple_app: Writing options file \"%s\".\n", 
          OptionsFileName);
  fflush(stderr);

  FILE *FP = fopen(OptionsFileName, "wb");
  if(!FP)
    Error("Couldn't open \"%s\" for write.\n", OptionsFileName);

  WriteOptionsFile(FP);

  fclose(FP);
}


static void StripNewLine(char *str)
{
  size_t StrLen = strlen(str);
  if(StrLen == 0)
    return;
  
  for(size_t i=StrLen-1; i >= 0; ++i)
    if(str[i] == '\n')
    {
      str[i] = 0;
      break;
    }
}

static void SetOptionValue(const char *name, 
                           oaOptionDataType type,
                           const char *value)
{
  assert(name != NULL);
  assert(type != OA_TYPE_INVALID);
  assert(value != NULL);

  oaValue Value;
  switch(type)
  {
    case OA_TYPE_INT:
      Value.Int = atoi(value);
      break;

    case OA_TYPE_FLOAT:
      Value.Float = StrToFloat(value);
      break;

    case OA_TYPE_BOOL:
      Value.Bool = atoi(value) ? OA_TRUE : OA_FALSE;
      break;

    case OA_TYPE_STRING:
      Value.String = (oaString)value;
      break;

    case OA_TYPE_ENUM:
      Value.Enum = (oaString)value;
      break;
  }

  SetOptionValue(name, type, &Value);
}

void ReadOptionsFile(void)
{
  FILE *FP = fopen(OptionsFileName, "rb");
  if(!FP)
    return;

  fprintf(stderr, "simple_app: Reading options file \"%s\".\n", 
          OptionsFileName);
  fflush(stderr);

  char Line[1024];
  int LineNum = 1;
  while(fgets(Line, sizeof(Line), FP) != NULL)
  {
    StripNewLine(Line);
    if(Line[0] == 0)
      continue;

    char *Name = strtok(Line, "\t");
    char *Value = strtok(NULL, "");

    if(!Name || !Value)
      Error("Invalid format in options file \"%s\" on line %d\n", 
            OptionsFileName, 
            LineNum);

    map<string, OptionValue>::const_iterator OptVal = 
      OptionValueMap.find(string(Name));

    if(OptVal == OptionValueMap.end())
      Error("Unknown option \"%s\" defined in options file \"%s\" on line %d.",
            Name, 
            OptionsFileName, 
            LineNum);

    SetOptionValue(Name, OptVal->second.Type, Value);

    LineNum++;
  }
}

void Error(const char *fmt, ...)
 {
  va_list AP;
  va_start(AP, fmt);

  fprintf(stderr, "ERROR: ");
  vfprintf(stderr, fmt, AP);
  fprintf(stderr, "\n");
  exit(-1);
 }


