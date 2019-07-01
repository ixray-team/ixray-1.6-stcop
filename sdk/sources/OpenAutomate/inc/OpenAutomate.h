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

#ifndef _OA_h
#define _OA_h

#define OA_WIN32  1
#define OA_CYGWIN 2
#define OA_LINUX  3
#define OA_DARWIN 4

/* Automatic Platform detection */
#if defined(WIN32)
#define OA_PLATFORM OA_WIN32
#pragma warning(disable:4995)
#pragma warning(disable:4996) 
#else
#define OA_PLATFORM OA_CYGWIN
#endif

#include <string.h>

#define OA_CHAR char
#define OA_STRCPY strcpy
#define OA_STRNCPY strncpy
#define OA_STRLEN strlen

#ifdef __cplusplus
extern "C"
{
#endif


/******************************************************************************* 
 * Types
 ******************************************************************************/

typedef enum
{
  OA_FALSE = 0,
  OA_OFF   = 0,
  OA_TRUE  = 1,
  OA_ON    = 1
} oaBool;

typedef OA_CHAR oaChar;
typedef oaChar *oaString;
typedef long int oaInt;
typedef double oaFloat;


/* 
 * Used for by oaInit to return the version number of the API.  The version
 * number will be equivalent to (Major + .001 * Minor).  Versions of the API
 * with differing major numbers are incompatible, whereas versions where only
 * the minor number differ are.
 */
typedef struct oaVersionStruct
{
  oaInt Major;  
  oaInt Minor; 
  oaInt Custom;
  oaInt Build;
} oaVersion;
 

typedef enum
{
  OA_TYPE_INVALID  = 0,
  OA_TYPE_STRING  = 1,
  OA_TYPE_INT     = 2,
  OA_TYPE_FLOAT   = 3,
  OA_TYPE_ENUM    = 4,
  OA_TYPE_BOOL    = 5
} oaOptionDataType;

typedef enum
{
  OA_COMP_OP_INVALID           = 0,
  OA_COMP_OP_EQUAL             = 1,
  OA_COMP_OP_NOT_EQUAL         = 2,
  OA_COMP_OP_GREATER           = 3,
  OA_COMP_OP_LESS              = 4,
  OA_COMP_OP_GREATER_OR_EQUAL  = 5,
  OA_COMP_OP_LESS_OR_EQUAL     = 6,
} oaComparisonOpType;

typedef struct oaValueStruct
{
  union
  {
    oaString String;      
    oaInt Int;
    oaFloat Float;
    oaString Enum;
    oaBool Bool;
  };
} oaValue;

typedef enum 
{
  OA_SIGNAL_SYSTEM_UNDEFINED = 0x0,  /* Should never be used */

  /* used for errors, warnings, and log messages */
  OA_SIGNAL_ERROR     = 0x1,         

  /* requests a reboot of the system */
  OA_SIGNAL_SYSTEM_REBOOT    = 0xF,
} oaSignalType;

typedef enum
{
  OA_ERROR_NONE                 = 0x00, /* no error */
  OA_ERROR_WARNING              = 0x01, /* not an error, just a warning*/
  OA_ERROR_LOG                  = 0x02, /* not an error, just a log message */

  OA_ERROR_INVALID_OPTION       = 0x10, /* option is invalid (wrong name) */
  OA_ERROR_INVALID_OPTION_VALUE = 0x11, /* option value is out of range */

  OA_ERROR_INVALID_BENCHMARK    = 0x21, /* chosen benchmark is invalid */

  OA_ERROR_OTHER                = 0xFF, /* unknown error */
} oaErrorType;

typedef struct oaMessageStruct
{
  oaInt StructSize; /* Size in bytes of the whole struct */

  oaErrorType Error;     /* Only used for OA_SIGNAL_ERROR */ 
  const oaChar *Message; 
} oaMessage;


/* Used when a parameter is only enabled if another parameter value meets
   a certain condition.  For example, the "AA Level" parameter may only be
   enabled if the "AA" parameter is equal to "On" */
typedef struct oaOptionDependencyStruct
{
  oaInt StructSize; /* Size in bytes of the whole struct */

  /* Name of the parent parameter the param will be dependent on */
  const oaChar *ParentName;
  
  /* The operator used to compare the parent value with ComparisonVal */
  oaComparisonOpType ComparisonOp;

  /* The value compared against the parents value.  It must be the same type */
  oaValue ComparisonVal;

  /* Data type of the comparison value */
  oaOptionDataType ComparisonValType;  

} oaOptionDependency;

typedef struct oaNamedOptionStruct
{
  oaInt StructSize; /* Size in bytes of the whole struct */

  oaOptionDataType DataType;  
  const oaChar *Name;             

  /* Currently only used for OA_TYPE_ENUM */
  oaValue Value;

  /* Used only for numeric types OA_TYPE_INT and OA_TYPE_FLOAT */
  oaValue MinValue;
  oaValue MaxValue;

  /* determines the allowable values for an option given min/max              */
  /*   NumSteps == -1  range is [-inf, inf]                                   */
  /*   NumSteps ==  0  range is continuous within [MinValue, MaxValue]        */
  /*   NumSteps >   0  assumes NumSteps uniform increments between min/max    */
  /*                   eg, if min = 0, max = 8, and NumSteps = 4, then our    */
  /*                   option can accept any value in the set {0, 2, 4, 6, 8} */
  oaInt NumSteps;   
  
  /* If Dependency is defined, the parameter is only enabled if the 
     condition defined within OptionDependency is true */
  oaOptionDependency Dependency;
} oaNamedOption;

typedef enum
{
  OA_CMD_EXIT                = 0, /* The app should exit */
  OA_CMD_RUN                 = 1, /* Run as normal */
  OA_CMD_GET_ALL_OPTIONS     = 2, /* Return all available options to OA */
  OA_CMD_GET_CURRENT_OPTIONS = 3, /* Return the option values currently set */
  OA_CMD_SET_OPTIONS         = 4, /* Persistantly set given options */
  OA_CMD_GET_BENCHMARKS      = 5, /* Return all known benchmark names to OA */
  OA_CMD_RUN_BENCHMARK       = 6, /* Run a given benchmark */
} oaCommandType;

typedef struct oaCommandStruct
{
  oaInt StructSize; /* Size in bytes of the whole struct */

  oaCommandType Type;
  const oaChar *BenchmarkName;  /* used for OA_CMD_RUN_BENCHMARK */
} oaCommand;


/******************************************************************************* 
 * Macros
 ******************************************************************************/

#define OA_RAISE_ERROR(error_type, message_str) \
  { \
    oaMessage Message; \
    oaInitMessage(&Message); \
    Message.Error = OA_ERROR_##error_type; \
    Message.Message = message_str; \
    oaSendSignal(OA_SIGNAL_ERROR, &Message); \
  }

#define OA_RAISE_WARNING(message_str) \
  { \
    oaMessage Message; \
    oaInitMessage(&Message); \
    Message.Error = OA_ERROR_WARNING; \
    Message.Message = message_str; \
    oaSendSignal(OA_SIGNAL_ERROR, &Message); \
  }

#define OA_RAISE_LOG(message_str) \
  { \
    oaMessage Message; \
    oaInitMessage(&Message); \
    Message.Error = OA_ERROR_LOG; \
    Message.Message = message_str; \
    oaSendSignal(OA_SIGNAL_ERROR, &Message); \
  }


/******************************************************************************* 
 * Functions
 ******************************************************************************/

/* Called when initializing OA mode.  init_str should be the string passed
   to the app as an option to the -openautomate command-line option */
oaBool oaInit(const oaChar *init_str, oaVersion *version);

/* Resets all values in the command to defaults */
void oaInitCommand(oaCommand *command);

/* Returns the next command for the app to execute.  If there are no commands
   left OA_CMD_EXIT will be returned. */
oaCommandType oaGetNextCommand(oaCommand *command);

/* Returns the next option for the app to set when in OA_CMD_SET_OPTIONS */
oaNamedOption *oaGetNextOption(void);

/* Resets all values in option to defaults */
void oaInitOption(oaNamedOption *option);

/* Adds an option to the option list when in OA_CMD_GET_ALL_OPTIONS */
void oaAddOption(const oaNamedOption *option);

/* Adds an option value to the option value list when in 
   OA_CMD_GET_CURRENT_OPTIONS */
void oaAddOptionValue(const oaChar *name, 
                      oaOptionDataType value_type,
                      const oaValue *value);

/* Adds a benchmark name to the list when in OA_CMD_GET_BENCHMARKS mode */
void oaAddBenchmark(const oaChar *benchmark_name);

/* Allows the application to send various signals.  Some signals may have 
   associated an associated parameter, passed in via the void *param.  See
   the the "Signals" section of the documentation for more info. Returns
   true if the signal was handled*/
oaBool oaSendSignal(oaSignalType signal, void *param);

/* Resets all values in option to defaults */
void oaInitMessage(oaMessage *message);

/******************************************************************************* 
 * Callback functions for benchmark mode
 ******************************************************************************/

/* The application should call this right before the benchmark starts.  It 
   should be called before any CPU or GPU computation is done for the first 
   frame. */
void oaStartBenchmark(void);

/* This should be called right before the final present call for each frame is 
   called. The t parameter should be set to the point in time the frame is 
   related to, in the application's time scale.*/
void oaDisplayFrame(oaFloat t);

/* Adds an optional result value from a benchmark run.  It can be called 
   multiple times, but 'name' must be different each time.  Also, it must be 
   called after the last call to oaDisplayFrame(), and before oaEndBenchmark() 
   */
void oaAddResultValue(const oaChar *name, 
                      oaOptionDataType value_type,
                      const oaValue *value);

/* Similar to oaAddResultValue(), but called per frame.  This call should be 
   made once for each value, before each call to oaDisplayFrame() */
void oaAddFrameValue(const oaChar *name, 
                     oaOptionDataType value_type,
                     const oaValue *value);

/* This should be called after the last frame is rendered in the benchmark */
void oaEndBenchmark(void);


#ifdef __cplusplus
}
#endif

#endif
