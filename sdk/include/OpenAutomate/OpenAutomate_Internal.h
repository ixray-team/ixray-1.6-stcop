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

#ifndef _OA_Internal_h
#define _OA_Internal_h

#include "OpenAutomate.h"
#include <stdio.h>
#include <assert.h>

#ifdef __cplusplus
extern "C"
{
#endif

#define OA_ERROR(msg) \
  fprintf(stderr, "ERROR: %s\n", msg);

#define OA_ASSERT(cond) assert(cond)

#define OA_PLUGIN_INIT_FUNC "oaPluginInit"

#define OA_VERSION_MAJOR  0
#define OA_VERSION_MINOR  6
#define OA_VERSION_CUSTOM 0
#define OA_VERSION_BUILD  2

#define OA_INIT_VERSION_STRUCT(ver) \
         { \
         (ver).Major  = OA_VERSION_MAJOR; \
         (ver).Minor  = OA_VERSION_MINOR; \
         (ver).Custom = OA_VERSION_CUSTOM; \
         (ver).Build  = OA_VERSION_BUILD; \
         }

#ifdef WIN32
# define OA_FUNC_DECL __cdecl
#else
# define OA_FUNC_DECL
#endif

typedef struct oaiFunctionTableStruct
{
  size_t TableSize;

  oaCommandType (OA_FUNC_DECL *GetNextCommand)(oaCommand *command);
  oaNamedOption* (OA_FUNC_DECL *GetNextOption)(void);
  void (OA_FUNC_DECL *AddOption)(const oaNamedOption *option);
  void (OA_FUNC_DECL *AddOptionValue)(const oaChar *name,
                                      oaOptionDataType value_type,
                                      const oaValue *value);
  void (OA_FUNC_DECL *AddBenchmark)(const oaChar *benchmark_name);
  void (OA_FUNC_DECL *AddResultValue)(const oaChar *name, 
                                        oaOptionDataType value_type,
                                        const oaValue *value);
  void (OA_FUNC_DECL *StartBenchmark)(void);
  void (OA_FUNC_DECL *DisplayFrame)(oaFloat t);
  void (OA_FUNC_DECL *EndBenchmark)(void);
  void (OA_FUNC_DECL *AddFrameValue)(const oaChar *name, 
                                     oaOptionDataType value_type,
                                     const oaValue *value);
  oaBool (OA_FUNC_DECL *SendSignal)(oaSignalType signal, void *param);
} oaiFunctionTable;

typedef oaiFunctionTable *
(OA_FUNC_DECL *oaiPluginInitFunc)(const oaChar *init_str, 
                                  oaVersion *plugin_version, 
                                  const oaVersion harness_version);


/* Initializes the function table */
void oaiInitFuncTable(oaiFunctionTable *table);

/* Returns the current function table, presumably returned from the plugin 
   init*/
oaiFunctionTable *oaiGetCurrentFuncTable(void);

#ifdef __cplusplus
}
#endif

#endif
