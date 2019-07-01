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

#ifndef _OAUtil_h
#define _OAUtil_h

#include "OpenAutomate.h"


#ifdef __cplusplus
extern "C"
{
#endif

/**********************************************************************
* Types
**********************************************************************/

typedef struct oauAppBuildIdStruct
{
  /* Name of the application developer (e.g. NVIDIA) */
  oaChar *DevName;

  /* Name of the application (e.g. MyGame) */
  oaChar *AppName;          

  /* Name of the application build (e.g. v1.2beta) */
  oaChar *AppBuildName;     
} oauAppBuildId;


typedef struct oauAppBuildInfoStruct
{
  /* The path of the root for the installation of the app */
  oaChar *InstallRootPath;  

  /* The full path to the entry executable to start OpenAutomate mode.  Can 
     optionally include command-line options, but should exclude the 
     -openautomate option */
  oaChar *EntryExe;

  /* The date/time the application was installed in IS08601 format */
  oaChar *InstallDateTime;

  /* A list of regions/languages supported by the build in RFC3066 format.  If
     multiple regions/languages are supported, they can be listed by separating
     them with the '|' character. */
  oaChar *Region;
} oauAppBuildInfo;

typedef enum
{
  /* Registers in the directory defined by the env variable OPENAUTOMATE_DIR */
  OAU_REGISTER_FILE_ENV        = 0x01, 

  /* Registers in the home directory */
  OAU_REGISTER_FILE_HOME       = 0x02, 

  /* Registers in a file system-wide  */
  OAU_REGISTER_FILE_SYSTEM     = 0x04, 

  /* Registers in the Windows registry under the user root key */
  OAU_REGISTER_REGISTRY_USER   = 0x08, 

  /* Registers in the Windows registry under the system root key */
  OAU_REGISTER_REGISTRY_SYSTEM = 0x0F
} oauAppBuildRegMethod;

typedef struct oauAppBuildRegEntryStruct
{
  oauAppBuildId *Id;
  oauAppBuildInfo *Info;
  oauAppBuildRegMethod Methods; 
} oauAppBuildRegEntry;

typedef struct oauAppBuildListStruct
{
  oauAppBuildRegEntry **AppBuilds;  /* Pointer to first appbuild */
  oaInt NumAppBuilds;               /* Number of appbuilds in the array */

  /* Internal.  Shouldn't be used directly */
  oaInt BufSize;                
} oauAppBuildList;

oauAppBuildId *oauAllocAppBuildId(void);
void oauFreeAppBuildId(oauAppBuildId *app_id);

oauAppBuildInfo *oauAllocAppBuildInfo(void);
void oauFreeAppBuildInfo(oauAppBuildInfo *app_info);

oauAppBuildRegEntry *oauAllocAppBuildRegEntry(void);
void oauFreeAppBuildRegEntry(oauAppBuildRegEntry *app_info);

oaBool oauRegisterAppBuild(oauAppBuildRegMethod method,
                           const oauAppBuildId *app_id,
                           const oauAppBuildInfo *app_info);

oaBool oauUnregisterAppBuild(oauAppBuildRegMethod method,
                             const oauAppBuildId *app_id);

oauAppBuildList *oauGetAllRegisteredAppBuilds(void);
void oauFreeAppBuildList(oauAppBuildList *appbuilds);

oaBool oauWriteRegistrationFile(const char *filename,
                                const oauAppBuildInfo *app_info);

oaBool oauReadRegistrationFile(const char *filename,
                               oauAppBuildInfo *app_info);

oaBool oauWriteRegistryKey(const char *root,
                           const oauAppBuildId *app_id,
                           const oauAppBuildInfo *app_info);

oaBool oauReadRegistryKey(const char *root,
                          const oauAppBuildId *app_id,
                          oauAppBuildInfo *app_info);


#ifdef __cplusplus
}
#endif

#endif


