
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

#include <oac/TestBase.h>
#include <oac/OAUtils.h>

using namespace std;

OAC_SIMPLE_TEST_HEAD(GetCurrentOptions, 
                     "Basic sanity tests for OA_CMD_GET_Current_OPTIONS",
                     -1, 
                     true) 


  const vector<oacContext::OptionValue *> &Options = 
    Context()->GetCurrentOptions();

  map<string, const oacContext::OptionValue *> OptionsMap;


  vector<oacContext::OptionValue *>::const_iterator Iter = Options.begin();
  for(; Iter != Options.end(); Iter++)
  {
    string Name((*Iter)->Name);

    OAC_TEST_MSG(OptionsMap.find(Name) == OptionsMap.end(),
      "oaAddOptionValue() was called for an option named '" << Name <<
      "' more than once.");

    OptionsMap[Name] = Context()->GetOptionValue(Name);

    OAC_TEST_MSG(oacOAUtils::IsValidType((*Iter)->Type),
      "Option '" << Name << "' added with oaAddOptionValue() has an "
      "invalid type (" << oacOAUtils::TypeToStr((*Iter)->Type) << ").")

    const oacContext::OptionVec *Option = Context()->GetOption(Name);
    OAC_TEST_MSG(Option != NULL,
      "oaAddOptionValue() was called for an option named '" << Name << 
      "', but oaAddOption() was never called with an option of the same name "
      "during the last issue of command OA_CMD_GET_ALL_OPTIONS.")

  }


OAC_SIMPLE_TEST_TAIL

