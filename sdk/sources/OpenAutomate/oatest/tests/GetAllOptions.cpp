
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

typedef oacContext::OptionVec OptionVecType;
typedef map<string, OptionVecType *> OptionMapType;

static bool ValueIsNull(const oaValue &val)
{
  const char *Ptr = (const char *)&val;
  int i;

  for(i=0; i < sizeof(val); ++i)
    if(Ptr[i] != 0)
      return(false);

  return(true);
}

static bool ValuesAreEqual(const oaValue &a, 
                           const oaValue &b, 
                           oaOptionDataType type)
{
  switch(type)
  {
    case OA_TYPE_INT:
      return(a.Int == b.Int);

    case OA_TYPE_FLOAT:
      return(a.Float == b.Float);

    case OA_TYPE_BOOL:
      return(a.Bool == b.Bool);

    case OA_TYPE_STRING:
      return(!strcmp(a.String, b.String));

    case OA_TYPE_ENUM:
      return(!strcmp(a.Enum, b.Enum));
  }

  assert("Shouldn't be here!" == NULL);
  return(false);
}

static void CheckRangeAndSteps(const oaNamedOption &option)
{
  switch(option.DataType)
  {
    case OA_TYPE_STRING:
    case OA_TYPE_ENUM:
    case OA_TYPE_BOOL:
      OAC_TEST_MSG(option.NumSteps == -1,
        "NumSteps for string, enum, and bool options must be -1.  It's possible"
        " oaInitOption() was not called for option named '" << option.Name <<
        "'.");

      OAC_TEST_MSG(ValueIsNull(option.MinValue) && 
                   ValueIsNull(option.MaxValue),
        "MinValue and MaxValue for string, enum, and bool options must be "
        " zeroed out.  It's possible oaInitOption() was not called for option "
        "named '" << option.Name << "'.");
      break;

   case OA_TYPE_INT:
     if(option.NumSteps > 0)
     {
       oaInt Delta = option.MaxValue.Int - option.MinValue.Int;
       oaInt Div = Delta / option.NumSteps;
       OAC_TEST_MSG(Div * option.NumSteps + option.MinValue.Int == 
                    option.MaxValue.Int,
         "NumSteps must be evenly divisible into MaxValue - MinValue for "
         "integer option named '" << option.Name << "'")
     }

   case OA_TYPE_FLOAT:
     if(option.NumSteps >= 0)
     {
       OAC_TEST_MSG(!ValuesAreEqual(option.MinValue, 
                                    option.MaxValue, 
                                    option.DataType),
         "MinValue and MaxValue should not be equal if NumSteps >= 0 for "
         "option named '" << option.Name << "'.")
     }
     else
     {
       OAC_TEST_MSG(option.NumSteps == -1,
         "NumSteps must be >= -1.  Option named '" << option.Name << "' has "
         "NumSteps equal to " << option.NumSteps << ".")
     }

  }
}

static void CheckEnumOptionSanity(const string &name,
                                  const OptionVecType &option,
                                  const OptionMapType &map)
{
  assert(option.size() > 0);
  assert(option[0].DataType == OA_TYPE_ENUM);

  OAC_COND_WARN(option.size() > 1, 
   "Option named '" << name << "' of type ENUM has only one declared "
   "value.");

  OptionVecType::const_iterator Iter = option.begin();

  for(; Iter != option.end(); ++Iter)
  {
    OAC_TEST_MSG(Iter->DataType == OA_TYPE_ENUM,
      "Option named '" << name << "' is declared multiple times, but isn't "
      "always declared as ENUM type.")

    OAC_TEST_MSG(Iter->Value.Enum != NULL,
      "Value field must point to a valid string for option named '" << 
      name << "'.");

    CheckRangeAndSteps(*Iter);
  }
}
                              
static void CheckOptionSanity(const string &name,
                              const OptionVecType &option,
                              const OptionMapType &map)
{
  assert(option.size() > 0);

  OAC_TEST_MSG(oacOAUtils::IsValidType(option[0].DataType),
    "Option '" << name << "' added with oaAddOption() has an invalid type (" << 
     oacOAUtils::TypeToStr(option[0].DataType) << ").")

  switch(option[0].DataType)
  {
    case OA_TYPE_ENUM:
      CheckEnumOptionSanity(name, option, map);
      break;

    default:
      OAC_TEST_MSG(option.size() == 1,
        "Option named '" << name << "' is declared more than once.  This is"
        "only allowed for options of type ENUM.")

      OAC_TEST_MSG(ValueIsNull(option[0].Value),
        "Value field must be zeroed out for option named '" << name <<
        "'.  Either oaInitOption() was not called, or Value field was "
        "changed by mistake.");

      CheckRangeAndSteps(option[0]);
  }
}

static void CheckOptionSanity(const OptionMapType &map)
{
  OptionMapType::const_iterator Iter = map.begin();
  for(; Iter != map.end(); Iter++)
    CheckOptionSanity(Iter->first, *Iter->second, map);
}


static void CheckDependencies(oacContext *context)
{
  unsigned int NumEnabledOptions = 0;

  const map<string, oacContext::OptionVec *> &Options = 
    context->GetAllOptionsMap();
  const map<string, oacContext::OptionValue *> &CurOptions = 
    context->GetCurrentOptionsMap();

  map<string, oacContext::OptionVec *>::const_iterator Iter = Options.begin(); 
  for(; Iter != Options.end(); Iter++)
  {
    const oaNamedOption &Option = (*Iter->second)[0];
    oaOptionDependency Dep = Option.Dependency;

    if(Dep.ParentName == NULL)
    {
      OAC_TEST_MSG(Dep.ComparisonOp == OA_COMP_OP_INVALID,
        "Dependency.ComparisonOp field for option named '" << Option.Name << 
        "' must be set to OA_COMP_INVALID if "
        "Dependency.Parent is NULL.  This may be a problem with oaInitOption()"
        " not being used before setting values in the oaNamedOption struct.");

      OAC_TEST_MSG(ValueIsNull(Dep.ComparisonVal),
        "Dependency.ComparisonVal field for option named '" << Option.Name << 
        "' must be zeroed out if"
        "Dependency.Parent is NULL.  This may be a problem with oaInitOption()"
        " not being used before setting values in the oaNamedOption struct.");

      OAC_TEST_MSG(Dep.ComparisonValType == OA_TYPE_INVALID,
        "Dependency.ComparisonValType field for option named '" << Option.Name << 
        "' must be OA_TYPE_INVALID"
        "Dependency.Parent is NULL.  This may be a problem with oaInitOption()"
        " not being used before setting values in the oaNamedOption struct.");

      NumEnabledOptions++; // Options without dependencies are always enabled
    }
    else
    {
      OAC_TEST_MSG(Dep.ComparisonOp != OA_COMP_OP_INVALID,
        "Dependency.ComparisonOp field for option named '" << Option.Name <<
        "' must be set to a valid value.  If Dependency.Parent is not NULL.");

      OAC_TEST_MSG(Dep.ComparisonValType != OA_TYPE_INVALID,
        "Dependency.ComparisonValType field for option named '" << 
        Option.Name << "' must be set to a valid value.  If Dependency.Parent "
        "is not NULL.");

      if(Dep.ComparisonValType == OA_TYPE_ENUM || 
         Dep.ComparisonValType == OA_TYPE_STRING)
        OAC_TEST_MSG(!ValueIsNull(Dep.ComparisonVal),
          "Dependency.ComparisonVal for option named '" << Option.Name << 
          "'must be defined for string or enum types.")

      const oacContext::OptionVec *ParentOpt = 
        context->GetOption(Dep.ParentName);

      OAC_TEST_MSG(ParentOpt != NULL,
        "Option named '" << Option.Name << "' is dependent on a non-existent "
        "parent option named '" << Dep.ParentName << "'.")

      OAC_TEST_MSG((*ParentOpt)[0].DataType == Dep.ComparisonValType,
        "The Dependency.ComparisonTypeVal field of of option named '" <<
        Option.Name << "' does not match the type of its parent option named "
        "'" << Dep.ParentName << "'")

      if((*ParentOpt)[0].DataType == Dep.ComparisonValType)
      {
        OAC_TEST_MSG(oacOAUtils::IsValidDepCmpOp(Dep.ComparisonOp, 
                                                 Dep.ComparisonValType),
          "The comparison operator " << oacOAUtils::OpToStr(Dep.ComparisonOp) 
          << " for option named '" << Option.Name << "' "
          "is not valid valid for the parent option's type " << 
          oacOAUtils::TypeToStr(Dep.ComparisonValType) << ".");

        const oacContext::OptionValue *ParentValue = 
          context->GetOptionValue(Dep.ParentName);

        if(oacOAUtils::OptionIsEnabled(Dep.ComparisonOp,
                                       Dep.ComparisonValType,
                                       ParentValue->Value,
                                       Dep.ComparisonVal))
          NumEnabledOptions++;
      }
    }
  }


  OAC_TEST_MSG(CurOptions.size() >= NumEnabledOptions,
    "The number of options given by the application during the command "
    "OA_CMD_GET_CURRENT_OPTIONS (" << CurOptions.size() << ") is less than the"
    "number of enabled options defined by all the available options given by "
    "OA_CMD_GET_ALL_OPTIONS (" << NumEnabledOptions << ").");

  OAC_COND_WARN(CurOptions.size() == NumEnabledOptions,
    "The number of options given by the application during the command "
    "OA_CMD_GET_CURRENT_OPTIONS (" << CurOptions.size() << ") is greater than "
    "the number of enabled options (" << NumEnabledOptions << ").");

}
                         

OAC_SIMPLE_TEST_HEAD(GetAllOptions, 
                     "Basic sanity tests for OA_CMD_GET_ALL_OPTIONS",
                     1, 
                     true) 


  const vector<oaNamedOption> &Options = Context()->GetAllOptions();


  const OptionMapType &OptionMap = Context()->GetAllOptionsMap();

  const vector<OptionValue *> &CurOptions = Context()->GetCurrentOptions();

  OAC_COND_WARN(Options.size() > 0, 
   "There were no options returned by the application for command "
   "OA_CMD_GET_ALL_OPTIONS.  This may be a bug.");

  CheckOptionSanity(OptionMap);

  CheckDependencies(Context());

OAC_SIMPLE_TEST_TAIL

