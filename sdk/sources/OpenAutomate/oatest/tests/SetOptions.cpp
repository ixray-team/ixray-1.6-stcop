
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


#include <math.h>
#include <iostream>

#include <oac/TestBase.h>
#include <oac/OAUtils.h>

using namespace std;


OAC_TEST_DECLARE_CLASS_HEAD(SetOptions, 
                            "Basic sanity tests for OA_CMD_SET_OPTIONS",
                            -1, 
                            true) 


  typedef oacContext::OptionVec OptionVecType;
  typedef map<string, OptionVecType *> OptionMapType;
  typedef vector<oacContext::OptionValue> OptionValueVecType;
  typedef map<string, oacContext::OptionValue> OptionValueMapType;

  bool IsWithinRange(oaInt x, oaInt a, oaInt b)
  {
    if(a > b)
    {
      oaInt Tmp = b;
      b = a;
      a = Tmp;
    }

    return(x >= a && x <= b);
  }

  bool IsWithinRange(oaFloat x, oaFloat a, oaFloat b)
  {
    if(a > b)
    {
      oaFloat Tmp = b;
      b = a;
      a = Tmp;
    }

    return(x >= a && x <= b);
  }

  void GenRandInt(const oaNamedOption &option,
                  oaValue &val)
  {
    OAC_TEST_MSG(option.NumSteps >= -1,
     "NumSteps for integer options must be >= -1.  NumSteps for the option "
     "named '" << option.Name << "' is " << option.NumSteps);

    oaInt Delta = option.MaxValue.Int - option.MinValue.Int;
    oaInt NumSteps = option.NumSteps;

    if(NumSteps > 0)
    {
      OAC_COND_WARN((Delta / NumSteps) * NumSteps == Delta,
        "NumSteps is not evenly divisble into MaxValue - MinValue for option "
        "named '" << option.Name << "'");
    }

    switch(NumSteps)
    {
      case -1:
        val.Int = (oaInt)(Rand() * (double)0xFFFFFFFF);
        break;
    
      case 0:
        val.Int = (oaInt)(Rand() * (double)Delta) + option.MinValue.Int;
        break;

      default:
        if(NumSteps > 0)
          val.Int = (oaInt)(Rand() * (double)Delta / (double)NumSteps) * 
            NumSteps + option.MinValue.Int;

        assert(IsWithinRange(val.Int, 
                             option.MinValue.Int, 
                             option.MaxValue.Int));
    }

    OAC_MSG("Setting INT option '" << option.Name << "' -> " << val.Int);
  }

  void GenRandFloat(const oaNamedOption &option,
                    oaValue &val)
  {
    OAC_TEST_MSG(option.NumSteps >= -1,
     "NumSteps for float options must be >= -1.  NumSteps for the option "
     "named '" << option.Name << "' is " << option.NumSteps);

    oaFloat Delta = option.MaxValue.Float - option.MinValue.Float;
    oaInt NumSteps = option.NumSteps;

    switch(NumSteps)
    {
      case -1:
        val.Float = Rand() * 1000000 - 500000;
        break;
    
      case 0:
        val.Float = Rand() * Delta + option.MinValue.Float;
        break;

      default:
        if(NumSteps > 0)
        {
          oaFloat Step = floor(Rand() * ((oaFloat)NumSteps + 0.999));
          val.Float = 
            (oaFloat)Step * Delta / (oaFloat)NumSteps + option.MinValue.Float;

          assert(IsWithinRange(val.Float, 
                               option.MinValue.Float, 
                               option.MaxValue.Float));
        }
    }

    OAC_MSG("Setting FLOAT option '" << option.Name << "' -> " << val.Float);
  }

  void GenRandBool(const oaNamedOption &option,
                   oaValue &val)
  {
    val.Bool = (Rand() >= 0.5) ? OA_TRUE : OA_FALSE;
    OAC_MSG("Setting BOOL option '" << option.Name << "' -> " << val.Bool);
  }

  void GenRandEnum(const OptionVecType &option,
                   oaValue &val)
  {
    int Index = (int)(Rand() * ((double)option.size() - 0.001));
    val.Enum = option[Index].Value.Enum;
    OAC_TEST_MSG(val.Enum != NULL,
      "Options of type enum type must always have the Value field defined. "
      "Please check the call to oaAddOption() for the option named '" <<
      option[0].Name << "'")

    OAC_MSG("Setting ENUM option '" << option[0].Name << 
            "' -> " << val.Enum);
  }

  void GenRandString(const oaNamedOption &option,
                     oaValue &val)
  {
    val.String = Context()->GetOptionValue(option.Name)->Value.String;
    OAC_MSG("Keeping STRING option '" << option.Name << 
            "' -> " << val.String);
  }

  void GenRandOptionValue(const OptionVecType &option,
                          oacContext::OptionValue &ret)
  {
    assert(option.size() > 0);

    oaValue Value;

    switch(option[0].DataType)
    {
      case OA_TYPE_INT:
        GenRandInt(option[0], Value);
        break;

      case OA_TYPE_FLOAT:
        GenRandFloat(option[0], Value);
        break;

      case OA_TYPE_BOOL:
        GenRandBool(option[0], Value);
        break;

      case OA_TYPE_ENUM:
        GenRandEnum(option, Value);
        break;

      case OA_TYPE_STRING:
        GenRandString(option[0], Value);
        break;

      default:
        OAC_TEST("DATA TYPE NOT HANDLED" == NULL)
    }

    ret.Set(option[0].Name, option[0].DataType, &Value);
  }

  void GenRandOptionValues(const OptionMapType &options,
                           OptionValueVecType &ret)
  {
    ret.clear();

    OptionMapType::const_iterator OIter = options.begin();
    for(; OIter != options.end(); OIter++)
    {
      oacContext::OptionValue Val;
      GenRandOptionValue(*OIter->second, Val);
      ret.push_back(Val);
    }
  }

  bool StrsAreEqual(const char *s1, const char *s2)
  {
    return(strcmp(s1, s2) == 0 ? true : false);
  }

  void CompareOptionValue(const oacContext::OptionValue &orig_value)
  {
    const oacContext::OptionValue *CurOpt = 
      Context()->GetOptionValue(orig_value.Name);

    OAC_TEST_MSG(CurOpt != NULL,
      "Option named '" << orig_value.Name << "' wasn't set during command "
      "OA_CMD_SET_OPTIONS.")

    if(CurOpt != NULL)
    {
      switch(orig_value.Type)
      {
#define COMPARE_VAL(type, capital_type) \
        case OA_TYPE_##capital_type : \
          OAC_TEST_MSG(orig_value.Value.type == CurOpt->Value.type,\
            "Option '" << orig_value.Name << "' of type " #capital_type\
            " should be set to '" << orig_value.Value.type << "' but is "\
            "instead set to '" << CurOpt->Value.type << "'") \
          break;
        
        COMPARE_VAL(Int, INT)
        COMPARE_VAL(Float, FLOAT)
        COMPARE_VAL(Bool, BOOL)

#define COMPARE_STR_VAL(type, capital_type) \
        case OA_TYPE_##capital_type : \
          OAC_TEST_MSG(StrsAreEqual(orig_value.Value.type, CurOpt->Value.type),\
            "Option '" << orig_value.Name << "' of type " #capital_type\
            " should be set to '" << orig_value.Value.type << "' but is "\
            "instead set to " << CurOpt->Value.type) \
          break;
        
        COMPARE_STR_VAL(String, STRING)
        COMPARE_STR_VAL(Enum, ENUM)
      }
    }
  }

  void CompareOptionValues(const OptionValueVecType &orig_opts)
  {
    const std::vector<OptionValue *> &CurOpts = 
      Context()->GetCurrentOptions(true);

    size_t NumCurOptions = CurOpts.size();

    OAC_TEST_MSG(NumCurOptions == orig_opts.size(),
      "The number of current options (" << NumCurOptions << ") returned after "
      "the application was restarted doesn't match the number of options set "
      "before (" << orig_opts.size() << ") the restart.")

    vector<oacContext::OptionValue>::const_iterator Iter = orig_opts.begin();
    for(; Iter != orig_opts.end(); Iter++)
      CompareOptionValue(*Iter);
  }

  OptionValueVecType pRandOptions;
  OptionValueVecType::const_iterator pRandOptionsIter;

  virtual void Run(void)
  {
    pRandOptions.clear();
    
    GenRandOptionValues(Context()->GetAllOptionsMap(), 
                        pRandOptions);

    pRandOptionsIter = pRandOptions.begin();

    oaCommand Command;
    oaInitCommand(&Command);
    Command.Type = OA_CMD_SET_OPTIONS;
    Context()->IssueCommand(&Command);

    Context()->IssueExitAndRestart();

    CompareOptionValues(pRandOptions);
  }

  virtual oaNamedOption* GetNextOption(void)
  {
    static oaNamedOption Option;

    if(pRandOptionsIter == pRandOptions.end())
      return(NULL);

    const char *Name = pRandOptionsIter->Name;
    Option = (*Context()->GetOption(Name))[0];
    Option.Value = pRandOptionsIter->Value;

    pRandOptionsIter++;

    return(&Option);
  }

OAC_TEST_DECLARE_CLASS_TAIL(SetOptions)
