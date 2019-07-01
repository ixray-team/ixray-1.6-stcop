
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
#include <string.h>
#include <oac/OAUtils.h>



const char *oacOAUtils::TypeToStr(oaOptionDataType type, bool upper_case)
{
  switch(type)
  {
#define OAC_TYPE_MACRO(type, lc_type) \
     case OA_TYPE_##type: \
       return(upper_case ? #type : #lc_type);

#include <oac/OAUtils_types.h>
  }

  assert("Shouldn't be here!!!" == NULL);
  return("UNKNOWN");
}

bool oacOAUtils::IsValidType(oaOptionDataType type)
{
  if(type == OA_TYPE_INVALID)
    return(false);

  switch(type)
  {
#define OAC_TYPE_MACRO(type, lc_type) \
     case OA_TYPE_##type: \
       return(true);

#include <oac/OAUtils_types.h>
  }

  return(false);
}

const char *oacOAUtils::OpToStr(oaComparisonOpType op)
{
  switch(op)
  {
#define OAC_OP_MACRO(op) \
     case op : \
       return(#op);

#include <oac/OAUtils_ops.h>
  }

  assert("Shouldn't be here!!!" == NULL);
  return("UNKNOWN");
}

bool oacOAUtils::IsValidOp(oaOptionDataType type)
{
  if(type == OA_COMP_OP_INVALID)
    return(false);

  switch(type)
  {
#define OAC_OP_MACRO(op) \
     case op : \
       return(true);

#include <oac/OAUtils_ops.h>
  }

  return(false);
}

bool oacOAUtils::IsValidDepCmpOp(oaComparisonOpType op, oaOptionDataType type)
{
  assert(type != OA_TYPE_INVALID);

  switch(type)
  {
    case OA_TYPE_ENUM:
    case OA_TYPE_STRING:
    case OA_TYPE_BOOL:
      switch(op)
      {
        case OA_COMP_OP_EQUAL:
        case OA_COMP_OP_NOT_EQUAL:
          return(true);

        default:
          return(false);
      }
      break;

    case OA_TYPE_INT:
    case OA_TYPE_FLOAT:
      switch(op)
      {
        case OA_COMP_OP_EQUAL:
        case OA_COMP_OP_NOT_EQUAL:
        case OA_COMP_OP_GREATER:
        case OA_COMP_OP_LESS:
        case OA_COMP_OP_GREATER_OR_EQUAL:
        case OA_COMP_OP_LESS_OR_EQUAL:
          return(true);
  
        default:
          return(false);
      };
  }

  return(false);
}

static bool OptionIsEnabled_INVALID(oaComparisonOpType op,
                                    const oaValue &parent_value,
                                    const oaValue &comparison_value)
{
  assert("This should never be called!" == NULL);
  return(false);
}

static bool StrsAreEqual(const char *s1, const char *s2)
{
  if((s1 == NULL) && (s2 == NULL))
    return(true);

  if((s1 == NULL) ^ (s2 == NULL))
    return(false);

  return(strcmp(s1, s2) ? false : true);
}

static bool OptionIsEnabled_STRING(oaComparisonOpType op,
                                   const oaValue &parent_value,
                                   const oaValue &comparison_value)
{
  switch(op)
  {
    case OA_COMP_OP_EQUAL:
      return(StrsAreEqual(parent_value.String, comparison_value.String));

    case OA_COMP_OP_NOT_EQUAL:
      return(!StrsAreEqual(parent_value.String, comparison_value.String));
  }

  assert("Invalid operation on string type" == NULL);
  return(false);
}

static bool OptionIsEnabled_ENUM(oaComparisonOpType op,
                                 const oaValue &parent_value,
                                 const oaValue &comparison_value)
{
  switch(op)
  {
    case OA_COMP_OP_EQUAL:
      return(StrsAreEqual(parent_value.Enum, comparison_value.Enum));

    case OA_COMP_OP_NOT_EQUAL:
      return(!StrsAreEqual(parent_value.Enum, comparison_value.Enum));
  }

  assert("Invalid operation on enum type" == NULL);
  return(false);
}

static bool OptionIsEnabled_BOOL(oaComparisonOpType op,
                                 const oaValue &parent_value,
                                 const oaValue &comparison_value)
{
  switch(op)
  {
    case OA_COMP_OP_EQUAL:
      return(parent_value.Bool == comparison_value.Bool);

    case OA_COMP_OP_NOT_EQUAL:
      return(parent_value.Bool != comparison_value.Bool);
  }

  assert("Invalid operation on bool type" == NULL);
  return(false);
}

static bool OptionIsEnabled_INT(oaComparisonOpType op,
                                 const oaValue &parent_value,
                                 const oaValue &comparison_value)
{
  switch(op)
  {
    case OA_COMP_OP_EQUAL:
      return(parent_value.Int == comparison_value.Int);

    case OA_COMP_OP_NOT_EQUAL:
      return(parent_value.Int != comparison_value.Int);

    case OA_COMP_OP_GREATER:
      return(parent_value.Int > comparison_value.Int);

    case OA_COMP_OP_LESS:
      return(parent_value.Int < comparison_value.Int);

    case OA_COMP_OP_GREATER_OR_EQUAL:
      return(parent_value.Int >= comparison_value.Int);

    case OA_COMP_OP_LESS_OR_EQUAL:
      return(parent_value.Int <= comparison_value.Int);
  }

  assert("Invalid operation on bool type" == NULL);
  return(false);
}

static bool OptionIsEnabled_FLOAT(oaComparisonOpType op,
                                 const oaValue &parent_value,
                                 const oaValue &comparison_value)
{
  switch(op)
  {
    case OA_COMP_OP_EQUAL:
      return(parent_value.Float == comparison_value.Float);

    case OA_COMP_OP_NOT_EQUAL:
      return(parent_value.Float != comparison_value.Float);

    case OA_COMP_OP_GREATER:
      return(parent_value.Float > comparison_value.Float);

    case OA_COMP_OP_LESS:
      return(parent_value.Float < comparison_value.Float);

    case OA_COMP_OP_GREATER_OR_EQUAL:
      return(parent_value.Float >= comparison_value.Float);

    case OA_COMP_OP_LESS_OR_EQUAL:
      return(parent_value.Float <= comparison_value.Float);
  }

  assert("Invalid operation on bool type" == NULL);
  return(false);
}
bool oacOAUtils::OptionIsEnabled(oaComparisonOpType op,
                                 oaOptionDataType type,
                                 const oaValue &parent_value,
                                 const oaValue &comparison_value)
{
  assert(IsValidDepCmpOp(op, type));

  switch(type)
  {
#   define OAC_TYPE_MACRO(type_name, lc_type) \
      case OA_TYPE_##type_name : \
        return(OptionIsEnabled_##type_name (op, parent_value, comparison_value));

#   include <oac/OAUtils_types.h>
  }

  assert("Shouldn't be here!" == NULL);

  return(false);
}


