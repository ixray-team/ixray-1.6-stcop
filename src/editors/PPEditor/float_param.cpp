#include "stdafx.h"
#pragma hdrstop

#include "float_param.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)

#pragma resource "float_param.dfm"
TfrmTimeConstructor *frmTimeConstructor;
//---------------------------------------------------------------------------
__fastcall TfrmTimeConstructor::TfrmTimeConstructor(TComponent* Owner)
    : TForm(Owner)
{
    ClientHeight = Panel1->Height;
    
}
//---------------------------------------------------------------------------

