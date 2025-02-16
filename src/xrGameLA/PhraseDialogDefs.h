#pragma once
//#include "boost/shared_ptr.hpp"

class CPhraseDialog;
//typedef boost::shared_ptr<CPhraseDialog>	DIALOG_SHARED_PTR;
typedef intrusive_ptr<CPhraseDialog>		DIALOG_SHARED_PTR;


//. typedef int PHRASE_ID;

//. typedef shared_str	PHRASE_DIALOG_ID;

using DIALOG_ID_VECTOR = xr_vector<shared_str>;
using DIALOG_ID_IT = DIALOG_ID_VECTOR::iterator;
//. #define  NO_PHRASE				-1
//. #define  START_PHRASE			0
//. #define  START_PHRASE_STR		"0"
#include "PhraseDialog.h"