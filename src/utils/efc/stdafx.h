////////////////////////////////////////////////////////////////////////////
//	Module 		: stdafx.h
//	Created 	: 16.12.2002
//  Modified 	: 16.12.2002
//	Author		: Dmitriy Iassenev
//	Description : Precompiled header
////////////////////////////////////////////////////////////////////////////

#pragma once

#include <io.h>
#include <math.h>
#include <memory.h>
#include <conio.h>
#include <stdio.h>
#include <time.h>
#include <windows.h>

#ifdef _DEBUG
	#define __ASSERT(bExpression)\
	{\
		if (!(bExpression))\
			printf("Assertion failed in module %s line %d\n",__FILE__,__LINE__);\
	}
#else
	#define __ASSERT(bExpression) ;
#endif

#define SQR(a) ((a)*(a))

#define CHECK_FILE_IF_OPENED(fFile,sName) if (!fFile) throw sName;

#define INI_FILE							"efc.ini"

#define LOG_DATA							"data\\%s\\efc.log"
#define TEXT_DATA							"data\\%s\\examples.txt"
#define BINARY_DATA							"data\\%s\\binary.dat"
#define CONFIG_DATA							"data\\%s\\configs.dat"
#define PATTERN_DATA						"data\\%s\\patterns.dat"
#define CORE_DATA							"data\\%s\\core.dat"
#define PARAMETERS_DATA						"data\\%s\\params.dat"
#define EF_DATA								"data\\%s\\ef.dat"

#define LOG_DATA_MASK_NAME					"LogData"
#define TEXT_DATA_MASK_NAME					"TextData"
#define BINARY_DATA_MASK_NAME				"BinaryData" 			
#define CONFIG_DATA_MASK_NAME				"ConfigData" 			
#define PATTERN_DATA_MASK_NAME				"PatternData" 		
#define CORE_DATA_MASK_NAME					"CoreData" 			
#define PARAMETERS_DATA_MASK_NAME			"ParametersData" 		
#define EF_DATA_MASK_NAME					"EFData" 				

#define EPSILON_NAME 						"Epsilon"
#define ALPHA_NAME   						"Alpha"
#define BETA_NAME							"Beta"
#define MAX_ITERATION_COUNT_NAME			"MaxIterationCount"
		
#define RANDOM_FACTOR_NAME 					"RandomFactor"
#define RANDOM_PROBABILITY_NAME				"RandomProbability"
#define RANDOM_UPDATE_NAME					"RandomUpdate"
#define RANDOM_START_SEED_NAME 				"RandomStartSeed"

#define MATCH_THRESHOLD_NAME				"MatchThreshold"
#define MAX_CARDINALITY_NAME				"MaxCardinality"

#define PATTERN_EXISTANCE_COEFFICIENT_NAME 	"PatternExistanceCoefficient"

////////////////////////////////////////////////////////////////////////////
// type declarations
////////////////////////////////////////////////////////////////////////////

typedef unsigned int uint;
typedef unsigned char uchar;
