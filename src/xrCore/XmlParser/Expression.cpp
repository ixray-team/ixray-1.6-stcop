#include "stdafx.h"
#include "Expression.h"

XRCORE_API CExpressionManager* g_uiExpressionMgr = nullptr;

enum ExpressionOptions
{
    EO_VARS_AS_INT,
    EO_VARS_AS_FLOAT,
    EO_VARS_AS_STRING,
};

struct ExpressionOpcode
{
    u32 OpcodeNum = 0;
    u32 Options = 0;

    ExpressionData GetData()
    {
        ExpressionData Result = OpcodeNum;
        Result |= u64(Options) << 32;
        return Result;
    }
};

void CExpressionManager::RegisterVariable(shared_str Name, GetFloatFunc delegate)
{
    SXmlExpressionDelegate NewDelegate (Name, delegate, eDT_FLOAT);
    m_delegates.emplace(std::pair<int, SXmlExpressionDelegate>(NewDelegate.Id, NewDelegate));
}

void CExpressionManager::RegisterVariable(shared_str Name, GetIntFunc delegate)
{
    SXmlExpressionDelegate NewDelegate(Name, delegate, eDT_INT);
    m_delegates.emplace(std::pair<int, SXmlExpressionDelegate>(NewDelegate.Id, NewDelegate));
}

void CExpressionManager::RegisterVariable(shared_str Name, GetStringFunc delegate)
{
    SXmlExpressionDelegate NewDelegate(Name, delegate, eDT_STRING);
    m_delegates.emplace(std::pair<int, SXmlExpressionDelegate>(NewDelegate.Id, NewDelegate));
}

u32 CExpressionManager::GetVariableIdByName(shared_str Name)
{
    auto DelegateIter = std::find_if(m_delegates.begin(), m_delegates.end(), [Name](auto& Elem) -> bool
    {
        return xr_strcmp(Elem.second.Name, Name) == 0;
    });

    if (DelegateIter != m_delegates.end())
    {
        return DelegateIter->second.Id;
    }

    return INVALID_VARIABLE_INDEX;
}

ExpressionVarVariadic CExpressionManager::GetVariableById(int Id)
{
    auto FoundedDelegate = m_delegates.find(Id);

    if (FoundedDelegate != m_delegates.end())
    {
        eVariableType Type = FoundedDelegate->second.Type;
        switch (Type)
        {
        case eDT_FLOAT:
        {
            GetFloatFunc FloatDelegate = (GetFloatFunc)FoundedDelegate->second.Func;
            float Value = FloatDelegate();
            return ExpressionVarVariadic(Value);
        }
            break;
        case eDT_INT:
        {
            GetIntFunc IntDelegate = (GetIntFunc)FoundedDelegate->second.Func;
            int Value = IntDelegate();
            return ExpressionVarVariadic(Value);
        }
            break;
        case eDT_BOOL:
        {
            GetBoolFunc BoolDelegate = (GetBoolFunc)FoundedDelegate->second.Func;
            bool Value = BoolDelegate();
            return ExpressionVarVariadic(Value);
        }
            break;
        case eDT_STRING:
        {
            GetStringFunc StringDelegate = (GetStringFunc)FoundedDelegate->second.Func;
            const char* Value = StringDelegate();
            return ExpressionVarVariadic(Value);
        }
            break;
        default:
            break;
        }
    }

    FATAL("XML EXPRESSION: Can't Get variable id '%d'"/*, Id*/);
    return ExpressionVarVariadic(42);
}

SXmlExpressionDelegate* CExpressionManager::GetVariableDescById(int Id)
{
    auto FoundedDelegate = m_delegates.find(Id);

    if (FoundedDelegate == m_delegates.end())
    {
        return nullptr;
    }

    return &FoundedDelegate->second;
}

u32 SXmlExpressionDelegate::IdGenerator = 0;

CExpression::CExpression()
    : m_expression(nullptr), m_dbgCompileError(nullptr)
{

}

CExpression::CExpression(CExpression&& Other)
{
    m_originalExpression = std::move(Other.m_originalExpression);
    m_expression = Other.m_expression;
    Other.m_expression = nullptr;
    m_dbgCompileError = Other.m_dbgCompileError;
    Other.m_dbgCompileError = nullptr;
    m_expressionDataSize = Other.m_expressionDataSize;
}

CExpression::CExpression(const CExpression& Other)
{
    m_originalExpression = Other.m_originalExpression;
    m_expression = new ExpressionData[Other.m_expressionDataSize];
    m_expressionDataSize = Other.m_expressionDataSize;
    memcpy(m_expression, Other.m_expression, m_expressionDataSize * sizeof(ExpressionData));
    if (Other.m_dbgCompileError != nullptr)
    {
        m_dbgCompileError = xr_strdup(Other.m_dbgCompileError);
    }
}

CExpression& CExpression::operator=(const CExpression& Other)
{
	m_originalExpression = Other.m_originalExpression;
	m_expression = new ExpressionData(Other.m_expressionDataSize);
	m_expressionDataSize = Other.m_expressionDataSize;
	memcpy(m_expression, Other.m_expression, m_expressionDataSize * sizeof(ExpressionData));
	m_dbgCompileError = xr_strdup(Other.m_dbgCompileError);

    return *this;
}

CExpression& CExpression::operator=(CExpression&& Other)
{
	m_originalExpression = std::move(Other.m_originalExpression);
	m_expression = Other.m_expression;
	Other.m_expression = nullptr;
	m_dbgCompileError = Other.m_dbgCompileError;
	Other.m_dbgCompileError = nullptr;
    m_expressionDataSize = Other.m_expressionDataSize;

    return *this;
}

CExpression::~CExpression()
{
    delete[] m_dbgCompileError;
    delete[] m_expression;
}

void CExpression::CompileExpression(xr_string& ExpressionStr, bool bAllowUnknowVariables /*= false*/)
{
    m_originalExpression = ExpressionStr;
    xr_string ClearedExpression = ExpressionStr;
   
    std::erase_if(ClearedExpression, isspace);

    enum WordPurpose
    {
        FUNCTION,
        VARIABLE,
        OPERATOR,
        CONSTANT
    };

    struct Lexema
    {
        xr_string Name;
        WordPurpose Purpose;
        ExpressionByteCode ByteCode;
        u32 VariableIndex = (u32)-1;

        float fltConstant = 0.0f;
        int   intConstant = 0;

        u32 StackDepth = 0;
        Lexema* pParentByStack = nullptr;

        Lexema(xr_string& InName, WordPurpose InPurpose, ExpressionByteCode InFunctionByteCode, u32 InStackDepth, Lexema* InParentByStack)
            : Name(InName), Purpose(InPurpose), ByteCode(InFunctionByteCode), StackDepth(InStackDepth), pParentByStack(InParentByStack)
        {}

        Lexema(const char InName[], WordPurpose InPurpose, ExpressionByteCode InFunctionByteCode, u32 InStackDepth, Lexema* InParentByStack)
            : Name(InName), Purpose(InPurpose), ByteCode(InFunctionByteCode), StackDepth(InStackDepth), pParentByStack(InParentByStack)
        {}
    };

    //first pass - validate instructions, variables, remember order, construct function stack levels
    xr_vector<Lexema> ExpressionBody;
    ExpressionBody.reserve(150);

    xr_string WordAccumulator;
    u32       FunctionStackDepth = 0;

    xr_stack<Lexema*>  FunctionStack;
    FunctionStack.push(nullptr);

    auto DeclareVariableOrConstantIfNeccesseryFunc = [this, &ExpressionBody, &WordAccumulator, &FunctionStackDepth, &FunctionStack, bAllowUnknowVariables]()
    {
        if (WordAccumulator.empty()) return;

        Lexema* pLexem = nullptr;
        u32 ParamIndex = g_uiExpressionMgr->GetVariableIdByName(WordAccumulator.c_str());
        if (ParamIndex == CExpressionManager::INVALID_VARIABLE_INDEX)
        {
            string128 str = { 0 };
            //Check for string constant first
            if (WordAccumulator[0] == '\"')
            {
                if (WordAccumulator[WordAccumulator.size() - 1] != '\"')
                {
					xr_sprintf(str, "'%s' is not a valid string constant declaration", WordAccumulator.c_str());
                    FailCompileWithReason(str);
                    return;
                }

                xr_string strConstant;
                strConstant.insert(strConstant.begin(), WordAccumulator.begin() + 1, WordAccumulator.end() - 1);
                pLexem = new Lexema(strConstant, CONSTANT, UI_CONSTANT_STRING, FunctionStackDepth, FunctionStack.top());
                goto FlushLexem;
            }

            //Check for float
            if (IsValidFloatConstantDeclaration(WordAccumulator))
            {
                float value = float(atof(WordAccumulator.c_str()));
                pLexem = new Lexema(WordAccumulator, CONSTANT, UI_CONSTANT_FLOAT, FunctionStackDepth, FunctionStack.top());
                pLexem->fltConstant = value;
                goto FlushLexem;
            }

            if (IsValidIntConstantDeclaration(WordAccumulator))
            {
                int value = atoi(WordAccumulator.c_str());
                pLexem = new Lexema(WordAccumulator, CONSTANT, UI_CONSTANT_INT, FunctionStackDepth, FunctionStack.top());
                pLexem->intConstant = value;
                goto FlushLexem;
            }

            if (!bAllowUnknowVariables)
            {
			    xr_sprintf(str, "'%s' is not a valid variable name", WordAccumulator.c_str());
                SetCompileError(str);
                return;
            }
        }

        if (bAllowUnknowVariables && ParamIndex == CExpressionManager::INVALID_VARIABLE_INDEX)
        {
			pLexem = new Lexema(WordAccumulator, VARIABLE, UI_VARIABLE_NAMED, FunctionStackDepth, FunctionStack.top());
        }
        else
        {
            pLexem = new Lexema(WordAccumulator, VARIABLE, UI_VARIABLE, FunctionStackDepth, FunctionStack.top());
        }
        pLexem->VariableIndex = ParamIndex;

        FlushLexem:
        ExpressionBody.push_back(*pLexem);
        //Drop word accum
        WordAccumulator.clear();
        delete pLexem;
    };

    auto DeclareFunctionIfNecessery = [this, &ExpressionBody, &WordAccumulator, &FunctionStackDepth, &FunctionStack]()
    {
        if (WordAccumulator.empty()) return;

        ExpressionByteCode FunctionByteCode = GetBytecodeByFunctionName(WordAccumulator);
        if (FunctionByteCode == UI_NONE)
        {
            string128 str = { 0 };
			xr_sprintf(str, "'%s' is not a function name", WordAccumulator.c_str());
            SetCompileError(str);
            return;
        }

        ExpressionBody.emplace_back(Lexema(WordAccumulator, FUNCTION, FunctionByteCode, ++FunctionStackDepth, FunctionStack.top()));
        FunctionStack.push(&ExpressionBody[ExpressionBody.size() - 1]);
        //Drop word accum
        WordAccumulator.clear();
    };

    for (auto strIter = ClearedExpression.begin(); strIter != ClearedExpression.end(); strIter++)
    {
        char ch = *strIter;

        switch (ch)
        {
        case '(': //start func
        {
            DeclareFunctionIfNecessery();
            if (m_dbgCompileError != nullptr) //function parsing failed
            {
                FailCompileWithReason();
                return;
            }
        }
            break;
        case ')': //end function
        {
            DeclareVariableOrConstantIfNeccesseryFunc();
            if (m_dbgCompileError != nullptr) //variable parsing failed
            {
                FailCompileWithReason();
                return; 
            }

            if (FunctionStackDepth == 0)
            {
                string128 str = { 0 };
				xr_sprintf(str, "Negative Function stack depth (expression have extra ')'). Did you forgot a '('?", WordAccumulator.c_str());
                FailCompileWithReason(str);
                return;
            }

            FunctionStack.pop();
            --FunctionStackDepth;
        }
            break;
        case ' ':
        {
            DeclareVariableOrConstantIfNeccesseryFunc();
            if (m_dbgCompileError != nullptr)
            {
                FlushCompileError();
                DeclareFunctionIfNecessery();
                if (m_dbgCompileError != nullptr)
                {
                    FlushCompileError();
                    string128 str = { 0 };
					xr_sprintf(str, "'%s' is not a function or variable name", WordAccumulator.c_str());
                    FailCompileWithReason(str);
                    return;
                }
            }
        }
            break;
            ///### OPERATORS
        case '+':
            DeclareVariableOrConstantIfNeccesseryFunc();
            if (m_dbgCompileError != nullptr)
            {
                FailCompileWithReason();
                return;
            }
            ExpressionBody.emplace_back(Lexema("+", OPERATOR, UI_ADD, FunctionStackDepth, FunctionStack.top()));
            break;
        case '-':
            DeclareVariableOrConstantIfNeccesseryFunc();
            if (m_dbgCompileError != nullptr)
            {
                FailCompileWithReason();
                return;
            }
            ExpressionBody.emplace_back(Lexema("-", OPERATOR, UI_SUBTRACT, FunctionStackDepth, FunctionStack.top()));
            break;
        case '=':
            if (*(strIter + 1) == '=')
            {
				DeclareVariableOrConstantIfNeccesseryFunc();
				if (m_dbgCompileError != nullptr)
				{
					FailCompileWithReason();
					return;
				}
                ExpressionBody.emplace_back(Lexema("==", OPERATOR, UI_COMPAREEQUAL, FunctionStackDepth, FunctionStack.top()));
            }
            break;
        case '/':
            DeclareVariableOrConstantIfNeccesseryFunc();
            if (m_dbgCompileError != nullptr)
            {
                FailCompileWithReason();
                return;
            }
            ExpressionBody.emplace_back(Lexema("/", OPERATOR, UI_DIVIDE, FunctionStackDepth, FunctionStack.top()));
            break;
        case '*':
            DeclareVariableOrConstantIfNeccesseryFunc();
            if (m_dbgCompileError != nullptr)
            {
                FailCompileWithReason();
                return;
            }
            ExpressionBody.emplace_back(Lexema("*", OPERATOR, UI_MULTIPLE, FunctionStackDepth, FunctionStack.top()));
            break;
        case '"':
            WordAccumulator.push_back(ch);
            break;
        default:
            //Allow only alphabet characters
            if ((!IsCharAlphaNumeric(ch)) && ch != '.')
            {
                string128 str = { 0 };
				xr_sprintf(str, "Character '%c' is not allowed in constant or variable declaration", ch);
                FailCompileWithReason(str);
                return;
            }
            WordAccumulator.push_back(ch);
            break;
        }
    }

    DeclareVariableOrConstantIfNeccesseryFunc();
    if (m_dbgCompileError != nullptr)
    {
        FailCompileWithReason();
        return;
    }
    /// **** SECOND PASS ****
    // second pass - emit code by go through lexem list

    xr_vector<ExpressionData> ResultBytecode;
    //Find the highest possible function stack, and go down
    struct FunctionPerStack
    {
        int StackDepth;
        Lexema* pFunction;

        FunctionPerStack(int InStackDepth, Lexema* InFunction)
            : StackDepth(InStackDepth), pFunction(InFunction)
        {}
    };

    xr_vector<FunctionPerStack> AllFunctionsPerStack;
    u32 MaxFunctionStack = 0;
    for (Lexema& Lex : ExpressionBody)
    {
        MaxFunctionStack = std::max(Lex.StackDepth, MaxFunctionStack);

        if (Lex.Purpose == FUNCTION)
        {
            AllFunctionsPerStack.emplace_back(FunctionPerStack(Lex.StackDepth, &Lex));
        }
    }

    auto FindAllFunctionsInStackLevelFunc = [&AllFunctionsPerStack](int StackDepth) -> xr_vector<FunctionPerStack>
    {
        xr_vector<FunctionPerStack> Result;

        for (FunctionPerStack& Func : AllFunctionsPerStack)
        {
            if (Func.StackDepth == StackDepth)
            {
                Result.push_back(Func);
            }
        }

        return Result;
    };

    auto FindAllLexemsInFunctionStackFunc = [&ExpressionBody](u32 FunctionStack, Lexema* pParent) -> xr_vector<Lexema>
    {
        xr_vector<Lexema> Result;
        for (Lexema& Lex : ExpressionBody)
        {
            if (Lex.StackDepth == FunctionStack && Lex.pParentByStack == pParent)
            {
                Result.push_back(Lex);
            }
        }

        return Result;
    };

    auto EmitCodeVariable = [&ResultBytecode, bAllowUnknowVariables](Lexema& Lex, eVariableType& OutVariableDelegateType)
    {
        R_ASSERT(Lex.Purpose == VARIABLE);

        if (bAllowUnknowVariables)
        {
            OutVariableDelegateType = eDT_UKNOWN;
        }
        else
        {
            SXmlExpressionDelegate* DelegateInfo = g_uiExpressionMgr->GetVariableDescById(Lex.VariableIndex);
            R_ASSERT(DelegateInfo);
            OutVariableDelegateType = DelegateInfo->Type;
        }
        ExpressionOpcode Opcode;
        Opcode.OpcodeNum = Lex.ByteCode;
        ResultBytecode.push_back(Opcode.GetData());

        ExpressionVarVariadic Parameter;
        if (Lex.ByteCode == UI_VARIABLE_NAMED)
        {
            Parameter.Str = Lex.Name.c_str();
        }
        else
        {
            Parameter.LongInt = Lex.VariableIndex;
        }
        ResultBytecode.push_back(Parameter.GetData());
    };

    auto EmitCodeConstant = [&ResultBytecode](Lexema& Lex, eVariableType& OutVariableDelegateType)
    {
        ExpressionOpcode Opcode;
        Opcode.OpcodeNum = Lex.ByteCode;
        ResultBytecode.push_back(Opcode.GetData());

        if (Lex.ByteCode == UI_CONSTANT_FLOAT)
        {
            ExpressionVarVariadic Parameter;
            Parameter.Flt = Lex.fltConstant;
            OutVariableDelegateType = eVariableType::eDT_FLOAT;
            ResultBytecode.push_back(Parameter.GetData());
        }
        else if (Lex.ByteCode == UI_CONSTANT_INT)
        {
            ExpressionVarVariadic Parameter;
            Parameter.Int = Lex.intConstant;
            OutVariableDelegateType = eVariableType::eDT_INT;
            ResultBytecode.push_back(Parameter.GetData());
        }
        else if (Lex.ByteCode == UI_CONSTANT_STRING)
        {
            ExpressionVarVariadic Parameter;
            Parameter.Str = Lex.Name.c_str();
            OutVariableDelegateType = eVariableType::eDT_STRING;
            ResultBytecode.push_back(Parameter.GetData());
        }
    };

    eVariableType VarType = eDT_INT;

    auto EmitAllLexemToBytecode = [this, &EmitCodeConstant, &EmitCodeVariable, &ResultBytecode, &VarType](xr_vector<Lexema>& LexemList)
    {
        for (auto LexIter = LexemList.begin(); LexIter != LexemList.end(); ++LexIter)
        {
            Lexema& Lex = *LexIter;

            switch (Lex.Purpose)
            {
            case VARIABLE:
            {
                EmitCodeVariable(Lex, VarType);
            }
            break;
            case OPERATOR:
            {
                //If operator placed between variable/constant, we should emit that variable/constant first
                auto NextLexIter = LexIter + 1;
                if (NextLexIter == LexemList.end())
                {
                    string128 str = { 0 };
                    xr_sprintf(str, "You should place variable/constant/function after operator");
                    FailCompileWithReason(str);
                    return;
                }

                Lexema& NextLex = *NextLexIter;
                if (NextLex.Purpose == VARIABLE || NextLex.Purpose == CONSTANT)
                {
                    if (NextLex.Purpose == VARIABLE)
                    {
                        EmitCodeVariable(NextLex, VarType);
                    }
                    else if (NextLex.Purpose == CONSTANT)
                    {
                        EmitCodeConstant(NextLex, VarType);
                    }
                    ++LexIter;
                }

                ExpressionOpcode Opcode;
                Opcode.OpcodeNum = Lex.ByteCode;
                switch (VarType)
                {
                case eDT_FLOAT:
                    Opcode.Options = EO_VARS_AS_FLOAT;
                    break;
                case eDT_INT:
                    Opcode.Options = EO_VARS_AS_INT;
                    break;
                case eDT_BOOL:
                    Opcode.Options = EO_VARS_AS_INT;
                    break;
                case eDT_STRING:
                    Opcode.Options = EO_VARS_AS_STRING;
                    break;
                }

                ResultBytecode.push_back(Opcode.GetData());
            }
            break;
            case CONSTANT:
            {
                EmitCodeConstant(Lex, VarType);
            }
            break;
            case FUNCTION:
            default:
                FATAL("Lexema purpose is unknown");
                break;
            }
        }
    };

    //process functions stack levels, from higher to lower
    for (u32 StackLevel = MaxFunctionStack; StackLevel > 0; --StackLevel)
    {
        xr_vector<FunctionPerStack> AllFunctionsInStack = FindAllFunctionsInStackLevelFunc(StackLevel);

        for (FunctionPerStack& Func : AllFunctionsInStack)
        {
            xr_vector<Lexema> AllLexemsInFunction = FindAllLexemsInFunctionStackFunc(StackLevel, Func.pFunction);

            EmitAllLexemToBytecode(AllLexemsInFunction);
            //Emit function
            ExpressionOpcode Opcode;
            Opcode.OpcodeNum = Func.pFunction->ByteCode;
            Opcode.Options = EO_VARS_AS_FLOAT;
            ResultBytecode.push_back(Opcode.GetData());
        }
    }

    //Emit code for zero level lexems (not a function stack level)
    xr_vector<Lexema> TopLexems = FindAllLexemsInFunctionStackFunc(0, nullptr);
    EmitAllLexemToBytecode(TopLexems);

    //Emit zero bytecode to mark end
    ExpressionOpcode TrailOpcode;
    ResultBytecode.push_back(TrailOpcode.GetData());

    //Flush bytecode
    m_expression = new ExpressionData[ResultBytecode.size()];
    m_expressionDataSize = ResultBytecode.size();
    memcpy(&m_expression[0], ResultBytecode.data(), ResultBytecode.size() * sizeof(ExpressionData));
}

ExpressionVarVariadic CExpression::ExecuteExpression()
{
    xr_string_map<xr_string, xr_string> DummyVariables;
    return ExecuteExpression(DummyVariables);
}

ExpressionVarVariadic CExpression::ExecuteExpression(const xr_string_map<xr_string, xr_string>& Variables)
{
    bool bWithoutVariables = Variables.empty();
	ExpressionVarVariadic stack[32];

    xr_string_map<xr_string, ExpressionVarVariadic> ParsedVariables;
    if (!bWithoutVariables)
    {
        ParseVariablesForExecution(Variables, ParsedVariables);
    }

	int StackCursor = 0;
	u64* CodeCursor = m_expression;
	ExpressionVarVariadic* CodeParam = nullptr;

	//while (ExpressionOpcode bytecode = (ExpressionOpcode)*CodeCursor)
	while (true)
	{
		ExpressionOpcode bytecode = *(ExpressionOpcode*)CodeCursor;
		if (bytecode.OpcodeNum == 0) break;
		++CodeCursor; //set position to a variable or next command
		switch (bytecode.OpcodeNum)
		{
		case UI_CONSTANT_STRING:
		case UI_CONSTANT_INT:
		case UI_CONSTANT_FLOAT:
			CodeParam = (ExpressionVarVariadic*)CodeCursor;
			stack[StackCursor++] = *CodeParam;
			++CodeCursor; //we use a parameter, set cursor to next command
			break;
		case UI_VARIABLE:
			CodeParam = (ExpressionVarVariadic*)CodeCursor;
			stack[StackCursor++] = g_uiExpressionMgr->GetVariableById(CodeParam->Int);
			++CodeCursor; //we use a parameter, set cursor to next command
			break;
        case UI_VARIABLE_NAMED:
            CodeParam = (ExpressionVarVariadic*)CodeCursor;
            stack[StackCursor++] = ParsedVariables[CodeParam->Str.c_str()];
            ++CodeCursor; //we use a parameter, set cursor to next command
            break;
		case UI_ADD:
			if (bytecode.Options == EO_VARS_AS_INT)
			{
				stack[StackCursor - 2].Int = stack[StackCursor - 2].Int + stack[StackCursor - 1].Int;
				--StackCursor;
			}
			else if (bytecode.Options == EO_VARS_AS_FLOAT)
			{
				stack[StackCursor - 2].Flt = stack[StackCursor - 2].Flt + stack[StackCursor - 1].Flt;
				--StackCursor;
			}
			break;
		case UI_SUBTRACT:
			if (bytecode.Options == EO_VARS_AS_INT)
			{
				stack[StackCursor - 2].Int = stack[StackCursor - 2].Int - stack[StackCursor - 1].Int;
				--StackCursor;
			}
			else if (bytecode.Options == EO_VARS_AS_FLOAT)
			{
				stack[StackCursor - 2].Flt = stack[StackCursor - 2].Flt - stack[StackCursor - 1].Flt;
				--StackCursor;
			}
			break;
		case UI_MULTIPLE:
			if (bytecode.Options == EO_VARS_AS_INT)
			{
				stack[StackCursor - 2].Int = stack[StackCursor - 2].Int * stack[StackCursor - 1].Int;
				--StackCursor;
			}
			else if (bytecode.Options == EO_VARS_AS_FLOAT)
			{
				stack[StackCursor - 2].Flt = stack[StackCursor - 2].Flt * stack[StackCursor - 1].Flt;
				--StackCursor;
			}
			break;
		case UI_DIVIDE:
			if (bytecode.Options == EO_VARS_AS_INT)
			{
				stack[StackCursor - 2].Int = stack[StackCursor - 2].Int / stack[StackCursor - 1].Int;
				--StackCursor;
			}
			else if (bytecode.Options == EO_VARS_AS_FLOAT)
			{
				stack[StackCursor - 2].Flt = stack[StackCursor - 2].Flt / stack[StackCursor - 1].Flt;
				--StackCursor;
			}
			break;
        case UI_COMPAREEQUAL:
            stack[StackCursor - 2].Boolean = stack[StackCursor - 2].Int / stack[StackCursor - 1].Int;
            --StackCursor;
            break;
		case UI_FLOOR:
			stack[StackCursor - 1].Flt = floor(stack[StackCursor - 1].Flt);
			break;
		case UI_CEIL:
			stack[StackCursor - 1].Flt = ceil(stack[StackCursor - 1].Flt);
			break;
		case UI_NONE:
			break;
		default:
			FATAL("Unknown expression opcode, stack can be corrupted!");
			break;
		}
	}

	R_ASSERT(StackCursor == 1);
	return stack[0];
}

bool CExpression::IsCompiled() const
{
    return m_expression != nullptr;
}

#include <regex>

std::vector<std::string> split(const xr_string& input, const xr_string& regex) 
{
    // passing -1 as the submatch index parameter performs splitting
    std::regex re(regex.c_str());
    std::sregex_token_iterator
        first{ input.begin(), input.end(), re, -1 },
        last;

    return { first, last };
}

void CExpression::ParseVariablesForExecution(const xr_string_map<xr_string, xr_string>& Variables, xr_string_map<xr_string, ExpressionVarVariadic>& OutVariables)
{
    for (const auto& [Name, Value] : Variables)
    {
        auto VariableDecl = split(Name, " ");
        auto Type = VariableDecl[0];
        ExpressionVarVariadic Variable;
        if (Type.starts_with("int"))
        {
            Variable.Int = atoi(Value.c_str());
        }
        else if (Type.starts_with("u32"))
        {
            sscanf(Value.c_str(), "%u", &Variable.UInt);
        }
		else if (Type.starts_with("float"))
		{
			sscanf(Value.c_str(), "%f", &Variable.Flt);
		}
		else if (Type.starts_with("u16"))
		{
			sscanf(Value.c_str(), "%u", &Variable.UInt);
		}
		else if (Type.starts_with("xr_string"))
		{
            Variable.Str = Value.c_str();
		}

        OutVariables.emplace(std::make_pair(Name, Variable));
    }
}

ExpressionByteCode CExpression::GetBytecodeByFunctionName(xr_string& FunctionName)
{
    if (FunctionName == "floor")
    {
        return UI_FLOOR;
    }

    if (FunctionName == "ceil")
    {
        return UI_CEIL;
    }

    return UI_NONE;
}

void CExpression::FailCompileWithReason(xr_string& reason) const
{
    FailCompileWithReason(reason.c_str());
}

void CExpression::FailCompileWithReason(const char* reason) const
{
    SetCompileError(reason);
    FailCompileWithReason();
}

void CExpression::FailCompileWithReason() const
{
    Msg("* XML EXPRESSION \"%s\" FAILED TO COMPILE: %s", m_originalExpression.c_str(), m_dbgCompileError);
}

bool CExpression::IsValidFloatConstantDeclaration(xr_string& LexemStr) const
{
    //A number with a dot
    //Dot is required
    bool bHaveDot = false;

    for (char ch : LexemStr)
    {
        if (!isdigit(ch))
        {
            if (ch == '.')
            {
                if (bHaveDot == true)
                {
                    string128 str = { 0 };
					xr_sprintf(str, "Double dot in numeric constant declaration: %s", LexemStr.c_str());
                    FailCompileWithReason(str);
                    return false;
                }
                bHaveDot = true;
                continue;
            }

            //allow minus
            if (ch == '-') continue;

            return false;
        }
    }

    return bHaveDot;
}

bool CExpression::IsValidIntConstantDeclaration(xr_string& LexemStr) const
{
    //it's just a number 
    for (char ch : LexemStr)
    {
        if (!isdigit(ch))
        {
            //allow minus
            if (ch == '-') continue;
            return false;
        }
    }

    return true;
}

void CExpression::SetCompileError(const char* reason) const
{
    m_dbgCompileError = xr_strdup(reason);
}

void CExpression::FlushCompileError()
{
    delete m_dbgCompileError;
    m_dbgCompileError = nullptr;
}
