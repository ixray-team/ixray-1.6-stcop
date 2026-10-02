#include "stdafx.h"
#include "Expression.h"
#include <limits>
#include <charconv>
#include <cmath>

namespace
{
    template<typename T>
    bool ParseExpressionNumber(const xr_string& Text, T& Value)
    {
        const char* First = Text.data();
        const char* Last = First + Text.size();
        while (First != Last && isspace(static_cast<unsigned char>(*First)))
            ++First;
        while (First != Last && isspace(static_cast<unsigned char>(Last[-1])))
            --Last;
        if (First != Last && *First == '+')
            ++First;
        const auto Result = std::from_chars(First, Last, Value);
        return Result.ec == std::errc{} && Result.ptr == Last && std::isfinite(static_cast<double>(Value));
    }
}

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
    SXmlExpressionDelegate NewDelegate (Name, (void*)delegate, eDT_FLOAT);
    m_delegates.emplace(std::pair<int, SXmlExpressionDelegate>(NewDelegate.Id, NewDelegate));
}

void CExpressionManager::RegisterVariable(shared_str Name, GetIntFunc delegate)
{
    SXmlExpressionDelegate NewDelegate(Name, (void*)delegate, eDT_INT);
    m_delegates.emplace(std::pair<int, SXmlExpressionDelegate>(NewDelegate.Id, NewDelegate));
}

void CExpressionManager::RegisterVariable(shared_str Name, GetStringFunc delegate)
{
    SXmlExpressionDelegate NewDelegate(Name, (void*)delegate, eDT_STRING);
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

    if (FoundedDelegate != m_delegates.end() && FoundedDelegate->second.Func)
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

    Msg("* XML EXPRESSION: Cannot get variable id %d", Id);
    return ExpressionVarVariadic(0);
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
    m_expressionStrings = std::move(Other.m_expressionStrings);
    m_expression = Other.m_expression;
    Other.m_expression = nullptr;
    m_dbgCompileError = Other.m_dbgCompileError;
    Other.m_dbgCompileError = nullptr;
    m_expressionDataSize = Other.m_expressionDataSize;
    Other.m_expressionDataSize = 0;
}

CExpression::CExpression(const CExpression& Other)
{
    m_originalExpression = Other.m_originalExpression;
    m_expressionStrings = Other.m_expressionStrings;
    m_expressionDataSize = Other.m_expressionDataSize;
    m_expression = m_expressionDataSize ? new ExpressionData[m_expressionDataSize] : nullptr;
    if (m_expressionDataSize)
        memcpy(m_expression, Other.m_expression, m_expressionDataSize * sizeof(ExpressionData));
    if (Other.m_dbgCompileError != nullptr)
    {
        m_dbgCompileError = xr_strdup(Other.m_dbgCompileError);
    }
}

CExpression& CExpression::operator=(const CExpression& Other)
{
    if (this != &Other)
    {
        CExpression copy(Other);
        *this = std::move(copy);
    }
    return *this;
}

CExpression& CExpression::operator=(CExpression&& Other)
{
    if (this != &Other)
    {
        delete[] m_expression;
        xr_free(m_dbgCompileError);
        m_originalExpression = std::move(Other.m_originalExpression);
        m_expressionStrings = std::move(Other.m_expressionStrings);
        m_expression = Other.m_expression;
        Other.m_expression = nullptr;
        m_dbgCompileError = Other.m_dbgCompileError;
        Other.m_dbgCompileError = nullptr;
        m_expressionDataSize = Other.m_expressionDataSize;
        Other.m_expressionDataSize = 0;
    }
    return *this;
}

CExpression::~CExpression()
{
    xr_free(m_dbgCompileError);
    delete[] m_expression;
}

void CExpression::CompileExpression(xr_string& ExpressionStr, bool bAllowUnknowVariables /*= false*/)
{
    FlushCompileError();
    delete[] m_expression;
    m_expression = nullptr;
    m_expressionDataSize = 0;
    m_expressionStrings.clear();
    m_originalExpression = ExpressionStr;
    xr_string ClearedExpression = ExpressionStr;
   
    std::erase_if(ClearedExpression, [](unsigned char Ch) { return isspace(Ch) != 0; });

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
    // Function pointers must remain valid when more lexemes are appended.
    xr_deque<Lexema> ExpressionBody;

    xr_string WordAccumulator;
    u32       FunctionStackDepth = 0;

    xr_stack<Lexema*>  FunctionStack;
    FunctionStack.push(nullptr);

    auto DeclareVariableOrConstantIfNeccesseryFunc = [this, &ExpressionBody, &WordAccumulator, &FunctionStackDepth, &FunctionStack, bAllowUnknowVariables]()
    {
        if (WordAccumulator.empty()) return;

        Lexema* pLexem = nullptr;
        const u32 ParamIndex = g_uiExpressionMgr
            ? g_uiExpressionMgr->GetVariableIdByName(WordAccumulator.c_str())
            : CExpressionManager::INVALID_VARIABLE_INDEX;
        if (ParamIndex == CExpressionManager::INVALID_VARIABLE_INDEX)
        {
            string128 str = { 0 };
            //Check for string constant first
            if (WordAccumulator[0] == '\"')
            {
                if (WordAccumulator.size() < 2 || WordAccumulator[WordAccumulator.size() - 1] != '\"')
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
                float value = 0;
                if (!ParseExpressionNumber(WordAccumulator, value))
                {
                    SetCompileError("Invalid or out-of-range floating constant");
                    return;
                }
                pLexem = new Lexema(WordAccumulator, CONSTANT, UI_CONSTANT_FLOAT, FunctionStackDepth, FunctionStack.top());
                pLexem->fltConstant = value;
                goto FlushLexem;
            }

            if (IsValidIntConstantDeclaration(WordAccumulator))
            {
                int value = 0;
                if (!ParseExpressionNumber(WordAccumulator, value))
                {
                    SetCompileError("Invalid or out-of-range integer constant");
                    return;
                }
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
            if (strIter + 1 != ClearedExpression.end() && *(strIter + 1) == '=')
            {
				DeclareVariableOrConstantIfNeccesseryFunc();
				if (m_dbgCompileError != nullptr)
				{
					FailCompileWithReason();
					return;
				}
                ExpressionBody.emplace_back(Lexema("==", OPERATOR, UI_COMPAREEQUAL, FunctionStackDepth, FunctionStack.top()));
                ++strIter;
            }
            else
            {
                FailCompileWithReason("Expected == comparison operator");
                return;
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
    if (FunctionStackDepth != 0)
    {
        FailCompileWithReason("Missing closing parenthesis");
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

    auto EmitCodeVariable = [this, &ResultBytecode, bAllowUnknowVariables](Lexema& Lex, eVariableType& OutVariableDelegateType)
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
            Parameter.LongInt = m_expressionStrings.size();
            m_expressionStrings.emplace_back(Lex.Name.c_str());
        }
        else
        {
            Parameter.LongInt = Lex.VariableIndex;
        }
        ResultBytecode.push_back(Parameter.GetData());
    };

    auto EmitCodeConstant = [this, &ResultBytecode](Lexema& Lex, eVariableType& OutVariableDelegateType)
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
            Parameter.LongInt = m_expressionStrings.size();
            m_expressionStrings.emplace_back(Lex.Name.c_str());
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

    if (m_dbgCompileError != nullptr)
        return;

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
    static xr_string_map<xr_string, xr_string> DummyVariables;
    return ExecuteExpression(DummyVariables);
}

ExpressionVarVariadic CExpression::ExecuteExpression(const xr_string_map<xr_string, xr_string>& Variables)
{
    auto Fail = [this](const char* Reason)
    {
        Msg("* XML EXPRESSION \"%s\" FAILED TO EXECUTE: %s", m_originalExpression.c_str(), Reason);
        return ExpressionVarVariadic(0);
    };
    if (!m_expression || !m_expressionDataSize)
        return Fail("Expression is not compiled");

    xr_vector<ExpressionVarVariadic> Stack;
    Stack.reserve(32);
    xr_string_map<xr_string, ExpressionVarVariadic> ParsedVariables;
    ParseVariablesForExecution(Variables, ParsedVariables);

    using Type = ExpressionVarVariadic::EVariadicType;
    auto Number = [](const ExpressionVarVariadic& Value, double& Out)
    {
        switch (Value.VarType)
        {
        case Type::eInt: Out = Value.Int; return true;
        case Type::eUint: Out = Value.UInt; return true;
        case Type::eFloat: Out = Value.Flt; return true;
        case Type::eBool: Out = Value.Boolean ? 1 : 0; return true;
        default: return false;
        }
    };

    size_t Cursor = 0;
    while (Cursor < m_expressionDataSize)
    {
        const ExpressionData Instruction = m_expression[Cursor++];
        const u32 Opcode = static_cast<u32>(Instruction);
        if (Opcode == UI_NONE)
        {
            if (Stack.size() != 1)
                return Fail("Expected one result on the stack");
            return Stack.back();
        }

        if (Opcode >= UI_CONSTANT_INT && Opcode <= UI_VARIABLE_NAMED)
        {
            if (Cursor >= m_expressionDataSize)
                return Fail("Missing instruction parameter");
            const ExpressionData Parameter = m_expression[Cursor++];
            switch (Opcode)
            {
            case UI_CONSTANT_INT:
            {
                int Value;
                memcpy(&Value, &Parameter, sizeof(Value));
                Stack.emplace_back(Value);
                break;
            }
            case UI_CONSTANT_FLOAT:
            {
                float Value;
                memcpy(&Value, &Parameter, sizeof(Value));
                Stack.emplace_back(Value);
                break;
            }
            case UI_CONSTANT_STRING:
            case UI_VARIABLE_NAMED:
            {
                if (Parameter >= m_expressionStrings.size())
                    return Fail("Invalid string index");
                const char* Text = m_expressionStrings[static_cast<size_t>(Parameter)].c_str();
                if (Opcode == UI_CONSTANT_STRING)
                    Stack.emplace_back(Text);
                else
                {
                    auto Found = ParsedVariables.find(xr_string(Text));
                    if (Found == ParsedVariables.end())
                        return Fail("Named variable is not defined");
                    Stack.push_back(Found->second);
                }
                break;
            }
            case UI_VARIABLE:
                if (Parameter > static_cast<u64>(std::numeric_limits<int>::max()) ||
                    !g_uiExpressionMgr || !g_uiExpressionMgr->GetVariableDescById(static_cast<int>(Parameter)))
                    return Fail("Variable delegate is not defined");
                Stack.push_back(g_uiExpressionMgr->GetVariableById(static_cast<int>(Parameter)));
                break;
            }
            continue;
        }

        if (Opcode == UI_FLOOR || Opcode == UI_CEIL)
        {
            double Value;
            if (Stack.empty() || !Number(Stack.back(), Value))
                return Fail("Function requires a numeric operand");
            Stack.back() = ExpressionVarVariadic(static_cast<float>(Opcode == UI_FLOOR ? floor(Value) : ceil(Value)));
            continue;
        }

        if (Opcode != UI_ADD && Opcode != UI_SUBTRACT && Opcode != UI_MULTIPLE &&
            Opcode != UI_DIVIDE && Opcode != UI_COMPAREEQUAL)
            return Fail("Unknown instruction");
        if (Stack.size() < 2)
            return Fail("Operator requires two operands");

        const auto& Left = Stack[Stack.size() - 2];
        const auto& Right = Stack.back();
        ExpressionVarVariadic Result;
        if (Opcode == UI_COMPAREEQUAL && Left.VarType == Type::eStr && Right.VarType == Type::eStr)
        {
            Result = ExpressionVarVariadic(xr_strcmp(Left.Str.c_str() ? Left.Str.c_str() : "",
                Right.Str.c_str() ? Right.Str.c_str() : "") == 0);
        }
        else
        {
            double A, B;
            if (!Number(Left, A) || !Number(Right, B))
                return Fail("Operator requires compatible numeric operands");
            if (Opcode == UI_COMPAREEQUAL)
                Result = ExpressionVarVariadic(A == B);
            else
            {
                const bool Floating = Left.VarType == Type::eFloat || Right.VarType == Type::eFloat;
                double Value = 0;
                switch (Opcode)
                {
                case UI_ADD: Value = A + B; break;
                case UI_SUBTRACT: Value = A - B; break;
                case UI_MULTIPLE: Value = A * B; break;
                case UI_DIVIDE:
                    if (B == 0)
                        return Fail("Division by zero");
                    Value = Floating ? A / B : static_cast<int64_t>(A) / static_cast<int64_t>(B);
                    break;
                }
                if (Floating)
                    Result = ExpressionVarVariadic(static_cast<float>(Value));
                else if (Value >= std::numeric_limits<int>::min() && Value <= std::numeric_limits<int>::max())
                    Result = ExpressionVarVariadic(static_cast<int>(Value));
                else if ((Left.VarType == Type::eUint || Right.VarType == Type::eUint) &&
                    Value >= 0 && Value <= std::numeric_limits<u32>::max())
                    Result = ExpressionVarVariadic(static_cast<u32>(Value));
                else
                    return Fail("Integer arithmetic overflow");
            }
        }
        Stack.pop_back();
        Stack.back() = Result;
    }
    return Fail("Missing end instruction");
}

bool CExpression::IsCompiled() const
{
    return m_expression != nullptr;
}

void CExpression::ParseVariablesForExecution(const xr_string_map<xr_string, xr_string>& Variables, xr_string_map<xr_string, ExpressionVarVariadic>& OutVariables)
{
    for (const auto& [Name, Value] : Variables)
    {
        auto VariableDecl = Name.Split(' ');
        if (VariableDecl.size() != 2)
            continue;
        auto Type = VariableDecl[0];
        ExpressionVarVariadic Variable;
        if (Type == "int")
        {
            int Number = 0;
            if (!ParseExpressionNumber(Value, Number))
                continue;
            Variable = ExpressionVarVariadic(Number);
        }
        else if (Type == "u32" || Type == "u16")
        {
            u32 Number = 0;
            if (!ParseExpressionNumber(Value, Number) || (Type == "u16" && Number > 65535))
                continue;
            Variable = ExpressionVarVariadic(Number);
        }
        else if (Type == "float")
        {
            float Number = 0;
            if (!ParseExpressionNumber(Value, Number))
                continue;
            Variable = ExpressionVarVariadic(Number);
        }
        else if (Type == "xr_string")
        {
            Variable = ExpressionVarVariadic(Value.c_str());
        }
        else
            continue;

        OutVariables.emplace(std::make_pair(VariableDecl[1], Variable));
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
    xr_free(m_dbgCompileError);
    m_dbgCompileError = xr_strdup(reason);
}

void CExpression::FlushCompileError()
{
    xr_free(m_dbgCompileError);
    m_dbgCompileError = nullptr;
}
