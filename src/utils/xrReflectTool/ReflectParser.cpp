// Copyright (c) 2026 IX-Ray and contributors. All rights reserved.
//
// This file is part of the IX-Ray engine and is licensed for use only within
// the IX-Ray project (https://github.com/ixray-team). The project-wide license
// in LICENSE.md does not apply to this file.
//
// You may use, modify and distribute this file only as part of IX-Ray or of
// a game or modification built on IX-Ray. Using, copying or adapting this file,
// or any part of it, in any other project is prohibited without prior written
// permission from the IX-Ray team.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY ARISING FROM THE USE OF THIS FILE.

#include "ReflectParser.h"

#include <algorithm>
#include <map>
#include <set>
#include <sstream>

namespace XReflectTool
{
	namespace
	{
		enum class ETokenKind
		{
			Identifier,
			Number,
			String,
			Punct
		};

		struct Token
		{
			ETokenKind Kind;
			std::string Text;
			int Line;
			int Condition;
		};

		bool IsWordToken(const Token& Tok)
		{
			return Tok.Kind == ETokenKind::Identifier || Tok.Kind == ETokenKind::Number;
		}

		bool IsIdentStart(char C)
		{
			return (C >= 'a' && C <= 'z') || (C >= 'A' && C <= 'Z') || C == '_';
		}

		bool IsIdentChar(char C)
		{
			return IsIdentStart(C) || (C >= '0' && C <= '9');
		}

		// Tracks #if / #ifdef / #ifndef / #elif / #else / #endif so every token knows the
		// preprocessor condition it sits under. Include guards are dropped.
		class ConditionTracker
		{
		public:
			std::vector<std::string> Table = { "" };

			int Current() const
			{
				return CurrentIndex;
			}

			void OnDirective(const std::string& Directive, size_t TokenCount)
			{
				size_t KeyEnd = 0;
				while (KeyEnd < Directive.size() && IsIdentChar(Directive[KeyEnd]))
				{
					++KeyEnd;
				}
				std::string Key = Directive.substr(0, KeyEnd);
				std::string Arg = Trim(Directive.substr(KeyEnd));

				if (!Frames.empty() && Frames.back().GuardCandidate)
				{
					Frame& Top = Frames.back();
					Top.GuardCandidate = false;
					if (Key == "define" && TokenCount == Top.TokensAtOpen && Arg.substr(0, Arg.find_first_of(" \t(")) == Top.GuardName)
					{
						Top.Branches.clear();
						Top.Expression.clear();
						Rebuild();
						return;
					}
				}

				if (Key == "if" || Key == "ifdef" || Key == "ifndef")
				{
					Frame NewFrame;
					std::string Cond = Key == "if" ? "(" + Arg + ")" : (Key == "ifdef" ? "defined(" + Arg + ")" : "!defined(" + Arg + ")");
					NewFrame.Branches.push_back(Cond);
					NewFrame.Expression = Cond;
					NewFrame.GuardCandidate = Key == "ifndef" && Frames.empty();
					NewFrame.GuardName = Arg;
					NewFrame.TokensAtOpen = TokenCount;
					Frames.push_back(NewFrame);
				}
				else if ((Key == "elif" || Key == "elifdef" || Key == "elifndef") && !Frames.empty())
				{
					Frame& Top = Frames.back();
					std::string Cond = Key == "elif" ? "(" + Arg + ")" : (Key == "elifdef" ? "defined(" + Arg + ")" : "!defined(" + Arg + ")");
					Top.Expression = Negated(Top.Branches) + " && " + Cond;
					Top.Branches.push_back(Cond);
				}
				else if (Key == "else" && !Frames.empty())
				{
					Frame& Top = Frames.back();
					Top.Expression = Top.Branches.empty() ? std::string() : Negated(Top.Branches);
				}
				else if (Key == "endif" && !Frames.empty())
				{
					Frames.pop_back();
				}
				else
				{
					return;
				}
				Rebuild();
			}

		private:
			struct Frame
			{
				std::vector<std::string> Branches;
				std::string Expression;
				bool GuardCandidate = false;
				std::string GuardName;
				size_t TokensAtOpen = 0;
			};

			std::vector<Frame> Frames;
			std::map<std::string, int> Indices = { { "", 0 } };
			int CurrentIndex = 0;

			static std::string Negated(const std::vector<std::string>& Branches)
			{
				std::string Result;
				for (const std::string& Branch : Branches)
				{
					Result += (Result.empty() ? "!" : " && !") + (Branch.front() == '(' ? Branch : "(" + Branch + ")");
				}
				return Result;
			}

			void Rebuild()
			{
				std::string Joined;
				for (const Frame& Entry : Frames)
				{
					if (!Entry.Expression.empty())
					{
						Joined += (Joined.empty() ? "" : " && ") + Entry.Expression;
					}
				}

				auto [It, Inserted] = Indices.emplace(Joined, (int)Table.size());
				if (Inserted)
				{
					Table.push_back(Joined);
				}
				CurrentIndex = It->second;
			}
		};

		std::vector<Token> Tokenize(const std::string& Src, ConditionTracker& Conditions)
		{
			std::vector<Token> Tokens;
			size_t Pos = Src.starts_with("\xEF\xBB\xBF") ? 3 : 0; // UTF-8 BOM
			int Line = 1;
			bool LineStart = true;

			auto Push = [&](ETokenKind Kind, std::string Text)
			{
				Tokens.push_back({ Kind, std::move(Text), Line, Conditions.Current() });
			};

			while (Pos < Src.size())
			{
				char C = Src[Pos];
				if (C == '\n')
				{
					++Line;
					++Pos;
					LineStart = true;
					continue;
				}

				if (C == ' ' || C == '\t' || C == '\r' || C == '\f' || C == '\v')
				{
					++Pos;
					continue;
				}

				// Preprocessor directive: collect its text (with continuations, without comments).
				if (C == '#' && LineStart)
				{
					std::string Directive;
					++Pos;
					while (Pos < Src.size() && Src[Pos] != '\n')
					{
						if (Src[Pos] == '\\' && Pos + 1 < Src.size() && (Src[Pos + 1] == '\n' || Src[Pos + 1] == '\r'))
						{
							Pos += Src[Pos + 1] == '\r' ? 3 : 2;
							++Line;
							Directive += ' ';
							continue;
						}
						if (Src.compare(Pos, 2, "//") == 0)
						{
							while (Pos < Src.size() && Src[Pos] != '\n')
							{
								++Pos;
							}
							break;
						}
						if (Src.compare(Pos, 2, "/*") == 0)
						{
							size_t End = Src.find("*/", Pos + 2);
							End = End == std::string::npos ? Src.size() : End + 2;
							Line += (int)std::count(Src.begin() + Pos, Src.begin() + End, '\n');
							Pos = End;
							Directive += ' ';
							continue;
						}
						Directive += Src[Pos++];
					}
					Conditions.OnDirective(Trim(Directive), Tokens.size());
					continue;
				}

				LineStart = false;

				if (C == '/' && Pos + 1 < Src.size() && Src[Pos + 1] == '/')
				{
					while (Pos < Src.size() && Src[Pos] != '\n')
					{
						++Pos;
					}
					continue;
				}

				if (C == '/' && Pos + 1 < Src.size() && Src[Pos + 1] == '*')
				{
					size_t End = Src.find("*/", Pos + 2);
					End = End == std::string::npos ? Src.size() : End + 2;
					Line += (int)std::count(Src.begin() + Pos, Src.begin() + End, '\n');
					Pos = End;
					continue;
				}

				// Raw string literal: R"delim(...)delim"
				if (C == 'R' && Pos + 1 < Src.size() && Src[Pos + 1] == '"')
				{
					size_t Open = Src.find('(', Pos + 2);
					if (Open != std::string::npos)
					{
						std::string Close = ")" + Src.substr(Pos + 2, Open - Pos - 2) + "\"";
						size_t End = Src.find(Close, Open);
						End = End == std::string::npos ? Src.size() : End + Close.size();
						Push(ETokenKind::String, Src.substr(Pos, End - Pos));
						Line += (int)std::count(Src.begin() + Pos, Src.begin() + End, '\n');
						Pos = End;
						continue;
					}
				}

				if (C == '"' || C == '\'')
				{
					size_t Start = Pos++;
					while (Pos < Src.size() && Src[Pos] != C && Src[Pos] != '\n')
					{
						Pos += Src[Pos] == '\\' ? 2 : 1;
					}
					++Pos;
					Push(ETokenKind::String, Src.substr(Start, Pos - Start));
					continue;
				}

				if (IsIdentStart(C))
				{
					size_t Start = Pos;
					while (Pos < Src.size() && IsIdentChar(Src[Pos]))
					{
						++Pos;
					}
					Push(ETokenKind::Identifier, Src.substr(Start, Pos - Start));
					continue;
				}

				if (C >= '0' && C <= '9')
				{
					size_t Start = Pos;
					while (Pos < Src.size() && (IsIdentChar(Src[Pos]) || Src[Pos] == '.' || Src[Pos] == '\''))
					{
						++Pos;
					}
					Push(ETokenKind::Number, Src.substr(Start, Pos - Start));
					continue;
				}

				if (Src.compare(Pos, 3, "...") == 0)
				{
					Push(ETokenKind::Punct, "...");
					Pos += 3;
					continue;
				}

				if (Src.compare(Pos, 2, "::") == 0 || Src.compare(Pos, 2, "&&") == 0 || Src.compare(Pos, 2, "->") == 0 ||
					Src.compare(Pos, 2, "<=") == 0 || Src.compare(Pos, 2, ">=") == 0 || Src.compare(Pos, 2, "==") == 0 || Src.compare(Pos, 2, "!=") == 0)
				{
					Push(ETokenKind::Punct, Src.substr(Pos, 2));
					Pos += 2;
					continue;
				}

				Push(ETokenKind::Punct, std::string(1, C));
				++Pos;
			}

			return Tokens;
		}

		std::string JoinTokens(const std::vector<Token>& Tokens, size_t Begin, size_t End)
		{
			std::string Result;
			for (size_t Index = Begin; Index < End; ++Index)
			{
				if (Index > Begin && ((IsWordToken(Tokens[Index - 1]) && IsWordToken(Tokens[Index])) || Tokens[Index - 1].Text == ","))
				{
					Result += ' ';
				}
				Result += Tokens[Index].Text;
			}
			return Result;
		}

		std::string Unquote(const std::string& Text)
		{
			if (Text.size() >= 2 && Text.front() == '"' && Text.back() == '"')
			{
				std::string Result;
				for (size_t Index = 1; Index + 1 < Text.size(); ++Index)
				{
					if (Text[Index] == '\\' && Index + 2 < Text.size())
					{
						++Index;
					}
					Result += Text[Index];
				}
				return Result;
			}
			return Text;
		}

		const std::set<std::string> DeclSpecifiers = {
			"virtual", "inline", "constexpr", "consteval", "explicit", "friend", "extern",
			"IC", "ICF", "ICN", "__forceinline", "XR_NODISCARD", "mutable"
		};

		// Type keywords that can end a parameter type, so they are never taken for a parameter name.
		const std::set<std::string> TypeKeywords = {
			"int", "char", "float", "double", "long", "short", "unsigned", "signed", "bool", "void", "const", "volatile", "wchar_t", "auto"
		};

		const std::set<std::string> ClassSpecifiers = { "LuaExport", "Name", "Constructible", "Bases", "LuaToString", "Condition" };
		const std::set<std::string> EnumSpecifiers = { "LuaEnum", "Self", "Name", "Prefix", "Exclude" };
		const std::set<std::string> FunctionSpecifiers = { "LuaExport", "Name", "Condition", "Self", "Policies" };
		const std::set<std::string> MemberSpecifiers = { "LuaCallable", "LuaReadOnly", "LuaReadWrite", "LuaConstructor", "LuaOperator", "LuaEnum", "Name", "Policies", "Prefix", "Exclude" };

		// Lua-bindable operators and the luabind expression operator for each.
		const std::set<std::string> SupportedOperators = { "+", "-", "*", "/", "==", "<", "<=", ">", ">=" };

		enum class EScopeKind
		{
			Namespace,
			Class,
			Other
		};

		struct Scope
		{
			EScopeKind Kind;
			std::string Name;
			int ClassIndex = -1;
		};

		class SourceParser
		{
		public:
			SourceParser(FileInfo& InFile, std::vector<Token> InTokens, std::vector<std::string> InConditions, std::vector<ParseError>& InErrors)
				: File(InFile), Tokens(std::move(InTokens)), Conditions(std::move(InConditions)), Errors(InErrors)
			{
			}

			void Parse()
			{
				size_t Index = 0;
				while (Index < Tokens.size())
				{
					Index = Step(Index);
				}

				if (PendingClass)
				{
					Error(PendingSpecs.Line, "XREFLECT is not followed by a class definition");
				}
			}

		private:
			FileInfo& File;
			std::vector<Token> Tokens;
			std::vector<std::string> Conditions;
			std::vector<ParseError>& Errors;
			std::vector<Scope> Scopes;
			bool PendingClass = false;
			Specifiers PendingSpecs;

			void Error(int Line, const std::string& Message)
			{
				Errors.push_back({ File.Path, Line, Message });
			}

			const std::string& Text(size_t Index) const
			{
				static const std::string Empty;
				return Index < Tokens.size() ? Tokens[Index].Text : Empty;
			}

			int LineAt(size_t Index) const
			{
				return Index < Tokens.size() ? Tokens[Index].Line : (Tokens.empty() ? 0 : Tokens.back().Line);
			}

			size_t SkipBalanced(size_t Index, const std::string& Open, const std::string& Close) const
			{
				int Depth = 0;
				for (; Index < Tokens.size(); ++Index)
				{
					if (Text(Index) == Open)
					{
						++Depth;
					}
					else if (Text(Index) == Close && --Depth == 0)
					{
						return Index + 1;
					}
				}
				return Index;
			}

			std::string ScopePrefix(bool IncludeClasses) const
			{
				std::string Prefix;
				for (const Scope& Entry : Scopes)
				{
					if (Entry.Kind == EScopeKind::Namespace || (IncludeClasses && Entry.Kind == EScopeKind::Class))
					{
						if (!Entry.Name.empty())
						{
							Prefix += Entry.Name + "::";
						}
					}
				}
				return Prefix;
			}

			Scope* CurrentScope()
			{
				return Scopes.empty() ? nullptr : &Scopes.back();
			}

			size_t ParseSpecifiers(size_t Index, Specifiers& Out)
			{
				Out = {};
				Out.Line = LineAt(Index);
				Out.Condition = Conditions[Tokens[Index].Condition];
				size_t End = SkipBalanced(Index + 1, "(", ")");
				for (size_t Pos = Index + 2; Pos + 1 < End;)
				{
					if (Tokens[Pos].Kind != ETokenKind::Identifier)
					{
						Error(LineAt(Pos), "unexpected '" + Text(Pos) + "' in specifier list");
						return End;
					}

					std::string Key = Text(Pos++);
					std::string Value;
					if (Text(Pos) == "=")
					{
						Value = Unquote(Text(Pos + 1));
						Pos += 2;
					}
					Out.Values[Key] = Value;

					if (Text(Pos) == ",")
					{
						++Pos;
					}
				}
				return End;
			}

			bool CheckSpecifiers(const Specifiers& Specs, const std::set<std::string>& Allowed)
			{
				bool Ok = true;
				for (const auto& [Key, Value] : Specs.Values)
				{
					if (!Allowed.contains(Key))
					{
						Error(Specs.Line, "unknown specifier '" + Key + "'");
						Ok = false;
					}
				}
				return Ok;
			}

			// Finds the end of a declaration: the index after ';' or after the body.
			// DeclEnd is the end of the declarator part (before '=', '{' or ';').
			size_t ScanDeclaration(size_t Index, size_t& DeclEnd) const
			{
				int Depth = 0;
				DeclEnd = std::string::npos;
				for (; Index < Tokens.size(); ++Index)
				{
					const std::string& Tok = Text(Index);
					if (Tok == "(" || Tok == "[")
					{
						++Depth;
					}
					else if (Tok == ")" || Tok == "]")
					{
						--Depth;
					}
					else if (Depth == 0)
					{
						if (Tok == "=" && DeclEnd == std::string::npos && Text(Index - 1) != "operator")
						{
							DeclEnd = Index;
						}
						else if (Tok == ";")
						{
							DeclEnd = std::min(DeclEnd, Index);
							return Index + 1;
						}
						else if (Tok == "{")
						{
							bool IsFunctionBody = DeclEnd == std::string::npos;
							DeclEnd = std::min(DeclEnd, Index);
							size_t After = SkipBalanced(Index, "{", "}");
							if (IsFunctionBody && Text(After) != ";")
							{
								return After;
							}
							Index = After - 1;
						}
						else if (Tok == ":" && DeclEnd == std::string::npos && Index > 0 && Text(Index - 1) == ")")
						{
							// Constructor initializer list: the body follows it.
							DeclEnd = Index;
							while (++Index < Tokens.size())
							{
								if (Text(Index) == "(")
								{
									Index = SkipBalanced(Index, "(", ")") - 1;
								}
								else if (Text(Index) == "{")
								{
									// Member brace-init "Value{}" follows a name; the body does not.
									bool IsMemberInit = Tokens[Index - 1].Kind == ETokenKind::Identifier || Text(Index - 1) == ">";
									size_t After = SkipBalanced(Index, "{", "}");
									if (!IsMemberInit)
									{
										return After;
									}
									Index = After - 1;
								}
							}
							return Index;
						}
					}
				}
				DeclEnd = std::min(DeclEnd, Index);
				return Index;
			}

			// Drops attributes, __declspec and declaration specifiers. Returns cleaned tokens.
			std::vector<Token> CleanDeclaration(size_t Begin, size_t End, bool& IsStatic) const
			{
				std::vector<Token> Out;
				IsStatic = false;
				for (size_t Index = Begin; Index < End; ++Index)
				{
					if (Text(Index) == "[" && Text(Index + 1) == "[")
					{
						Index = SkipBalanced(Index, "[", "]") - 1;
						continue;
					}
					if ((Text(Index) == "__declspec" || Text(Index) == "alignas") && Text(Index + 1) == "(")
					{
						Index = SkipBalanced(Index + 1, "(", ")") - 1;
						continue;
					}
					if (Text(Index) == "static")
					{
						IsStatic = true;
						continue;
					}
					if (DeclSpecifiers.contains(Text(Index)))
					{
						continue;
					}
					Out.push_back(Tokens[Index]);
				}
				return Out;
			}

			// Returns the index of the parameter list '(' or npos for a variable declaration.
			// OperatorAt receives the index of the 'operator' keyword, if any.
			static size_t FindFunctionParen(const std::vector<Token>& Decl, size_t& OperatorAt)
			{
				OperatorAt = std::string::npos;
				int AngleDepth = 0;
				for (size_t Index = 0; Index < Decl.size(); ++Index)
				{
					const std::string& Tok = Decl[Index].Text;
					if (Tok == "operator" && AngleDepth == 0)
					{
						OperatorAt = Index;
						// operator() has its symbol in parentheses: skip them.
						size_t Paren = Index + 1;
						if (Paren + 1 < Decl.size() && Decl[Paren].Text == "(" && Decl[Paren + 1].Text == ")")
						{
							Paren += 2;
						}
						for (; Paren < Decl.size() && Decl[Paren].Text != "("; ++Paren)
						{
						}
						return Paren < Decl.size() ? Paren : std::string::npos;
					}
					if (Tok == "<" && Index > 0 && Decl[Index - 1].Kind == ETokenKind::Identifier)
					{
						++AngleDepth;
					}
					else if (Tok == ">" && AngleDepth > 0)
					{
						--AngleDepth;
					}
					else if (Tok == "(" && AngleDepth == 0)
					{
						return Index > 0 && Decl[Index - 1].Kind == ETokenKind::Identifier ? Index : std::string::npos;
					}
				}
				return std::string::npos;
			}

			// Parameter type without its name: "const T& Value" -> "const T&".
			static std::string ParamType(const std::vector<Token>& Decl, size_t Begin, size_t End)
			{
				if (End - Begin >= 2 && Decl[End - 1].Kind == ETokenKind::Identifier && !TypeKeywords.contains(Decl[End - 1].Text) &&
					Decl[End - 2].Text != "::")
				{
					--End;
				}
				return JoinTokens(Decl, Begin, End);
			}

			bool BuildFunction(const std::vector<Token>& Decl, size_t ParenIndex, size_t OperatorAt, int Line, FunctionInfo& Out)
			{
				size_t NameIndex = OperatorAt == std::string::npos ? ParenIndex - 1 : OperatorAt;
				if (NameIndex > 0 && Decl[NameIndex - 1].Text == "::")
				{
					Error(Line, "reflected functions must be declared in their own scope");
					return false;
				}

				if (OperatorAt != std::string::npos)
				{
					Out.CppName = "operator" + JoinTokens(Decl, OperatorAt + 1, ParenIndex);
					Out.OperatorSymbol = JoinTokens(Decl, OperatorAt + 1, ParenIndex);
				}
				else
				{
					Out.CppName = Decl[NameIndex].Text;
				}
				Out.ReturnType = JoinTokens(Decl, 0, NameIndex);
				if (Out.ReturnType == "auto" || Out.ReturnType == "decltype(auto)")
				{
					Error(Line, "reflected functions need an explicit return type");
					return false;
				}

				size_t Close = ParenIndex;
				int Depth = 0;
				for (; Close < Decl.size(); ++Close)
				{
					if (Decl[Close].Text == "(")
					{
						++Depth;
					}
					else if (Decl[Close].Text == ")" && --Depth == 0)
					{
						break;
					}
				}

				// Split parameters at top-level commas and drop default arguments.
				size_t ParamBegin = ParenIndex + 1;
				Depth = 0;
				size_t DefaultAt = std::string::npos;
				for (size_t Index = ParamBegin; Index <= Close && Index < Decl.size(); ++Index)
				{
					const std::string& Tok = Decl[Index].Text;
					bool AtEnd = Index == Close;
					if (!AtEnd && (Tok == "(" || Tok == "[" || Tok == "{" || Tok == "<"))
					{
						++Depth;
					}
					else if (!AtEnd && (Tok == ")" || Tok == "]" || Tok == "}" || Tok == ">"))
					{
						--Depth;
					}
					else if (!AtEnd && Depth == 0 && Tok == "=" && DefaultAt == std::string::npos)
					{
						DefaultAt = Index;
					}

					if (AtEnd || (Depth == 0 && Tok == ","))
					{
						size_t ParamEnd = std::min(Index, DefaultAt);
						std::string Param = JoinTokens(Decl, ParamBegin, ParamEnd);
						if (!Param.empty() && Param != "void")
						{
							Out.ParamTypes.push_back(ParamType(Decl, ParamBegin, ParamEnd));
						}
						ParamBegin = Index + 1;
						DefaultAt = std::string::npos;
					}
				}

				for (size_t Index = Close + 1; Index < Decl.size(); ++Index)
				{
					const std::string& Tok = Decl[Index].Text;
					if (Tok == "const" || Tok == "volatile" || Tok == "&" || Tok == "&&")
					{
						Out.Qualifiers += (Out.Qualifiers.empty() || Tok[0] == '&' ? "" : " ") + Tok;
					}
					else if (Tok == "noexcept" && Index + 1 < Decl.size() && Decl[Index + 1].Text == "(")
					{
						break;
					}
				}
				if (!Out.Qualifiers.empty() && Out.Qualifiers[0] != '&')
				{
					Out.Qualifiers = " " + Out.Qualifiers;
				}
				return true;
			}

			size_t ParseClassHead(size_t Index)
			{
				// Index points at class/struct/union.
				bool IsStruct = Text(Index) != "class";
				size_t Pos = Index + 1;
				size_t Colon = std::string::npos;
				for (; Pos < Tokens.size(); ++Pos)
				{
					const std::string& Tok = Text(Pos);
					bool InHead = Colon == std::string::npos;
					if (Tok == ";" || (InHead && (Tok == "(" || Tok == "=" || Tok == ")" || Tok == ",")))
					{
						// Forward declaration or elaborated type specifier.
						if (PendingClass)
						{
							Error(PendingSpecs.Line, "XREFLECT is not followed by a class definition");
							PendingClass = false;
						}
						return Index + 1;
					}
					if (Tok == "{")
					{
						break;
					}
					if (Tok == ":" && Colon == std::string::npos)
					{
						Colon = Pos;
					}
				}

				size_t NameEnd = Colon == std::string::npos ? Pos : Colon;
				std::string Name;
				for (size_t NamePos = Index + 1; NamePos < NameEnd; ++NamePos)
				{
					if (Tokens[NamePos].Kind == ETokenKind::Identifier && Text(NamePos) != "final")
					{
						Name = Text(NamePos);
					}
				}

				Scope NewScope { EScopeKind::Class, Name };

				if (PendingClass)
				{
					PendingClass = false;
					if (CheckSpecifiers(PendingSpecs, ClassSpecifiers))
					{
						if (!PendingSpecs.Has("LuaExport"))
						{
							Error(PendingSpecs.Line, "XREFLECT on a class requires LuaExport");
						}

						ClassInfo Class;
						Class.File = File.Path;
						Class.Line = PendingSpecs.Line;
						Class.QualifiedName = ScopePrefix(true) + Name;
						Class.LuaName = PendingSpecs.Get("Name", Name);
						Class.Condition = PendingSpecs.Condition;
						Class.RuntimeCondition = PendingSpecs.Get("Condition");
						Class.Constructible = PendingSpecs.Has("Constructible");
						Class.ToString = PendingSpecs.Has("LuaToString");
						Class.ExplicitBases = SplitList(PendingSpecs.Get("Bases"), ',');

						if (Colon != std::string::npos)
						{
							size_t BaseBegin = Colon + 1;
							int Depth = 0;
							for (size_t Base = BaseBegin; Base <= Pos; ++Base)
							{
								const std::string& Tok = Text(Base);
								if (Base < Pos && (Tok == "<" || Tok == "("))
								{
									++Depth;
								}
								else if (Base < Pos && (Tok == ">" || Tok == ")"))
								{
									--Depth;
								}
								else if (Base == Pos || (Depth == 0 && Tok == ","))
								{
									bool IsPublic = IsStruct;
									size_t TypeBegin = BaseBegin;
									for (; TypeBegin < Base; ++TypeBegin)
									{
										const std::string& Word = Text(TypeBegin);
										if (Word == "public")
										{
											IsPublic = true;
										}
										else if (Word == "private" || Word == "protected")
										{
											IsPublic = false;
										}
										else if (Word != "virtual")
										{
											break;
										}
									}
									if (IsPublic && TypeBegin < Base)
									{
										Class.PublicBases.push_back(JoinTokens(Tokens, TypeBegin, Base));
									}
									BaseBegin = Base + 1;
								}
							}
						}

						NewScope.ClassIndex = (int)File.Classes.size();
						File.Classes.push_back(std::move(Class));
					}
				}

				Scopes.push_back(NewScope);
				return Pos + 1;
			}

			size_t ParseMember(size_t Index, const Specifiers& Specs)
			{
				size_t DeclEnd = 0;
				size_t Next = ScanDeclaration(Index, DeclEnd);
				int Line = LineAt(Index);

				Scope* Owner = CurrentScope();
				if (!Owner || Owner->Kind != EScopeKind::Class || Owner->ClassIndex < 0)
				{
					Error(Specs.Line, "XPROPERTY must be inside a class marked with XREFLECT");
					return Next;
				}
				if (!CheckSpecifiers(Specs, MemberSpecifiers))
				{
					return Next;
				}

				ClassInfo& Class = File.Classes[Owner->ClassIndex];

				if (Text(Index) == "enum")
				{
					if (!Specs.Has("LuaEnum"))
					{
						Error(Specs.Line, "XPROPERTY on an enum requires LuaEnum");
						return Next;
					}
					MemberInfo Member;
					if (ReadEnum(Index, Next, Specs, Class.QualifiedName + "::", Member))
					{
						Class.Members.push_back(std::move(Member));
					}
					return Next;
				}
				if (Specs.Has("LuaEnum"))
				{
					Error(Specs.Line, "LuaEnum applies to enums only");
					return Next;
				}

				bool IsStatic = false;
				std::vector<Token> Decl = CleanDeclaration(Index, DeclEnd, IsStatic);
				size_t OperatorAt = std::string::npos;
				size_t Paren = FindFunctionParen(Decl, OperatorAt);

				MemberInfo Member;
				Member.Condition = Specs.Condition;

				if (Paren != std::string::npos)
				{
					if (IsStatic)
					{
						Error(Specs.Line, "static methods are exported as free functions with XREFLECT");
						return Next;
					}

					bool IsConstructor = OperatorAt == std::string::npos && Paren == 1 && Decl[0].Text == Owner->Name;
					if (IsConstructor != Specs.Has("LuaConstructor"))
					{
						Error(Specs.Line, IsConstructor ? "XPROPERTY on a constructor requires LuaConstructor" : "LuaConstructor applies to constructors only");
						return Next;
					}
					if ((OperatorAt != std::string::npos) != Specs.Has("LuaOperator"))
					{
						Error(Specs.Line, OperatorAt != std::string::npos ? "XPROPERTY on an operator requires LuaOperator" : "LuaOperator applies to operators only");
						return Next;
					}
					if (!IsConstructor && OperatorAt == std::string::npos && !Specs.Has("LuaCallable"))
					{
						Error(Specs.Line, "XPROPERTY on a method requires LuaCallable");
						return Next;
					}

					if (IsConstructor)
					{
						// Constructor: the "return type" is empty and the name is the class name.
						std::vector<Token> AsFunction = Decl;
						if (!BuildFunction(AsFunction, Paren, std::string::npos, Line, Member.Function))
						{
							return Next;
						}
						Member.Kind = EMemberKind::Constructor;
						Class.Members.push_back(std::move(Member));
						return Next;
					}

					if (!BuildFunction(Decl, Paren, OperatorAt, Line, Member.Function))
					{
						return Next;
					}

					if (OperatorAt != std::string::npos)
					{
						if (!SupportedOperators.contains(Member.Function.OperatorSymbol) || Member.Function.ParamTypes.size() != 1)
						{
							Error(Specs.Line, "only binary member operators + - * / == < <= > >= can be reflected");
							return Next;
						}
						Member.Kind = EMemberKind::Operator;
						Class.Members.push_back(std::move(Member));
						return Next;
					}

					Member.Kind = EMemberKind::Method;
					Member.LuaName = Specs.Get("Name", Member.Function.CppName);
					Member.Policies = Specs.Get("Policies");
					Class.Members.push_back(std::move(Member));
					return Next;
				}

				bool ReadOnly = Specs.Has("LuaReadOnly");
				if (!ReadOnly && !Specs.Has("LuaReadWrite"))
				{
					Error(Specs.Line, "XPROPERTY on a field requires LuaReadOnly or LuaReadWrite");
					return Next;
				}
				if (IsStatic)
				{
					Error(Specs.Line, "static fields can not be reflected");
					return Next;
				}
				if (Decl.empty() || Decl.back().Text == "]")
				{
					Error(Specs.Line, "array fields can not be reflected");
					return Next;
				}
				if (Decl.back().Kind != ETokenKind::Identifier || (Decl.size() > 1 && Decl[Decl.size() - 2].Text == ","))
				{
					Error(Specs.Line, "declare one reflected field per declaration");
					return Next;
				}

				Member.Kind = EMemberKind::Field;
				Member.Function.CppName = Decl.back().Text;
				Member.LuaName = Specs.Get("Name", Member.Function.CppName);
				Member.ReadOnly = ReadOnly;
				Class.Members.push_back(std::move(Member));
				return Next;
			}

			// enum [class] Name [: type] { A [= expr], ... };
			// Scope is the qualified prefix of the enum ("CFoo::" or "Ns::").
			bool ReadEnum(size_t Index, size_t Next, const Specifiers& Specs, const std::string& Scope, MemberInfo& Out)
			{
				size_t Pos = Index + 1;
				bool IsScoped = Text(Pos) == "class" || Text(Pos) == "struct";
				if (IsScoped)
				{
					++Pos;
				}
				if (Tokens[Pos].Kind != ETokenKind::Identifier)
				{
					Error(Specs.Line, "reflected enums need a name");
					return false;
				}

				Out.Kind = EMemberKind::Enum;
				Out.Condition = Specs.Condition;
				Out.LuaName = Specs.Get("Name", Text(Pos));
				Out.EnumScope = Scope + (IsScoped ? Text(Pos) + "::" : std::string());

				std::string Prefix = Specs.Get("Prefix");
				std::vector<std::string> Exclude = SplitList(Specs.Get("Exclude"), ',');

				while (Pos < Next && Text(Pos) != "{")
				{
					++Pos;
				}

				bool ExpectName = true;
				int Depth = 0;
				for (++Pos; Pos < Next && !(Depth == 0 && Text(Pos) == "}"); ++Pos)
				{
					const std::string& Tok = Text(Pos);
					if (Tok == "(" || Tok == "{" || Tok == "[")
					{
						++Depth;
					}
					else if (Tok == ")" || Tok == "}" || Tok == "]")
					{
						--Depth;
					}
					else if (Depth == 0 && Tok == ",")
					{
						ExpectName = true;
					}
					else if (ExpectName && Tokens[Pos].Kind == ETokenKind::Identifier)
					{
						ExpectName = false;
						if (std::find(Exclude.begin(), Exclude.end(), Tok) != Exclude.end())
						{
							continue;
						}
						if (!Prefix.empty() && !Tok.starts_with(Prefix))
						{
							Error(LineAt(Pos), "enumerator '" + Tok + "' does not start with Prefix '" + Prefix + "'");
							continue;
						}
						Out.Enumerators.push_back({ Tok.substr(Prefix.size()), Tok });
					}
				}
				return true;
			}

			size_t ParseFreeEnum(size_t Index, const Specifiers& Specs)
			{
				size_t DeclEnd = 0;
				size_t Next = ScanDeclaration(Index, DeclEnd);

				Scope* Owner = CurrentScope();
				if (Owner && Owner->Kind != EScopeKind::Namespace)
				{
					Error(Specs.Line, "use XPROPERTY(LuaEnum) for enums inside a class");
					return Next;
				}
				if (!CheckSpecifiers(Specs, EnumSpecifiers))
				{
					return Next;
				}
				if (!Specs.Has("LuaEnum") || Specs.Get("Self").empty())
				{
					Error(Specs.Line, "XREFLECT on an enum requires LuaEnum and Self");
					return Next;
				}

				FreeEnumInfo Info;
				Info.File = File.Path;
				Info.Line = Specs.Line;
				Info.SelfClass = Specs.Get("Self");
				if (ReadEnum(Index, Next, Specs, ScopePrefix(false), Info.Member))
				{
					File.Enums.push_back(std::move(Info));
				}
				return Next;
			}

			size_t ParseFreeFunction(size_t Index, const Specifiers& Specs)
			{
				size_t DeclEnd = 0;
				size_t Next = ScanDeclaration(Index, DeclEnd);
				int Line = LineAt(Index);

				Scope* Owner = CurrentScope();
				if (Owner && Owner->Kind != EScopeKind::Namespace)
				{
					Error(Specs.Line, "XREFLECT functions must be declared at namespace scope");
					return Next;
				}
				if (!CheckSpecifiers(Specs, FunctionSpecifiers))
				{
					return Next;
				}
				if (!Specs.Has("LuaExport"))
				{
					Error(Specs.Line, "XREFLECT on a function requires LuaExport");
					return Next;
				}

				bool IsStatic = false;
				std::vector<Token> Decl = CleanDeclaration(Index, DeclEnd, IsStatic);
				size_t OperatorAt = std::string::npos;
				size_t Paren = FindFunctionParen(Decl, OperatorAt);
				if (Paren == std::string::npos || OperatorAt != std::string::npos)
				{
					Error(Specs.Line, "XREFLECT must be followed by a class or a function");
					return Next;
				}
				if (IsStatic && !File.IsSource)
				{
					Error(Specs.Line, "static functions in headers can not be reflected");
					return Next;
				}

				FreeFunctionInfo Info;
				Info.File = File.Path;
				Info.Line = Specs.Line;
				Info.Condition = Specs.Condition;
				Info.RuntimeCondition = Specs.Get("Condition");
				Info.SelfClass = Specs.Get("Self");
				Info.Policies = Specs.Get("Policies");
				if (!Info.SelfClass.empty() && !Info.RuntimeCondition.empty())
				{
					Error(Specs.Line, "Condition can not be used with Self");
					return Next;
				}
				if (BuildFunction(Decl, Paren, OperatorAt, Line, Info.Function))
				{
					Info.QualifiedName = ScopePrefix(false) + Info.Function.CppName;
					Info.LuaName = Specs.Get("Name", Info.Function.CppName);
					File.Functions.push_back(std::move(Info));
				}
				return Next;
			}

			size_t Step(size_t Index)
			{
				const std::string& Tok = Text(Index);

				if (Tok == "XREFLECT" && Text(Index + 1) == "(")
				{
					Specifiers Specs;
					size_t Next = ParseSpecifiers(Index, Specs);
					if (Text(Next) == "class" || Text(Next) == "struct")
					{
						PendingClass = true;
						PendingSpecs = Specs;
						return ParseClassHead(Next);
					}
					if (Text(Next) == "enum")
					{
						return ParseFreeEnum(Next, Specs);
					}
					return ParseFreeFunction(Next, Specs);
				}

				if (Tok == "XPROPERTY" && Text(Index + 1) == "(")
				{
					Specifiers Specs;
					size_t Next = ParseSpecifiers(Index, Specs);
					return ParseMember(Next, Specs);
				}

				if (Tok == "XGENERATED_BODY" && Text(Index + 1) == "(")
				{
					return SkipBalanced(Index + 1, "(", ")");
				}

				if (Tok == "template" && Text(Index + 1) == "<")
				{
					return SkipBalanced(Index + 1, "<", ">");
				}

				if (Tok == "namespace")
				{
					std::string Name;
					size_t Pos = Index + 1;
					for (; Pos < Tokens.size() && Text(Pos) != "{" && Text(Pos) != "=" && Text(Pos) != ";"; ++Pos)
					{
						Name += Text(Pos);
					}
					if (Text(Pos) == "{")
					{
						Scopes.push_back({ EScopeKind::Namespace, Name });
					}
					return Pos + 1;
				}

				if (Tok == "enum")
				{
					size_t Pos = Index + 1;
					while (Pos < Tokens.size() && Text(Pos) != "{" && Text(Pos) != ";")
					{
						++Pos;
					}
					return Text(Pos) == "{" ? SkipBalanced(Pos, "{", "}") : Pos + 1;
				}

				if (Tok == "class" || Tok == "struct" || Tok == "union")
				{
					const std::string& Prev = Index > 0 ? Text(Index - 1) : std::string();
					if (Index == 0 || Prev == ";" || Prev == "{" || Prev == "}" || Prev == ":" || Prev == ">" || Prev == ")")
					{
						return ParseClassHead(Index);
					}
					return Index + 1;
				}

				if (Tok == "{")
				{
					Scopes.push_back({ EScopeKind::Other, {} });
					return Index + 1;
				}

				if (Tok == "}")
				{
					if (!Scopes.empty())
					{
						Scopes.pop_back();
					}
					return Index + 1;
				}

				return Index + 1;
			}
		};

		void SortClasses(std::vector<ClassInfo*>& Classes, std::vector<ParseError>& Errors)
		{
			std::map<std::string, ClassInfo*> ByName;
			for (ClassInfo* Class : Classes)
			{
				ByName[Class->QualifiedName] = Class;
			}

			auto Resolve = [&](const ClassInfo* Owner, const std::string& Base) -> ClassInfo*
			{
				// Try the base relative to the owner's enclosing scopes, then as written.
				std::string Scope = Owner->QualifiedName;
				while (true)
				{
					size_t Cut = Scope.rfind("::");
					Scope = Cut == std::string::npos ? std::string() : Scope.substr(0, Cut);
					std::string Candidate = Scope.empty() ? Base : Scope + "::" + Base;
					if (auto It = ByName.find(Candidate); It != ByName.end())
					{
						return It->second;
					}
					if (Scope.empty())
					{
						break;
					}
				}
				std::string Trimmed = Base.starts_with("::") ? Base.substr(2) : Base;
				auto It = ByName.find(Trimmed);
				return It == ByName.end() ? nullptr : It->second;
			};

			std::vector<ClassInfo*> Sorted;
			std::set<ClassInfo*> Done;
			std::set<ClassInfo*> Visiting;

			auto Visit = [&](auto&& Self, ClassInfo* Class) -> void
			{
				if (Done.contains(Class))
				{
					return;
				}
				if (Visiting.contains(Class))
				{
					Errors.push_back({ Class->File, Class->Line, "inheritance cycle through '" + Class->QualifiedName + "'" });
					return;
				}
				Visiting.insert(Class);
				for (const std::string& Base : Class->PublicBases)
				{
					if (ClassInfo* BaseClass = Resolve(Class, Base))
					{
						Self(Self, BaseClass);
					}
				}
				Visiting.erase(Class);
				Done.insert(Class);
				Sorted.push_back(Class);
			};

			for (ClassInfo* Class : Classes)
			{
				Visit(Visit, Class);
			}

			// Keep only reflected bases plus explicit ones in the generated base list.
			for (ClassInfo* Class : Sorted)
			{
				std::vector<std::string> Bases;
				for (const std::string& Base : Class->PublicBases)
				{
					if (const ClassInfo* BaseClass = Resolve(Class, Base))
					{
						Bases.push_back(BaseClass->QualifiedName);
					}
				}
				for (const std::string& Base : Class->ExplicitBases)
				{
					if (std::find(Bases.begin(), Bases.end(), Base) == Bases.end())
					{
						Bases.push_back(Base);
					}
				}
				Class->PublicBases = std::move(Bases);
			}

			Classes = std::move(Sorted);
		}
	}

	void ParseFile(FileInfo& File, const std::string& Source, std::vector<ParseError>& Errors)
	{
		ConditionTracker Conditions;
		std::vector<Token> Tokens = Tokenize(Source, Conditions);
		SourceParser Parser(File, std::move(Tokens), Conditions.Table, Errors);
		Parser.Parse();
	}

	std::vector<ClassInfo*> ResolveModel(std::vector<FileInfo>& Files, std::vector<ParseError>& Errors)
	{
		std::vector<ClassInfo*> Classes;
		for (FileInfo& File : Files)
		{
			for (ClassInfo& Class : File.Classes)
			{
				Classes.push_back(&Class);
			}
		}

		auto FindClass = [&](const std::string& Name) -> ClassInfo*
		{
			std::string Trimmed = Name.starts_with("::") ? Name.substr(2) : Name;
			auto It = std::find_if(Classes.begin(), Classes.end(), [&](const ClassInfo* Class) { return Class->QualifiedName == Trimmed; });
			return It == Classes.end() ? nullptr : *It;
		};

		// Enums with Self become enums of that class. The class registration must see the declaration.
		for (FileInfo& File : Files)
		{
			for (FreeEnumInfo& Enum : File.Enums)
			{
				ClassInfo* Class = FindClass(Enum.SelfClass);
				if (!Class)
				{
					Errors.push_back({ Enum.File, Enum.Line, "Self class '" + Enum.SelfClass + "' is not reflected" });
				}
				else if (File.IsSource && Class->File != File.Path)
				{
					Errors.push_back({ Enum.File, Enum.Line, "declare a Self enum in a header or in the file of its class" });
				}
				else
				{
					Class->Members.push_back(std::move(Enum.Member));
				}
			}
			File.Enums.clear();
		}

		// Free functions with Self become methods of that class.
		for (FileInfo& File : Files)
		{
			std::erase_if(File.Functions, [&](const FreeFunctionInfo& Function)
			{
				if (Function.SelfClass.empty())
				{
					return false;
				}

				std::string SelfName = Function.SelfClass.starts_with("::") ? Function.SelfClass.substr(2) : Function.SelfClass;
				auto It = std::find_if(Classes.begin(), Classes.end(), [&](const ClassInfo* Class) { return Class->QualifiedName == SelfName; });
				if (It == Classes.end())
				{
					Errors.push_back({ Function.File, Function.Line, "Self class '" + SelfName + "' is not reflected" });
					return true;
				}

				// The class registration must see the function declaration.
				ClassInfo& Class = **It;
				if (File.IsSource && Class.File != File.Path)
				{
					Errors.push_back({ Function.File, Function.Line, "declare a Self function in a header or in the file of its class" });
					return true;
				}

				MemberInfo Member;
				Member.Kind = EMemberKind::Extension;
				Member.LuaName = Function.LuaName;
				Member.Function = Function.Function;
				Member.QualifiedName = Function.QualifiedName;
				Member.Policies = Function.Policies;
				Member.Condition = Function.Condition;
				Class.Members.push_back(std::move(Member));
				return true;
			});
		}

		SortClasses(Classes, Errors);
		return Classes;
	}
}
