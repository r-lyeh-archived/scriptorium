
#ifndef _PARSER
#define _PARSER

#ifdef _DEBUG
#ifndef DBG_NEW      
#define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )     
#define new DBG_NEW   
#endif

#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif

#include <stdio.h>
#include <map>

#include "Token.h"
#include "Compiler.h"
#include "Expressions.h"
#include "Parselets.h"
#include "Lexer.h"
#include "JetExceptions.h"

#include <deque>

namespace Jet
{
	class Parser
	{
		Lexer* lexer;
		std::map<TokenType, InfixParselet*> mInfixParselets;
		std::map<TokenType, PrefixParselet*> mPrefixParselets;
		std::map<TokenType, StatementParselet*> mStatementParselets;
		std::deque<Token> mRead;

	public:
		std::string filename;
		Parser(Lexer* l);

		~Parser();

		Expression* parseExpression(int precedence = 0);
		Expression* ParseStatement(bool takeTrailingSemicolon = true);//call this until out of tokens (hit EOF)
		BlockExpression* parseBlock(bool allowsingle = true);
		BlockExpression* parseAll();

		int getPrecedence();

		Token Consume();
		Token Consume(TokenType expected);

		Token LookAhead(unsigned int num = 0);

		bool Match(TokenType expected);
		bool MatchAndConsume(TokenType expected);

		void Register(TokenType token, InfixParselet* parselet);
		void Register(TokenType token, PrefixParselet* parselet);
		void Register(TokenType token, StatementParselet* parselet);
	};
}
#endif