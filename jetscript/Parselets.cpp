#include "Parselets.h"
#include "Expressions.h"
#include "Parser.h"
#include "UniquePtr.h"
#include <string>

using namespace Jet;

Expression* NameParselet::parse(Parser* parser, Token token)
{
	if (parser->MatchAndConsume(TokenType::LeftBracket))
	{
		//array index
		UniquePtr<Expression*> index = parser->parseExpression();
		parser->Consume(TokenType::RightBracket);

		return new IndexExpression(new NameExpression(token.text), index.Release(), token);
	}
	else
		return new NameExpression(token.text);
}

Expression* AssignParselet::parse(Parser* parser, Expression* left, Token token)
{
	Expression* right = parser->parseExpression(Precedence::ASSIGNMENT-1/*assignment prcedence -1 */);

	if (dynamic_cast<IStorableExpression*>(left) == 0)
	{
		delete right;
		throw CompilerException(parser->filename, token.line, "AssignParselet: Left hand side must be a storable location!");
	}
	return new AssignExpression(left, right);
}

Expression* OperatorAssignParselet::parse(Parser* parser, Expression* left, Token token)
{
	if (dynamic_cast<IStorableExpression*>(left) == 0)
		throw CompilerException(parser->filename, token.line, "OperatorAssignParselet: Left hand side must be a storable location!");

	Expression* right = parser->parseExpression(Precedence::ASSIGNMENT-1/*assignment prcedence -1 */);

	return new OperatorAssignExpression(token, left, right);
}

Expression* SwapParselet::parse(Parser* parser, Expression* left, Token token)
{
	if (dynamic_cast<IStorableExpression*>(left) == 0)
		throw CompilerException(parser->filename, token.line, "SwapParselet: Left hand side must be a storable location!");

	UniquePtr<Expression*> right = parser->parseExpression(Precedence::ASSIGNMENT-1/*assignment prcedence -1 */);

	if (dynamic_cast<IStorableExpression*>((Expression*)right) == 0)
		throw CompilerException(parser->filename, token.line, "SwapParselet: Right hand side must be a storable location!");

	return new SwapExpression(left, right.Release());
}

Expression* PrefixOperatorParselet::parse(Parser* parser, Token token)
{
	Expression* right = parser->parseExpression(precedence);
	if (right == 0)
		throw CompilerException(parser->filename, token.line, "PrefixOperatorParselet: Right hand side missing!");

	return new PrefixExpression(token, right);
}

Expression* BinaryOperatorParselet::parse(Parser* parser, Expression* left, Token token)
{
	Expression* right = parser->parseExpression(precedence - (isRight ? 1 : 0));
	if (right == 0)
		throw CompilerException(parser->filename, token.line, "BinaryOperatorParselet: Right hand side missing!");

	return new OperatorExpression(left, token, right);
}

Expression* GroupParselet::parse(Parser* parser, Token token)
{
	UniquePtr<Expression*> exp = parser->parseExpression();
	parser->Consume(TokenType::RightParen);
	return exp.Release();
}

Expression* WhileParselet::parse(Parser* parser, Token token)
{
	parser->Consume(TokenType::LeftParen);

	UniquePtr<Expression*> condition = parser->parseExpression();

	parser->Consume(TokenType::RightParen);

	auto block = new ScopeExpression(parser->parseBlock());
	return new WhileExpression(token, condition.Release(), block);
}

Expression* ForParselet::parse(Parser* parser, Token token)
{
	parser->Consume(TokenType::LeftParen);
	if (parser->LookAhead().type == TokenType::Local)
	{
		if (parser->LookAhead(1).type == TokenType::Name)
		{
			Token n = parser->LookAhead(2);
			if (n.type == TokenType::Name && n.text == "in")
			{
				//ok its a foreach loop
				parser->Consume();
				auto name = parser->Consume();
				parser->Consume();
				UniquePtr<Expression*> container = parser->parseExpression();
				parser->Consume(TokenType::RightParen);

				auto block = new ScopeExpression(parser->parseBlock());
				return new ForEachExpression(name, container.Release(), block);
			}
		}
	}

	UniquePtr<Expression*> initial = parser->ParseStatement(true);
	UniquePtr<Expression*> condition = parser->ParseStatement(true);
	UniquePtr<Expression*> increment = parser->parseExpression();

	parser->Consume(TokenType::RightParen);

	auto block = new ScopeExpression(parser->parseBlock());
	return new ForExpression(token, initial.Release(), condition.Release(), increment.Release(), block);
}

Expression* IfParselet::parse(Parser* parser, Token token)
{
	std::vector<Branch*> branches;
	//take parens
	parser->Consume(TokenType::LeftParen);
	UniquePtr<Expression*> ifcondition = parser->parseExpression();
	parser->Consume(TokenType::RightParen);

	BlockExpression* ifblock = parser->parseBlock(true);

	branches.push_back(new Branch(ifblock, ifcondition.Release()));

	Branch* Else = 0;
	while(true)
	{
		//look for elses
		if (parser->MatchAndConsume(TokenType::ElseIf))
		{
			//keep going
			parser->Consume(TokenType::LeftParen);
			UniquePtr<Expression*> condition = parser->parseExpression();
			parser->Consume(TokenType::RightParen);

			BlockExpression* block = parser->parseBlock(true);

			branches.push_back(new Branch(block, condition.Release()));
		}
		else if (parser->MatchAndConsume(TokenType::Else))
		{
			//its an else
			BlockExpression* block = parser->parseBlock(true);

			Else = new Branch(block, 0);
			break;
		}
		else
			break;//nothing else
	}

	return new IfExpression(token, std::move(branches), Else);
}

Expression* FunctionParselet::parse(Parser* parser, Token token)
{
	auto name = new NameExpression(parser->Consume(TokenType::Name).getText());
	auto arguments = new std::vector<Expression*>;

	NameExpression* varargs = 0;
	parser->Consume(TokenType::LeftParen);

	if (!parser->MatchAndConsume(TokenType::RightParen))
	{
		do
		{
			Token name = parser->Consume();
			if (name.type == TokenType::Name)
			{
				arguments->push_back(new NameExpression(name.text));
			}
			else if (name.type == TokenType::Ellipses)
			{
				varargs = new NameExpression(parser->Consume(TokenType::Name).getText());

				break;//this is end of parsing arguments
			}
			else
			{
				std::string str = "Consume: TokenType not as expected! Expected Name or Ellises Got: " + name.text;
				throw CompilerException(parser->filename, name.line, str);
			}
		}
		while(parser->MatchAndConsume(TokenType::Comma));

		parser->Consume(TokenType::RightParen);
	}

	auto block = new ScopeExpression(parser->parseBlock());
	return new FunctionExpression(token, name, arguments, block, varargs);
}

Expression* LambdaParselet::parse(Parser* parser, Token token)
{
	parser->Consume(TokenType::LeftParen);

	NameExpression* varargs = 0;
	auto arguments = new std::vector<Expression*>;
	if (parser->LookAhead().type != TokenType::RightParen)
	{
		do
		{
			Token name = parser->Consume();
			if (name.type == TokenType::Name)
			{
				arguments->push_back(new NameExpression(name.getText()));
			}
			else if (name.type == TokenType::Ellipses)
			{
				varargs = new NameExpression(parser->Consume(TokenType::Name).getText());

				break;//this is end of parsing arguments
			}
			else
			{
				std::string str = "Consume: TokenType not as expected! Expected Name or Ellises Got: " + name.text;
				throw CompilerException(parser->filename, name.line, str);
			}
		}
		while(parser->MatchAndConsume(TokenType::Comma));
	}

	parser->Consume(TokenType::RightParen);

	auto block = new ScopeExpression(parser->parseBlock());
	return new FunctionExpression(token, 0, arguments, block, varargs);
}

Expression* CallParselet::parse(Parser* parser, Expression* left, Token token)
{
	UniquePtr<std::vector<Expression*>*> arguments = new std::vector<Expression*>;

	if (!parser->MatchAndConsume(TokenType::RightParen))
	{
		do
		{
			arguments->push_back(parser->parseExpression(Precedence::ASSIGNMENT));
		}
		while( parser->MatchAndConsume(TokenType::Comma));

		parser->Consume(TokenType::RightParen);
	}
	return new CallExpression(token, left, arguments.Release());
}

Expression* ReturnParselet::parse(Parser* parser, Token token)
{
	Expression* right = 0;
	if (parser->Match(TokenType::Semicolon) == false)
		right = parser->parseExpression(Precedence::ASSIGNMENT);

	return new ReturnExpression(token, right);
}

Expression* LocalParselet::parse(Parser* parser, Token token)
{
	UniquePtr<std::vector<Token>*> names = new std::vector<Token>;

	do
	{
		Token name = parser->Consume(TokenType::Name);
		names->push_back(name);
	}
	while (parser->MatchAndConsume(TokenType::Comma));

	parser->Consume(TokenType::Assign);//its possible this wont be here and it may just be a mentioning, but no assignment

	//handle multiple comma expressions
	UniquePtr<std::vector<Expression*>*> rights = new std::vector<Expression*>;
	do
	{
		Expression* right = parser->parseExpression(Precedence::ASSIGNMENT-1/*assignment prcedence -1 */);

		rights->push_back(right);
	}
	while (parser->MatchAndConsume(TokenType::Comma));

	parser->Consume(TokenType::Semicolon);

	return new LocalExpression(names.Release(), rights.Release());
}

Expression* ConstParselet::parse(Parser* parser, Token token)
{
	throw CompilerException("", 0, "Const keyword not implemented!");

	std::vector<Token>* names = new std::vector<Token>;
	do
	{
		Token name = parser->Consume(TokenType::Name);
		names->push_back(name);
	}
	while (parser->MatchAndConsume(TokenType::Comma));

	parser->Consume(TokenType::Assign);//its possible this wont be here and it may just be a mentioning, but no assignment

	//do somethign with multiple comma expressions
	std::vector<Expression*>* rights = new std::vector<Expression*>;
	do
	{
		Expression* right = parser->parseExpression(Precedence::ASSIGNMENT-1/*assignment prcedence -1 */);

		rights->push_back(right);
	}
	while (parser->MatchAndConsume(TokenType::Comma));

	parser->Consume(TokenType::Semicolon);
	//do stuff with this and store and what not
	//need to add this variable to this's block expression

	return new LocalExpression(names, rights);
}

Expression* ArrayParselet::parse(Parser* parser, Token token)
{
	std::vector<Expression*> inits;// = new std::vector<Expression*>;
	while(parser->LookAhead().getType() != TokenType::RightBracket)
	{
		Expression* e = parser->parseExpression(2);

		inits.push_back(e);

		if (!parser->MatchAndConsume(TokenType::Comma))//check if more
			break;//we are done
	}
	parser->Consume(TokenType::RightBracket);
	return new ArrayExpression(std::move(inits));
}

Expression* IndexParselet::parse(Parser* parser, Expression* left, Token token)
{
	UniquePtr<Expression*> index = parser->parseExpression();
	parser->Consume(TokenType::RightBracket);

	return new IndexExpression(left, index.Release(), token);
}

Expression* MemberParselet::parse(Parser* parser, Expression* left, Token token)
{
	//this is for const members
	Expression* member = parser->parseExpression(Precedence::CALL);
	UniquePtr<NameExpression*> name = dynamic_cast<NameExpression*>(member);
	if (name == 0)
	{
		delete member; delete left;
		throw CompilerException(parser->filename, token.line, "Cannot access member name that is not a string");
	}

	auto ret = new IndexExpression(left, new StringExpression(name->GetName()), token);

	return ret;
}

Expression* ObjectParselet::parse(Parser* parser, Token token)
{
	if (parser->MatchAndConsume(TokenType::RightBrace))
	{
		//we are done, return null object
		return new ObjectExpression();
	}

	//parse initial values
	std::vector<std::pair<std::string, Expression*>>* inits = new std::vector<std::pair<std::string, Expression*>>;
	while(parser->LookAhead().type == TokenType::Name || parser->LookAhead().type == TokenType::String || parser->LookAhead().type == TokenType::Number)
	{
		Token name = parser->Consume();

		parser->Consume(TokenType::Assign);

		//parse the data;
		Expression* e = parser->parseExpression(Precedence::LOGICAL);

		inits->push_back(std::pair<std::string, Expression*>(name.text, e));
		if (!parser->MatchAndConsume(TokenType::Comma))//is there more to parse?
			break;//we are done
	}
	parser->Consume(TokenType::RightBrace);//end part
	return new ObjectExpression(inits);
};

Expression* YieldParselet::parse(Parser* parser, Token token)
{
	Expression* right = 0;
	if (parser->Match(TokenType::Semicolon) == false)
		right = parser->parseExpression(Precedence::ASSIGNMENT);

	return new YieldExpression(token, right);
}

Expression* InlineYieldParselet::parse(Parser* parser, Token token)
{
	Expression* right = 0;
	if (parser->Match(TokenType::Semicolon) == false && parser->LookAhead().type != TokenType::RightParen)
		right = parser->parseExpression(Precedence::ASSIGNMENT);

	return new YieldExpression(token, right);
}

Expression* ResumeParselet::parse(Parser* parser, Token token)
{
	Expression* right = parser->parseExpression(Precedence::ASSIGNMENT);

	return new ResumeExpression(token, right);
}

Expression* ResumePrefixParselet::parse(Parser* parser, Token token)
{
	Expression* right = parser->parseExpression(Precedence::ASSIGNMENT);

	return new ResumeExpression(token, right);
}