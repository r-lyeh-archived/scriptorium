#ifdef _DEBUG
#ifndef DBG_NEW      
//#define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )     
//#define new DBG_NEW   
#endif

#define _CRTDBG_MAP_ALLOC
#define _CRTDBG_MAP_ALLOC_NEW
#include <crtdbg.h>
#endif

#include "Compiler.h"
#include "Parser.h"

using namespace Jet;

CompilerContext::CompilerContext(void)
{
	this->vararg = false;
	this->isgenerator = false;
	this->closures = 0;
	this->parent = 0;
	this->uuid = 0;
	this->localindex = 0;
	this->lastline = 0;
	this->scope = new CompilerContext::Scope;
	this->scope->level = 0;
	this->scope->previous = this->scope->next = 0;
}

CompilerContext::~CompilerContext(void)
{
	if (this->scope)
	{
		auto next = this->scope->next;
		while (next)
		{
			auto tmp = next->next;
			delete next;
			next = tmp;
		}
	}
	delete this->scope;

	//delete functions
	for (auto ii: this->functions)
		delete ii.second;
}

void CompilerContext::PrintAssembly()
{
	int index = 0;
	for (unsigned int i = 0; i < this->out.size(); i++)
	{
		auto ins = this->out[i];
		if (ins.type == InstructionType::Function)
		{
			printf("\n\nfunction %s definition\n%d arguments, %d locals, %d captures", ins.string, ins.a, ins.b, ins.c);
			index = 0;

			if (i+1 < this->out.size() && out[i+1].type == InstructionType::DebugLine)
			{
				++i;
				printf(", %s line %.0lf", out[i].string, out[i].second);
			}
			continue;
		}
		else if (ins.type == InstructionType::Local)
		{
			printf("\nLocal\t%s", ins.string);
		}
		else if (ins.type == InstructionType::Capture)
		{
			printf("\nCapture\t%s", ins.string);
		}
		else if (ins.type == InstructionType::Label)
		{
			printf("\nLabel\t%s", ins.string);
		}
		else
		{
			if (ins.string)
				printf("\n[%d]\t%-15s %-5.0lf %s", index++, Instructions[(int)ins.type], ins.second, ins.string);
			else
				printf("\n[%d]\t%-15s %-5d %.0lf", index++, Instructions[(int)ins.type], ins.first, ins.second);
		}

		if (i+1 < this->out.size() && out[i+1].type == InstructionType::DebugLine)
		{
			++i;
			printf(" ; %s line %.0lf", out[i].string, out[i].second);
		}
	}
	printf("\n");
}

std::vector<IntermediateInstruction> CompilerContext::Compile(BlockExpression* expr, const char* filename)
{
	try
	{
		//may want to correct number of locals here
		this->FunctionLabel("{Entry Point}", 0, 0, 0, this->vararg);

		expr->Compile(this);

		//add a return to signify end of global code
		if (this->out[this->out.size()-1].type != InstructionType::Return)
		{
			this->Null();
			this->Return();
		}

		this->Compile();

		if (localindex > 255)
			throw CompilerException(this->filename, this->lastline, "Too many locals: over 256 locals in function!");
		if (closures > 255)
			throw CompilerException(this->filename, this->lastline, "Too many closures: over 256 closures in function!");

		//modify the entry point with number of locals
		this->out[0].b = this->localindex;
		this->out[0].c = this->closures;
		this->out[0].d = this->vararg + this->isgenerator*2;
	}
	catch (CompilerException e)
	{
		//clean up the compiler
		if (this->scope)
		{
			auto next = this->scope->next;
			this->scope->next = 0;
			while (next)
			{
				auto tmp = next->next;
				delete next;
				next = tmp;
			}
		}
		this->scope->localvars.clear();

		for (auto ii: this->functions)
			delete ii.second;

		this->functions.clear();

		this->localindex = 0;
		this->closures = 0;

		throw e;
	}

	if (this->scope)
	{
		auto next = this->scope->next;
		this->scope->next = 0;
		while (next)
		{
			auto tmp = next->next;
			delete next;
			next = tmp;
		}
		this->scope->localvars.clear();
	}

	for (auto ii: this->functions)
		delete ii.second;

	this->functions.clear();
	//add custom operators
	this->localindex = 0;
	this->closures = 0;

	//this->PrintAssembly();

	auto temp = std::move(this->out);
	this->out.clear();
	return std::move(temp);
}

bool CompilerContext::RegisterLocal(const std::string name)
{
	//neeed to store locals in a contiguous array, even with different scopes
	for (unsigned int i = 0; i < this->scope->localvars.size(); i++)
	{
		if (this->scope->localvars[i].name == name)
			return false;
	}

	LocalVariable var;
	var.local = this->localindex++;
	var.name = name;
	this->scope->localvars.push_back(var);

	out.push_back(IntermediateInstruction(InstructionType::Local, var.name, 0));

	return true;
}

void CompilerContext::BinaryOperation(TokenType operation)
{
	switch (operation)
	{
	case TokenType::Plus:
	case TokenType::AddAssign:
		this->out.push_back(IntermediateInstruction(InstructionType::Add));
		break;	
	case TokenType::Asterisk:
	case TokenType::MultiplyAssign:
		this->out.push_back(IntermediateInstruction(InstructionType::Mul));
		break;	
	case TokenType::Minus:
	case TokenType::SubtractAssign:
		this->out.push_back(IntermediateInstruction(InstructionType::Sub));
		break;
	case TokenType::Slash:
	case TokenType::DivideAssign:
		this->out.push_back(IntermediateInstruction(InstructionType::Div));
		break;
	case TokenType::Modulo:
		this->out.push_back(IntermediateInstruction(InstructionType::Modulus));
		break;
	case TokenType::Equals:
		this->out.push_back(IntermediateInstruction(InstructionType::Eq));
		break;
	case TokenType::NotEqual:
		this->out.push_back(IntermediateInstruction(InstructionType::NotEq));
		break;
	case TokenType::LessThan:
		this->out.push_back(IntermediateInstruction(InstructionType::Lt));
		break;
	case TokenType::GreaterThan:
		this->out.push_back(IntermediateInstruction(InstructionType::Gt));
		break;
	case TokenType::LessThanEqual:
		this->out.push_back(IntermediateInstruction(InstructionType::LtE));
		break;
	case TokenType::GreaterThanEqual:
		this->out.push_back(IntermediateInstruction(InstructionType::GtE));
		break;
	case TokenType::OrAssign:
	case TokenType::BOr:
		this->out.push_back(IntermediateInstruction(InstructionType::BOr));
		break;
	case TokenType::AndAssign:
	case TokenType::BAnd:
		this->out.push_back(IntermediateInstruction(InstructionType::BAnd));
		break;
	case TokenType::XorAssign:
	case TokenType::Xor:
		this->out.push_back(IntermediateInstruction(InstructionType::Xor));
		break;
	case TokenType::LeftShift:
		this->out.push_back(IntermediateInstruction(InstructionType::LeftShift));
		break;
	case TokenType::RightShift:
		this->out.push_back(IntermediateInstruction(InstructionType::RightShift));
		break;
	}
}

void CompilerContext::UnaryOperation(TokenType operation)
{
	switch (operation)
	{
	case TokenType::Increment:
		this->out.push_back(IntermediateInstruction(InstructionType::Incr));
		break;
	case TokenType::Decrement:
		this->out.push_back(IntermediateInstruction(InstructionType::Decr));
		break;	
	case TokenType::Minus:
		this->out.push_back(IntermediateInstruction(InstructionType::Negate));
		break;
	case TokenType::BNot:
		this->out.push_back(IntermediateInstruction(InstructionType::BNot));
		break;
	}
}

CompilerContext* CompilerContext::AddFunction(std::string name, unsigned int args, bool vararg)
{
	//push instruction that sets the function
	//todo, may need to have functions in other instruction code sets
	CompilerContext* newfun = new CompilerContext();
	//insert this into my list of functions
	std::string fname = name+this->GetUUID();
	newfun->arguments = args;
	newfun->uuid = this->uuid;
	newfun->parent = this;
	newfun->vararg = vararg;
	this->functions[fname] = newfun;

	//store the function in the variable
	this->LoadFunction(fname);

	return newfun;
};

void CompilerContext::FinalizeFunction(CompilerContext* c)
{
	this->uuid = c->uuid + 1;

	//move upvalues
	for (auto ii: this->functions)//check all children
	{
		for (auto& i: ii.second->captures)
		{
			if (i.second.uploaded == false)
			{
				i.second.uploaded = true;
				//printf("closed %d %d\n", i.second.captureindex, i.second.localindex);
				out.push_back(IntermediateInstruction(InstructionType::CInit,i.second.localindex/*ptr->localvars[i].local*/, i.second.captureindex/*ptr->localvars[i].capture*/));
			}
		}
	}
}

void CompilerContext::Load(const std::string variable)
{
	Scope* ptr = this->scope;
	while (ptr)
	{
		//look for var in locals
		for (unsigned int i = 0; i < ptr->localvars.size(); i++)
		{
			if (ptr->localvars[i].name == variable)
			{
				//printf("We found loading of a local var: %s at level %d, index %d\n", variable.c_str(), ptr->level, ptr->localvars[i].first);
				out.push_back(IntermediateInstruction(InstructionType::LLoad, ptr->localvars[i].local, 0));//i, ptr->level));
				return;//exit the loops we found it
			}
		}
		ptr = ptr->previous;
	}

	int level = 0;
	auto cur = this->parent;
	auto prev = this;
	while(cur)
	{
		ptr = cur->scope;
		while (ptr)
		{
			//look for var in locals
			for (unsigned int i = 0; i < ptr->localvars.size(); i++)
			{
				if (ptr->localvars[i].name == variable)
				{
					//printf("We found loading of a captured var: %s at level %d, index %d\n", variable.c_str(), level, ptr->localvars[i].first);
					auto cpt = prev->captures.find(ptr->localvars[i].name);
					if (cpt == prev->captures.end())
					{
						prev->captures[ptr->localvars[i].name] = Capture(level, ptr->localvars[i].local, prev->closures++);
						cpt = prev->captures.find(ptr->localvars[i].name);

						out.push_back(IntermediateInstruction(InstructionType::Capture, ptr->localvars[i].name, 0));
					}

					out.push_back(IntermediateInstruction(InstructionType::CLoad, cpt->second.captureindex/*ptr->localvars[i].capture*/, level));//i, ptr->level));
					return;//exit the loops we found it
				}
			}
			ptr = ptr->previous;
		}
		level--;
		prev = cur;
		cur = cur->parent;
	}
	out.push_back(IntermediateInstruction(InstructionType::Load, variable));
}

void CompilerContext::Store(const std::string variable)
{
	//look up if I am a local or global
	Scope* ptr = this->scope;
	while (ptr)
	{
		//look for var in locals
		for (unsigned int i = 0; i < ptr->localvars.size(); i++)
		{
			if (ptr->localvars[i].name == variable)
			{
				//printf("We found storing of a local var: %s at level %d, index %d\n", variable.c_str(), ptr->level, ptr->localvars[i].first);
				out.push_back(IntermediateInstruction(InstructionType::LStore, ptr->localvars[i].local, 0));//i, ptr->level));
				return;//exit the loops we found it
			}
		}
		ptr = ptr->previous;
	}

	int level = 0;
	auto cur = this->parent;
	auto prev = this;
	while(cur)
	{
		ptr = cur->scope;
		while (ptr)
		{
			//look for var in locals
			for (unsigned int i = 0; i < ptr->localvars.size(); i++)
			{
				if (ptr->localvars[i].name == variable)
				{
					//printf("We found storing of a captured var: %s at level %d, index %d\n", variable.c_str(), level, ptr->localvars[i].first);
					//exit the loops we found it
					auto cpt = prev->captures.find(ptr->localvars[i].name);
					if (cpt == prev->captures.end())
					{
						prev->captures[ptr->localvars[i].name] = Capture(level, ptr->localvars[i].local, prev->closures++);
						cpt = prev->captures.find(ptr->localvars[i].name);

						out.push_back(IntermediateInstruction(InstructionType::Capture, ptr->localvars[i].name, 0));
					}

					out.push_back(IntermediateInstruction(InstructionType::CStore, cpt->second.captureindex /*ptr->localvars[i].capture*/, level));//i, ptr->level));
					return;
				}
			}
			ptr = ptr->previous;
		}
		level--;
		prev = cur;
		cur = cur->parent;
	}
	out.push_back(IntermediateInstruction(InstructionType::Store, variable));
}