#include "Value.h"
#include "JetContext.h"

using namespace Jet;

#ifdef _DEBUG
#ifndef DBG_NEW      
#define DBG_NEW new ( _NORMAL_BLOCK , __FILE__ , __LINE__ )     
#define new DBG_NEW   
#endif

#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif

size_t stringhash(const char* str)
{
	/*size_t hash = *str;
	while (*(str++))
	hash += *str;*/

	size_t hash = 5381;
	char c;
	while (c = *str++)
		hash = ((hash << 5) + hash) + c;
	return hash;
}

JetObject::JetObject(JetContext* jcontext)
{
	grey = this->mark = false;
	refcount = 0;
	type = ValueType::Object;

	prototype = jcontext->object;
	context = jcontext;
	Size = 0;
	nodecount = 2;
	nodes = new ObjNode[2];
}

JetObject::~JetObject()
{
	delete[] nodes;
}

std::size_t JetObject::key(const Value* v) const
{
	switch(v->type)
	{
	case ValueType::Null:
		return 0;
	case ValueType::Array:
	case ValueType::Userdata:
	case ValueType::Function:
	case ValueType::Object:
		return (size_t)v->_array;
	case ValueType::Number:
		return (size_t)v->value;
	case ValueType::String:
		return stringhash(v->_string->data);
	case ValueType::NativeFunction:
		return (size_t)v->func;
	}
}

//just looks for a node
ObjNode* JetObject::findNode(const Value* key)
{
	ObjNode* node = &this->nodes[this->key(key)%this->nodecount];
	if (node->first.type != ValueType::Null)
	{
		do
		{
			if (node->first == *key)
				return node;//we found it
			else
				node = node->next;
		}
		while(node);
	}	
	return 0;
}

ObjNode* JetObject::findNode(const char* key)
{
	ObjNode* node = &this->nodes[stringhash(key)%this->nodecount];
	if (node->first.type != ValueType::Null)
	{
		do
		{
			if (node->first.type == ValueType::String && strcmp(node->first._string->data, key) == 0)
				return node;//we found it
			else
				node = node->next;
		}
		while(node);
	}	
	return 0;
}

//finds node for key or creates one if doesnt exist
ObjNode* JetObject::getNode(const Value* key)
{
	ObjNode* mpnode = &this->nodes[this->key(key)%this->nodecount];//node in the main position
	if (mpnode->first.type != ValueType::Null)
	{
		ObjNode* node = mpnode;
		while(node->next)//go down the linked list of nodes
		{
			if (node->first == *key)
				return node;//we found it
			else
				node = node->next;
		}
		if (node->first == *key)
			return node;//we found it

		if (this->Size == this->nodecount)//regrow if we are out of room
		{
			this->resize();
			return this->getNode(key);
		}

		ObjNode* onode = &this->nodes[this->key(&mpnode->first)%this->nodecount];//get the main position of the node in our main position

		ObjNode* newnode = this->getFreePosition();//find an open node to insert into

		if (onode != mpnode)//is this node not in the main position?
		{
			//relocate the node and relink it in, then insert the new node at its position
			while (onode->next != mpnode) onode = onode->next;

			this->Barrier();

			onode->next = newnode;
			*newnode = *mpnode;
			mpnode->next = 0;
			mpnode->second = Value();
			mpnode->first = *key;
			Size++;

			return mpnode;
		}
		else
		{
			this->Barrier();

			//insert the new node and just link in, we had a hash collision
			newnode->first = *key;
			newnode->next = 0;
			node->next = newnode;//link the new one into the chain
			Size++;

			return newnode;
		}
	}

	this->Barrier();

	//main position for key is open, just insert
	mpnode->first = *key;
	mpnode->next = 0;
	Size++;

	return mpnode;
}

//this method allocates new key strings
ObjNode* JetObject::getNode(const char* key)
{
	ObjNode* mpnode = &this->nodes[stringhash(key)%this->nodecount];//node in the main position
	if (mpnode->first.type != ValueType::Null)
	{
		ObjNode* node = mpnode;
		while (node->next)
		{
			if (node->first.type == ValueType::String && strcmp(node->first._string->data, key) == 0)
				return node;//we found it
			else
				node = node->next;
		}

		if (node->first.type == ValueType::String && strcmp(node->first._string->data, key) == 0)
			return node;//we found it

		if (this->Size == this->nodecount)//regrow if we are out of room
		{
			this->resize();
			return this->getNode(key);
		}

		ObjNode* onode = &this->nodes[this->key(&mpnode->first)%this->nodecount];//get the main position of the node in our main position

		ObjNode* newnode = this->getFreePosition();//find an open node to insert into

		if (onode != mpnode)//is this node not in the main position?
		{
			//relocate the node and relink it in, then insert the new node at its position
			while (onode->next != mpnode) onode = onode->next;

			this->Barrier();

			onode->next = newnode;
			*newnode = *mpnode;
			mpnode->next = 0;
			mpnode->second = Value();
			mpnode->first = context->NewString(key);
			Size++;

			return mpnode;
		}
		else
		{
			this->Barrier();

			//insert the new node and just link in, we had a hash collision
			newnode->first = context->NewString(key);
			newnode->next = 0;
			node->next = newnode;//link the new one into the chain
			Size++;

			return newnode;
		}
	}

	this->Barrier();

	//main position for key is open, just insert
	mpnode->first = context->NewString(key);
	mpnode->next = 0;
	Size++;

	return mpnode;
}

ObjNode* JetObject::getFreePosition()
{
	for (unsigned int i = 0; i < this->nodecount; i++)
	{
		if (this->nodes[i].first.type == ValueType::Null)
			return &this->nodes[i];
	}
	return 0;
}

void JetObject::resize()
{
	//change resize rate, this is just a quickie
	auto t = this->nodes;
	this->nodes = new ObjNode[this->nodecount*2];//lets be dumb atm
	auto osize = this->nodecount;
	this->nodecount *= 2;
	this->Size = 0;//cheat
	for (unsigned int i = 0; i < osize; i++)
	{
		//reinsert into hashtable
		ObjNode* n = &t[i];

		ObjNode* np = this->getNode(&n->first);
		np->second = n->second;
	}

	delete[] t;

	/*printf("JetObject Resized:\n");
	for (int i = 0; i < this->nodecount; i++)
	{
	auto k = this->nodes[i].first.ToString();
	auto v = this->nodes[i].second.ToString();
	printf("[%d] %s    %s   Hash: %i\n", i, k.c_str(), v.c_str(), (this->key(&this->nodes[i].first)%this->nodecount));
	}*/
}

//try not to use these in the vm
Value& JetObject::operator [](const Value& key)
{
	ObjNode* node = this->getNode(&key);
	/*printf("JetObject Changed:\n");
	for (int i = 0; i < this->nodecount; i++)
	{
		auto k = this->nodes[i].first.ToString();
		auto v = this->nodes[i].second.ToString();
		printf("[%d] %s    %s   Hash: %i\n", i, k.c_str(), v.c_str(), (this->key(&this->nodes[i].first)%this->nodecount));
	}*/
	return node->second;
}

//special operator for strings to deal with insertions
Value& JetObject::operator [](const char* key)
{
	ObjNode* node = this->getNode(key);
	/*printf("JetObject Changed:\n");
	for (int i = 0; i < this->nodecount; i++)
	{
		auto k = this->nodes[i].first.ToString();
		auto v = this->nodes[i].second.ToString();
		printf("[%d] %s    %s   Hash: %i\n", i, k.c_str(), v.c_str(), (this->key(&this->nodes[i].first)%this->nodecount));
	}*/
	return node->second;
}

void JetObject::DebugPrint()
{
	printf("JetObject Changed:\n");
	for (unsigned int i = 0; i < this->nodecount; i++)
	{
		auto k = this->nodes[i].first.ToString();
		auto v = this->nodes[i].second.ToString();
		printf("[%d] %s    %s   Hash: %i\n", i, k.c_str(), v.c_str(), (this->key(&this->nodes[i].first)%this->nodecount));
	}
}

//use this function
void JetObject::Barrier()
{
	if (this->mark)
	{
		//reset to grey and push back for reprocessing
		//printf("object write barrier triggered!\n");
		this->mark = false;
		this->context->gc.greys.Push(this);//push to grey stack
	}
}