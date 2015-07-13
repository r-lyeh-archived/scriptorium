#include "os-sqlite3.h"
#include "../os-binder.h"
// #include "sqlite3ext.h"
#include "sqlite3.h"

namespace ObjectScript {

class SqliteOS: public OS // get access to protected members
{
public:

	static void triggerError(OS * os, int code)
	{
		OS_ASSERT(code != SQLITE_OK);
		os->getGlobal(OS_TEXT("SqliteException"));
		os->pushGlobals();
		os->pushString(sqlite3_errstr(code));
		os->pushNumber(code);
		os->callFT(2, 1);
		os->setException();
	}

	static void triggerError(OS * os, const OS_CHAR * msg)
	{
		os->getGlobal(OS_TEXT("SqliteException"));
		os->pushGlobals();
		os->pushString(msg);
		os->callFT(1, 1);
		os->setException();
	}

	static bool checkError(OS * os, int err_code)
	{
		if(err_code != SQLITE_OK){
			if(!os->isExceptionSet()){
				triggerError(os, err_code);
			}
			return true;
		}
		return false; // os->isExceptionSet();
	}

	static void initExtension(OS * os);

	struct SqliteStatement;
	struct Sqlite
	{
		OS * os;
		sqlite3 * db;
		SqliteStatement * list;

		Sqlite(OS * p_os, const char * flename)
		{
			os = p_os;
			db = NULL;
			list = NULL;
			checkError(sqlite3_open(flename, &db));
		}

		~Sqlite()
		{
			while(list){
				removeStatement(list);
			}
			close();
		}

		bool findStatement(SqliteStatement * stmt);
		void addStatement(SqliteStatement * stmt);
		void removeStatement(SqliteStatement * stmt);

		static void initScript(OS * os)
		{
#define OS_AUTO_TEXT(exp) OS_TEXT(#exp)
			os->eval(OS_AUTO_TEXT(
				SqliteException = extends Exception {
					__construct = function(msg, code){
						super(msg)
						@code = code
					}
				}
				function SqliteConnection.execute(sql, params){
					return @query(sql, params).execute()
				}
				function SqliteConnection.fetch(sql, params){
					return @query(sql, params).fetch()
				}
			));
		}

		bool checkError(int err_code)
		{
			return SqliteOS::checkError(os, err_code);
		}

		bool isOpen(){ return db != NULL; }

		void close()
		{
			if(db){
				sqlite3_close(db);
				db = NULL;
			}
		}

		SqliteStatement * prepare(const OS::String& sql);

		static int __newinstance(OS * os, int params, int, int, void * user_param)
		{
			if(params < 1){
				triggerError(os, OS_TEXT("filename requied"));
				return 0;
			}
			Sqlite * self = new (os->malloc(sizeof(Sqlite) OS_DBG_FILEPOS)) Sqlite(os, os->toString(-params+0));
			if(self->isOpen()){
				pushCtypeValue(os, self);
				return 1;
			}
			self->~Sqlite();
			os->free(self);
			return 0;
		}

		static int getLastInsertId(OS * os, int params, int, int, void * user_param);
		static int query(OS * os, int params, int, int, void * user_param);
	};

	struct SqliteStatement
	{
		Sqlite * owner;
		SqliteStatement * next;
		sqlite3_stmt * stmt;
		int step_code;
		int row_num;

		SqliteStatement(Sqlite * p_owner, sqlite3_stmt * p_stmt)
		{
			OS_ASSERT(p_owner && p_stmt);
			owner = p_owner;
			next = NULL;
			stmt = p_stmt;
			step_code = SQLITE_OK;
			row_num = 0;
			owner->addStatement(this);
		}

		~SqliteStatement()
		{
			if(owner){
				OS_ASSERT(owner->findStatement(this));
				owner->removeStatement(this);
			}
			close();
		}

		void close()
		{
			if(stmt){
				sqlite3_finalize(stmt);
				stmt = NULL;
			}
		}

		bool checkError(int err_code)
		{
			OS_ASSERT(owner);
			return SqliteOS::checkError(owner->os, err_code);
		}

		void addColumn(const char * name, OS_NUMBER value)
		{
			OS_ASSERT(owner);
			OS * os = owner->os;
#if 0
			os->pushStackValue();
			os->pushNumber(value);
			os->addProperty(false);
#endif
			os->pushNumber(value);
			os->setProperty(-2, name, false);
		}

		void addColumn(const char * name, const void * p_value, int size)
		{
			OS_ASSERT(owner);
			OS * os = owner->os;
			OS::Core::String value(os, p_value, size);
#if 0
			os->pushStackValue();
			os->pushString(value);
			os->addProperty(false);
#endif
			os->pushString(value);
			os->setProperty(-2, name, false);
		}

		void addColumn(const char * name)
		{
			OS_ASSERT(owner);
			OS * os = owner->os;
#if 0
			os->pushStackValue();
			os->pushNull();
			os->addProperty(false);
#endif
			os->pushNull();
			os->setProperty(-2, name, false);
		}

		enum EStepType {
			ITERATE,
			FETCH,
			EXECUTE
		};

		int step(EStepType type)
		{
			OS_ASSERT(owner && stmt);
			if(step_code == SQLITE_DONE){
				return 0;
			}
			OS * os = owner->os;
			step_code = sqlite3_step(stmt);
			if(step_code == SQLITE_ROW){
				row_num++;
				if(type == ITERATE){
					os->pushBool(true);
					os->pushNumber(row_num-1);
				}else if(type == EXECUTE){
					os->pushBool(true);
					return 1;
				}
				os->newObject();
				int cols = sqlite3_column_count(stmt);
				for(int i = 0; i < cols; i++){
					const char * name = sqlite3_column_name(stmt, i);
					int type = sqlite3_column_type(stmt, i);
					switch(type){
					case SQLITE_INTEGER:
						addColumn(name, (OS_NUMBER)sqlite3_column_int64(stmt, i));
						break;

					case SQLITE_FLOAT:
						addColumn(name, (OS_NUMBER)sqlite3_column_double(stmt, i));
						break;

					case SQLITE_TEXT:
						{
							const unsigned char * buf = sqlite3_column_text(stmt, i);
							int size = sqlite3_column_bytes(stmt, i);
							addColumn(name, buf, size);
							break;
						}

					case SQLITE_BLOB:
						{
							const void * buf = sqlite3_column_blob(stmt, i);
							int size = sqlite3_column_bytes(stmt, i);
							addColumn(name, buf, size);
							break;
						}

					default:
						OS_ASSERT(false);

					case SQLITE_NULL:
						addColumn(name);
					}
				}
				return type == ITERATE ? 3 : 1;
			}
			if(step_code != SQLITE_DONE){
				checkError(step_code);
			}
			return 0;
		}

		int getColumnCount()
		{
			OS_ASSERT(owner && stmt);
			return sqlite3_column_count(stmt);
		}

		int getColumnType(int col)
		{
			OS_ASSERT(owner && stmt);
			return sqlite3_column_type(stmt, col);
		}

		int getParamIndex(const OS::String& name);
		void bindParams();

		/* static int __construct(OS * os, int params, int, int, void * user_param)
		{
			triggerError(os, OS_TEXT("you should not create new instance of SqliteStatement"));
			return 0;
		} */

		static int __iter(OS * os, int params, int, int, void * user_param);
		static int bind(OS * os, int params, int, int, void * user_param);
		static int execute(OS * os, int params, int, int, void * user_param);
		static int fetch(OS * os, int params, int, int, void * user_param);
	};
};

bool SqliteOS::Sqlite::findStatement(SqliteStatement * stmt)
{
	OS_ASSERT(stmt->owner == this);
	for(SqliteStatement * cur = list; cur; cur = cur->next){
		if(cur == stmt){
			return true;
		}
	}
	return false;
}

void SqliteOS::Sqlite::addStatement(SqliteStatement * stmt)
{
	OS_ASSERT(!stmt->next && !findStatement(stmt));
	stmt->next = list;
	list = stmt;
}

void SqliteOS::Sqlite::removeStatement(SqliteStatement * stmt)
{
	OS_ASSERT(stmt->owner == this);
	for(SqliteStatement * cur = list, * prev = NULL; cur; prev = cur, cur = cur->next){
		if(cur == stmt){
			if(prev){
				prev->next = cur->next;
			}else{
				list = cur->next;
			}
			cur->close();
			cur->owner = NULL;
			cur->next = NULL;
			return;
		}
	}
}

SqliteOS::SqliteStatement * SqliteOS::Sqlite::prepare(const OS::String& sql)
{
	OS_ASSERT(db);
	sqlite3_stmt * stmt = NULL;
	const char * sql_tail = NULL;
	int err_code = sqlite3_prepare_v2(db, sql.toChar(), sql.getDataSize(), &stmt, &sql_tail);
	if(!checkError(err_code)){
		OS_ASSERT(stmt);
		return new (os->malloc(sizeof(SqliteStatement) OS_DBG_FILEPOS)) SqliteStatement(this, stmt);
	}
	if(stmt){
		// sqlite3_finalize(stmt);
	}
	return NULL;
}

template <> struct CtypeName<SqliteOS::Sqlite>{ static const OS_CHAR * getName(){ return OS_TEXT("SqliteConnection"); } };
template <> struct CtypeValue<SqliteOS::Sqlite*>: public CtypeUserClass<SqliteOS::Sqlite*>{};
template <> struct UserDataDestructor<SqliteOS::Sqlite>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<SqliteOS::Sqlite*>((SqliteOS::Sqlite*)data));
		SqliteOS::Sqlite * buf = (SqliteOS::Sqlite*)data;
		buf->~Sqlite();
		os->free(buf);
	}
};

template <> struct CtypeName<SqliteOS::SqliteStatement>{ static const OS_CHAR * getName(){ return OS_TEXT("SqliteStatement"); } };
template <> struct CtypeValue<SqliteOS::SqliteStatement*>: public CtypeUserClass<SqliteOS::SqliteStatement*>{};
template <> struct UserDataDestructor<SqliteOS::SqliteStatement>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<SqliteOS::SqliteStatement*>((SqliteOS::SqliteStatement*)data));
		SqliteOS::SqliteStatement * buf = (SqliteOS::SqliteStatement*)data;
		buf->~SqliteStatement();
		os->free(buf);
	}
};

int SqliteOS::SqliteStatement::getParamIndex(const OS::String& name)
{
	OS_CHAR c = name.toChar()[0];
	if(c != OS_TEXT(':') && c != OS_TEXT('$') && c != OS_TEXT('@')){
		int len = name.getLen();
		const int MAX_STACK_LEN = 64;
		if(len < MAX_STACK_LEN-2){
			OS_CHAR buf[MAX_STACK_LEN];
			buf[0] = OS_TEXT(':');
			OS_MEMCPY(buf+1, name.toChar(), (len+1) * sizeof(OS_CHAR));
			return sqlite3_bind_parameter_index(stmt, buf);
		}else{
			Core::Buffer buf(owner->os);
			buf.reserveCapacity((len+2) * sizeof(OS_CHAR));
			buf.buffer.buf[0] = OS_TEXT(':');
			OS_MEMCPY(buf.buffer.buf+1, name.toChar(), (len+1) * sizeof(OS_CHAR));
			return sqlite3_bind_parameter_index(stmt, (OS_CHAR*)buf.buffer.buf);
		}
	}
	return sqlite3_bind_parameter_index(stmt, name);
}

void SqliteOS::SqliteStatement::bindParams()
{
	OS_ASSERT(owner && stmt);
	OS * os = owner->os;

	if(os->isNull()){
		return;
	}

	os->getGlobal(OS_TEXT("Buffer"));
	int buffer_type_id = os->getValueId();
	OS_ASSERT(buffer_type_id);
	os->pop();

	if(!os->isObject() && !os->isArray()){
		triggerError(os, OS_TEXT("wrong bind parameters, object or array required"));
		return;
	}

	for(int i = 1; !os->isExceptionSet() && os->nextIteratorStep(); i++){
		int index = i;
		OS_EValueType type = os->getType(-2);
		if(type == OS_VALUE_TYPE_STRING){
			OS::String name = os->toString(-2);
			index = getParamIndex(name);
			if(!index){
				triggerError(os, OS::String::format(os, OS_TEXT("wrong sql parameter name: %s"), name.toChar()));
				return;
			}
		}else if(type != OS_VALUE_TYPE_NUMBER){
			triggerError(os, OS::String::format(os, OS_TEXT("wrong sql parameter name type: %s"), os->getTypeStr(-2).toChar()));
			return;
		}
		switch(os->getType(-1)){
		default:
			OS_ASSERT(false);

		case OS_VALUE_TYPE_NULL:
			checkError(sqlite3_bind_null(stmt, index));
			break;

		case OS_VALUE_TYPE_BOOL:
		case OS_VALUE_TYPE_NUMBER:
			checkError(sqlite3_bind_double(stmt, index, os->toDouble(-1)));
			break;

		case OS_VALUE_TYPE_OBJECT:
		case OS_VALUE_TYPE_USERDATA:
		case OS_VALUE_TYPE_USERPTR:
			{
				os->pushStackValue(-2);
				os->pushValueById(buffer_type_id);
				bool is_buffer = os->is();
				os->pop(2);
				if(is_buffer){
					OS::String buf = os->toString(-1);
					checkError(sqlite3_bind_blob(stmt, index, buf.toChar(), buf.getDataSize(), SQLITE_TRANSIENT));
					break;
				}
				// no break
			}

		case OS_VALUE_TYPE_STRING:
		case OS_VALUE_TYPE_ARRAY:
		case OS_VALUE_TYPE_FUNCTION:
		case OS_VALUE_TYPE_CFUNCTION:
			{
				OS::String buf = os->toString(-1);
				checkError(sqlite3_bind_text(stmt, index, buf.toChar(), buf.getDataSize(), SQLITE_TRANSIENT));
				break;
			}
		}
		os->pop(2);
	}
}

int SqliteOS::SqliteStatement::bind(OS * os, int params, int, int, void * user_param)
{
	OS_GET_SELF(SqliteStatement*);
	if(params < 1){
		triggerError(os, OS_TEXT("bind parameters required"));
		return 0;
	}
	os->pop(params - 1);
	self->bindParams();
	return 0;
}

int SqliteOS::SqliteStatement::execute(OS * os, int params, int, int, void * user_param)
{
	OS_GET_SELF(SqliteStatement*);
	if(params > 0){
		os->pop(params - 1);
		self->bindParams();
	}
	return self->step(EXECUTE);
}

int SqliteOS::SqliteStatement::fetch(OS * os, int params, int, int, void * user_param)
{
	OS_GET_SELF(SqliteStatement*);
	if(params > 0){
		os->pop(params - 1);
		self->bindParams();
	}
	return self->step(FETCH);
}

int SqliteOS::SqliteStatement::__iter(OS * os, int params, int, int, void * user_param)
{
	struct Lib
	{
		static int iterStep(OS * os, int params, int closure_values, int, void * user_param)
		{
			OS_ASSERT(params == 0 && closure_values == 1);
			SqliteStatement * self = CtypeValue< RemoveConst<SqliteStatement*>::type >::getArg(os, -1);
			OS_ASSERT(self);
			return self->step(ITERATE);
		}
	};

	OS_GET_SELF(SqliteStatement*);
	pushCtypeValue(os, self);
	os->pushCFunction(Lib::iterStep, 1);
	return 1;
}

int SqliteOS::Sqlite::getLastInsertId(OS * os, int params, int, int, void * user_param)
{
	OS_GET_SELF(Sqlite*);
	if(!self->db){
		triggerError(os, OS_TEXT("closed db"));
		return 0;
	}
	os->pushNumber(sqlite3_last_insert_rowid(self->db));
	return 1;
}

int SqliteOS::Sqlite::query(OS * os, int params, int, int, void * user_param)
{
	OS_GET_SELF(Sqlite*);
	if(params < 1){
		triggerError(os, OS_TEXT("sql query required"));
		return 0;
	}
	if(!self->db){
		triggerError(os, OS_TEXT("closed db"));
		return 0;
	}
	SqliteStatement * stmt = self->prepare(os->toString(-params+0));
	if(stmt){
		if(params >= 2){
			os->pop(params - 2);
			stmt->bindParams();
		}
		pushCtypeValue(os, stmt);
		return 1;
	}
	return 0;
}

void SqliteOS::initExtension(OS * os)
{
	{
		OS::FuncDef funcs[] = {
			{OS_TEXT("__newinstance"), Sqlite::__newinstance},
			{OS_TEXT("query"), Sqlite::query},
			{OS_TEXT("__get@lastInsertId"), Sqlite::getLastInsertId},
			{OS_TEXT("getLastInsertId"), Sqlite::getLastInsertId},
			{}
		};

		registerUserClass<Sqlite>(os, funcs);
	}
	{
		OS::FuncDef funcs[] = {
			// {OS_TEXT("__construct"), SqliteStatement::__construct},
			{OS_TEXT("__iter"), SqliteStatement::__iter},
			{OS_TEXT("bind"), SqliteStatement::bind},
			{OS_TEXT("execute"), SqliteStatement::execute},
			{OS_TEXT("fetch"), SqliteStatement::fetch},
			{}
		};

		registerUserClass<SqliteStatement>(os, funcs, NULL, false);
	}
	Sqlite::initScript(os);
}

void initSqlite3Extension(OS* os)
{
	SqliteOS::initExtension(os);
}

} // namespace ObjectScript
