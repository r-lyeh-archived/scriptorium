#ifndef WIN32
#include "os.config.h"
#endif
#include "os-odbo.h"
#include "../objectscript.h"
#include "../os-binder.h"

#include <soci/soci.h>

#ifdef WIN32
#include <soci/mysql/soci-mysql.h>
#include <soci/odbc/soci-odbc.h>
#else
#if defined(SOCI_MYSQL_FOUND) && SOCI_MYSQL_FOUND != 0
#include <soci/mysql/soci-mysql.h>
#endif
#if defined(SOCI_ODBC_FOUND) && SOCI_ODBC_FOUND != 0
#include <soci/odbc/soci-odbc.h>
#endif
#endif

#include <soci/soci-simple.h>

namespace ObjectScript {

class ODBO_OS: public OS
{
public:

	static void triggerError(OS * os, const OS::String& msg)
	{
		os->getGlobal(OS_TEXT("ODBOException"));
		os->pushGlobals();
		os->pushString(msg);
		os->callFT(1, 1);
		os->setException();
	}

	static void triggerError(OS * os, const char * msg)
	{
		os->getGlobal(OS_TEXT("ODBOException"));
		os->pushGlobals();
		os->pushString(msg);
		os->callFT(1, 1);
		os->setException();
	}

	struct ODBOStatement;
	struct ODBO
	{
		OS * os;
		soci::session * handle;
		ODBOStatement * list;
		OS::Core::String type;
		std::vector<soci::transaction*> transactions;
		int dateTimeId;

		ODBO(OS * p_os): type(p_os)
		{
			os = p_os;
			handle = NULL;
			list = NULL;

			/* os->getGlobal("ODBODateTime");
			dateTimeId[0] = os->getValueId();
			os->pop(); */

			os->getGlobal("DateTime");
			dateTimeId = os->getValueId();
			os->pop();
		}

		~ODBO()
		{
			while(list){
				removeStatement(list);
			}
			close();
		}

		OS::String getType() const { return OS::String(type); }

		void begin()
		{
			if(!handle){
				triggerError(os, "closed db");
				return;
			}
			soci::transaction * t = new soci::transaction(*handle);
			transactions.push_back(t);
		}

		void commit()
		{
			if(!transactions.size()){
				triggerError(os, "there is no open transation");
				return;
			}
			soci::transaction * t = transactions.back();
			transactions.pop_back();
			t->commit();
			delete t;
		}

		void rollback()
		{
			if(!transactions.size()){
				triggerError(os, "there is no open transation");
				return;
			}
			soci::transaction * t = transactions.back();
			transactions.pop_back();
			t->rollback();
			delete t;
		}

		bool isOpen() const { return handle != NULL; }

		void close()
		{
			while(transactions.size() > 0){
				commit();
			}
			if(handle){
				delete handle;
				// soci_destroy_session(handle);
				handle = NULL;
			}
		}

		bool open(const char * connection_string)
		{
			close();
			try {
				handle = new soci::session(connection_string);
			}catch(std::exception& e){
				triggerError(os, e.what()); // soci_session_error_message(handle));
			}
			return isOpen();
		}

		bool open(const soci::connection_parameters& p)
		{
			close();
			try {
				handle = new soci::session(p);
			}catch(std::exception& e){
				triggerError(os, e.what()); // soci_session_error_message(handle));
			}
			return isOpen();
		}

		bool findStatement(ODBOStatement * stmt);
		void addStatement(ODBOStatement * stmt);
		void removeStatement(ODBOStatement * stmt);

		ODBOStatement * prepare(const OS::Core::String& sql);

		static bool isValidOption(const char * s, int len, bool odbc)
		{
			for(int i = 0; i < len; i++){
				if(!s[i] || (odbc ? s[i] == ';' : s[i] <= ' ') || s[i] == '='){
					return false;
				}
			}
			return true;
		}

		static bool isValidOption(const OS::String& s, bool odbc)
		{
			return isValidOption(s, s.getLen(), odbc);
		}

		static int __newinstance(OS * os, int params, int, int, void * user_param)
		{
			if(params < 1){
				triggerError(os, OS_TEXT("driver parameter requied"));
				return 0;
			}
			ODBO * self = new (os->malloc(sizeof(ODBO) OS_DBG_FILEPOS)) ODBO(os);
			if(params >= 2 && os->isObject(-params+1)){
				self->type = os->toString(-params+0);
				bool odbc = self->type == "odbc";
				bool odbc_driver_complete = false;

				OS::Core::Buffer connection_str(os);
				// connection_str.append(self->type = os->toString(-params+0));
				// connection_str.append("://");
				if(params > 2){
					os->pop(params-2);
				}
				for(int i = 0; os->nextIteratorStep(); i++){
					OS::String key = os->toString(-2);
					OS::String value = os->toString(-1);

					if(!isValidOption(key, odbc) || !isValidOption(value, odbc)){
						triggerError(os, OS::String::format(os, "invalid char of option '%s=%s'", key.toChar(), value.toChar()));
						self->close();
						break;
					}
					if(i > 0){
						connection_str.append(odbc ? ";" : " ");
					}
					connection_str.append(key);
					connection_str.append("=");
					connection_str.append(value);

					os->pop(2);
				}
				if(!os->isExceptionSet()){
					try {
						soci::connection_parameters parameters(self->type.toChar(), connection_str.toString().toChar());
						if(odbc){
#if defined(SOCI_ODBC_FOUND) && SOCI_ODBC_FOUND != 0
							parameters.set_option(soci::odbc_option_driver_complete, odbc_driver_complete ? "1" : "0" /* SQL_DRIVER_NOPROMPT */);
#endif
						}
						self->open(parameters);
					}
					catch (std::exception& e){
						triggerError(os, e.what()); // soci_session_error_message(handle));
					}
				}
			}else{
				self->open(os->toString(-params+0));
			}
			if(self->isOpen()){
				pushCtypeValue(os, self);
				return 1;
			}
			self->~ODBO();
			os->free(self);
			return 0;
		}

		static int getLastInsertId(OS * os, int params, int, int, void * user_param);
		static int query(OS * os, int params, int, int, void * user_param);
	};

	struct ODBOStatement
	{
		struct BindValue
		{
			std::string str;
			std::tm tm;
			OS_NUMBER num;
			BindValue * next;

			BindValue(OS * os)
			{
				num = (OS_NUMBER)0;
				next = NULL;
			}
		};

		ODBO * owner;
		OS::Core::String sql;
		ODBOStatement * next;
		BindValue * bind_values;
		soci::statement * stmt;
		soci::row row;

		enum EStepCode {
			STEP_DONE,
			STEP_ROW,
			STEP_OK
		} step_code;

		int row_num;

		ODBOStatement(ODBO * p_owner, soci::statement * p_stmt, const OS::Core::String& p_sql): sql(p_sql)
		{
			OS_ASSERT(p_owner && p_stmt);
			owner = p_owner;
			next = NULL;
			stmt = p_stmt;
			bind_values = NULL;
			step_code = STEP_OK;
			row_num = 0;
			owner->addStatement(this);
		}

		~ODBOStatement()
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
				delete stmt; // soci_destroy_statement(stmt);
				stmt = NULL;
			}
			while(bind_values){
				BindValue * cur = bind_values;
				bind_values = cur->next;
				cur->next = NULL;
				cur->~BindValue();
				owner->os->free(cur);
			}
		}

		/* bool checkError(int err_code)
		{
			OS_ASSERT(owner);
			return ODBO_OS::checkError(owner->os, err_code);
		} */

		void addColumn(const std::string& name, OS_NUMBER value)
		{
			OS_ASSERT(owner);
			OS * os = owner->os;
			os->pushNumber(value);
			os->setProperty(-2, name.c_str(), false);
		}

		void addColumn(const std::string& name, const void * p_value, int size)
		{
			OS_ASSERT(owner);
			OS * os = owner->os;
			OS::Core::String value(os, p_value, size);
			os->pushString(value);
			os->setProperty(-2, name.c_str(), false);
		}

		void addColumn(const std::string& name, const std::tm& date)
		{
			OS_ASSERT(owner);
			OS * os = owner->os;

			os->pushValueById(owner->dateTimeId);
			os->pushGlobals();
			os->pushNumber(date.tm_year + 1900);
			os->pushNumber(date.tm_mon + 1);
			os->pushNumber(date.tm_mday);
			os->pushNumber(date.tm_hour);
			os->pushNumber(date.tm_min);
			os->pushNumber(date.tm_sec);
			os->callFT(6, 1);

			/*
			char value[32];
			sprintf(value, "%04d %02d %02d %02d %02d %02d", date.tm_year + 1900, date.tm_mon + 1,
				date.tm_mday, date.tm_hour, date.tm_min, date.tm_sec);
			os->pushString(value);
			*/

			os->setProperty(-2, name.c_str(), false);
		}

		void addColumn(const std::string& name, const std::string& value)
		{
			addColumn(name, value.c_str(), value.length());
		}

		void addColumn(const std::string& name)
		{
			OS_ASSERT(owner);
			OS * os = owner->os;
			os->pushNull();
			os->setProperty(-2, name.c_str(), false);
		}

		enum EStepType {
			ITERATE,
			FETCH,
			EXECUTE
		};

		int step(EStepType type)
		{
			OS_ASSERT(owner && (stmt || step_code == STEP_DONE));
			OS * os = owner->os;
			try{
				if(step_code == STEP_DONE || !stmt){
					return 0;
				}
				if(step_code == STEP_OK){
					if(type != EXECUTE){
						stmt->exchange(soci::into(row));
					}
					stmt->alloc();
					stmt->prepare(sql.toChar());
					stmt->define_and_bind();
					stmt->execute();
					if(type == EXECUTE){
						step_code = STEP_DONE;
						close();
						os->pushBool(true);
						return 1;
					}
				}
				step_code = stmt->fetch() ? STEP_ROW : STEP_DONE;
				if(step_code == STEP_ROW){
					row_num++;
					if(type == ITERATE){
						os->pushBool(true);
						os->pushNumber(row_num-1);
					}else if(type == EXECUTE){
						os->pushBool(true);
						return 1;
					}
					os->newObject();
					int cols = row.size(); // sqlite3_column_count(stmt);
					for(int i = 0; i < cols; i++){
						const soci::column_properties& props = row.get_properties(i);
						std::string name = props.get_name(); // sqlite3_column_name(stmt, i);
						if(row.get_indicator(i) == soci::i_null){
							addColumn(name);
							continue;
						}
						int type = props.get_data_type(); // sqlite3_column_type(stmt, i);
						switch(type){
						case soci::dt_double:
							addColumn(name, (OS_NUMBER)row.get<double>(i));
							break;

						case soci::dt_integer:
							addColumn(name, (OS_NUMBER)row.get<int>(i));
							break;

						case soci::dt_long_long:
							addColumn(name, (OS_NUMBER)row.get<long long>(i));
							break;

						case soci::dt_unsigned_long_long:
							addColumn(name, (OS_NUMBER)row.get<unsigned long long>(i));
							break;

						case soci::dt_string:
							addColumn(name, row.get<std::string>(i));
							break;

						case soci::dt_date:
							addColumn(name, row.get<std::tm>(i));
							break;

						default:
							OS_ASSERT(false);
							addColumn(name);
						}
					}
					return type == ITERATE ? 3 : 1;
				}
				if(step_code == STEP_DONE){
					close();
					return 0;
				}
				/* if(step_code != SQLITE_DONE){
					checkError(step_code);
				} */
			}catch(std::exception& e){
				triggerError(os, e.what());
			}
			return 0;
		}

		int getColumnCount()
		{
			OS_ASSERT(owner);
			return stmt ? row.size() : 0; // sqlite3_column_count(stmt);
		}

		int getColumnType(int col)
		{
			OS_ASSERT(owner);
			if(!stmt){
				return 0;
			}
			const soci::column_properties& props = row.get_properties(col);
			return props.get_data_type(); // sqlite3_column_type(stmt, col);
		}

		// int getParamIndex(const OS::String& name);
		void bindParams();

		/* static int __construct(OS * os, int params, int, int, void * user_param)
		{
			triggerError(os, OS_TEXT("you should not create new instance of ODBOStatement"));
			return 0;
		} */

		static int __iter(OS * os, int params, int, int, void * user_param);
		static int bind(OS * os, int params, int, int, void * user_param);
		static int execute(OS * os, int params, int, int, void * user_param);
		static int fetch(OS * os, int params, int, int, void * user_param);
	};

	static void initExtension(OS* os);

};

template <> struct CtypeName<ODBO_OS::ODBO>{ static const OS_CHAR * getName(){ return OS_TEXT("ODBO"); } };
template <> struct CtypeValue<ODBO_OS::ODBO*>: public CtypeUserClass<ODBO_OS::ODBO*>{};
template <> struct UserDataDestructor<ODBO_OS::ODBO>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<ODBO_OS::ODBO*>((ODBO_OS::ODBO*)data));
		ODBO_OS::ODBO * buf = (ODBO_OS::ODBO*)data;
		buf->~ODBO();
		os->free(buf);
	}
};

template <> struct CtypeName<ODBO_OS::ODBOStatement>{ static const OS_CHAR * getName(){ return OS_TEXT("ODBOStatement"); } };
template <> struct CtypeValue<ODBO_OS::ODBOStatement*>: public CtypeUserClass<ODBO_OS::ODBOStatement*>{};
template <> struct UserDataDestructor<ODBO_OS::ODBOStatement>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<ODBO_OS::ODBOStatement*>((ODBO_OS::ODBOStatement*)data));
		ODBO_OS::ODBOStatement * buf = (ODBO_OS::ODBOStatement*)data;
		buf->~ODBOStatement();
		os->free(buf);
	}
};

bool ODBO_OS::ODBO::findStatement(ODBOStatement * stmt)
{
	OS_ASSERT(stmt && stmt->owner == this);
	for(ODBOStatement * cur = list; cur; cur = cur->next){
		if(cur == stmt){
			return true;
		}
	}
	return false;
}

void ODBO_OS::ODBO::addStatement(ODBOStatement * stmt)
{
	OS_ASSERT(stmt && !stmt->next && !findStatement(stmt));
	stmt->next = list;
	list = stmt;
}

void ODBO_OS::ODBO::removeStatement(ODBOStatement * stmt)
{
	OS_ASSERT(stmt && stmt->owner == this);
	for(ODBOStatement * cur = list, * prev = NULL; cur; prev = cur, cur = cur->next){
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

/*
int ODBO_OS::ODBOStatement::getParamIndex(const OS::String& name)
{
	OS_CHAR c = name.toChar()[0];
	if(c != OS_TEXT(':') && c != OS_TEXT('$') && c != OS_TEXT('@')){
		int len = name.getLen();
		const int MAX_STACK_LEN = 64;
		if(len < MAX_STACK_LEN-2){
			OS_CHAR buf[MAX_STACK_LEN];
			buf[0] = OS_TEXT(':');
			OS_MEMCPY(buf+1, name.toChar(), (len+1) * sizeof(OS_CHAR));
			OS_ASSERT(false);
			return 0; // sqlite3_bind_parameter_index(stmt, buf);
		}else{
			Core::Buffer buf(owner->os);
			buf.reserveCapacity((len+2) * sizeof(OS_CHAR));
			buf.buffer.buf[0] = OS_TEXT(':');
			OS_MEMCPY(buf.buffer.buf+1, name.toChar(), (len+1) * sizeof(OS_CHAR));
			OS_ASSERT(false);
			return 0; // sqlite3_bind_parameter_index(stmt, (OS_CHAR*)buf.buffer.buf);
		}
	}
	OS_ASSERT(false);
	return 0; // sqlite3_bind_parameter_index(stmt, name);
}
*/

void ODBO_OS::ODBOStatement::bindParams()
{
	OS_ASSERT(owner);
	OS * os = owner->os;

	if(os->isNull()){
		return;
	}
	if(!stmt){
		triggerError(os, OS_TEXT("statement closed"));
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

	BindValue * value;
	soci::indicator ind = soci::i_null;
	for(int i = 1; !os->isExceptionSet() && os->nextIteratorStep(); i++){
		// int index = i;
		OS::String name(os);
		OS_EValueType type = os->getType(-2);
		if(type == OS_VALUE_TYPE_STRING){
			name = os->toString(-2);
		}else if(type != OS_VALUE_TYPE_NUMBER){
			triggerError(os, OS::String::format(os, OS_TEXT("wrong sql parameter name type: %s"), os->getTypeStr(-2).toChar()));
			return;
		}
		switch(os->getType(-1)){
		default:
			OS_ASSERT(false);

		case OS_VALUE_TYPE_NULL:
			// OS_ASSERT(false); // checkError(sqlite3_bind_null(stmt, index));
			value = new (owner->os->malloc(sizeof(*value) OS_DBG_FILEPOS)) BindValue(owner->os);
			value->next = bind_values;
			bind_values = value;
			if(name.isEmpty()){
				stmt->exchange(soci::use(value->str, ind));
			}else{
				stmt->exchange(soci::use(value->str, ind, name.toChar()));
			}
			break;

		case OS_VALUE_TYPE_BOOL:
		case OS_VALUE_TYPE_NUMBER:
			// OS_ASSERT(false); // checkError(sqlite3_bind_double(stmt, index, os->toDouble(-1)));
			value = new (owner->os->malloc(sizeof(*value) OS_DBG_FILEPOS)) BindValue(owner->os);
			value->num = os->toDouble(-1);
			value->next = bind_values;
			bind_values = value;
			if(name.isEmpty()){
				stmt->exchange(soci::use(value->num));
			}else{
				stmt->exchange(soci::use(value->num, name.toChar()));
			}
			break;

		case OS_VALUE_TYPE_OBJECT:
		case OS_VALUE_TYPE_USERDATA:
		case OS_VALUE_TYPE_USERPTR:
			if(1){
				os->pushValueById(owner->dateTimeId);
				bool is_date =  os->is(-2, -1);
				os->pop(1);
				/* if(!is_date){
					os->pushValueById(owner->dateTimeId[1]);
					is_date =  os->is(-2, -1);
					os->pop(1);
				} */
				if(is_date){
					std::tm tm;
					os->getProperty(-1, "year");	tm.tm_year = os->popInt()-1900;
					os->getProperty(-1, "month");	tm.tm_mon = os->popInt()-1;
					os->getProperty(-1, "day");		tm.tm_mday = os->popInt();
					os->getProperty(-1, "hour");	tm.tm_hour = os->popInt();
					os->getProperty(-1, "minute");	tm.tm_min = os->popInt();
					os->getProperty(-1, "second");	tm.tm_sec = os->popInt();
					tm.tm_wday = 0;
					tm.tm_yday = 0;
					tm.tm_isdst = -1;
					// std::mktime(&tm);
					value = new (owner->os->malloc(sizeof(*value) OS_DBG_FILEPOS)) BindValue(owner->os);
					value->tm = tm;
					value->next = bind_values;
					bind_values = value;
					if(name.isEmpty()){
						stmt->exchange(soci::use(value->tm));
					}else{
						stmt->exchange(soci::use(value->tm, name.toChar()));
					}
					// OS_ASSERT(false); // checkError(sqlite3_bind_text(stmt, index, buf.toChar(), buf.getDataSize(), SQLITE_TRANSIENT));
					break;
				}

				/*
				os->pushStackValue(-1);
				os->pushValueById(buffer_type_id);
				bool is_buffer = os->is();
				os->pop(2);
				if(is_buffer){
					value = new (owner->os->malloc(sizeof(*value) OS_DBG_FILEPOS)) BindValue(owner->os);
					value->str = os->toString(-1);
					value->next = bind_values;
					bind_values = value;
					if(name.isEmpty()){
						stmt->exchange(soci::use(value->str));
					}else{
						stmt->exchange(soci::use(value->str, name.toChar()));
					}
					// OS_ASSERT(false); // checkError(sqlite3_bind_blob(stmt, index, buf.toChar(), buf.getDataSize(), SQLITE_TRANSIENT));
					break;
				}
				*/
				// no break
			}

		case OS_VALUE_TYPE_STRING:
		case OS_VALUE_TYPE_ARRAY:
		case OS_VALUE_TYPE_FUNCTION:
		case OS_VALUE_TYPE_CFUNCTION:
			{
				value = new (owner->os->malloc(sizeof(*value) OS_DBG_FILEPOS)) BindValue(owner->os);
				value->str = os->toString(-1);
				value->next = bind_values;
				bind_values = value;
				if(name.isEmpty()){
					stmt->exchange(soci::use(value->str));
				}else{
					stmt->exchange(soci::use(value->str, name.toChar()));
				}
				// OS_ASSERT(false); // checkError(sqlite3_bind_text(stmt, index, buf.toChar(), buf.getDataSize(), SQLITE_TRANSIENT));
				break;
			}
		}
		os->pop(2);
	}
}

int ODBO_OS::ODBOStatement::bind(OS * os, int params, int, int, void * user_param)
{
	OS_GET_SELF(ODBOStatement*);
	if(params < 1){
		triggerError(os, OS_TEXT("bind parameters required"));
		return 0;
	}
	os->pop(params - 1);
	self->bindParams();
	return 0;
}

int ODBO_OS::ODBOStatement::execute(OS * os, int params, int, int, void * user_param)
{
	OS_GET_SELF(ODBOStatement*);
	if(params > 0){
		os->pop(params - 1);
		self->bindParams();
	}
	return self->step(EXECUTE);
}

int ODBO_OS::ODBOStatement::fetch(OS * os, int params, int, int, void * user_param)
{
	OS_GET_SELF(ODBOStatement*);
	if(params > 0){
		os->pop(params - 1);
		self->bindParams();
	}
	return self->step(FETCH);
}

int ODBO_OS::ODBOStatement::__iter(OS * os, int params, int, int, void * user_param)
{
	struct Lib
	{
		static int iterStep(OS * os, int params, int closure_values, int, void * user_param)
		{
			OS_ASSERT(params == 0 && closure_values == 1);
			// ODBOStatement * self = CtypeValue< RemoveConst<ODBOStatement*>::type >::getArg(os, -1);
			// OS_ASSERT(self);
			OS_GET_SELF(ODBOStatement*);
			return self->step(ITERATE);
		}
	};

	OS_GET_SELF(ODBOStatement*);
	pushCtypeValue(os, self);
	os->pushCFunction(Lib::iterStep, 1);
	return 1;
}

ODBO_OS::ODBOStatement * ODBO_OS::ODBO::prepare(const OS::Core::String& sql)
{
	OS_ASSERT(handle);
	if(!isOpen()){
		triggerError(os, "wrong connection");
		return NULL;
	}
	try{
		soci::statement * stmt = new soci::statement(*handle);
		return new (os->malloc(sizeof(ODBOStatement) OS_DBG_FILEPOS)) ODBOStatement(this, stmt, sql);
	}catch(std::exception& e){
		triggerError(os, e.what());
	}
	return NULL;
}

int ODBO_OS::ODBO::getLastInsertId(OS * os, int params, int, int, void * user_param)
{
	struct Lib {
		static void skipSpaces(const char *& str)
		{
			while(*str && OS_IS_SPACE(*str)){
				str++;
			}
		}
		static bool is(const char * str, const char * what)
		{
#ifdef _MSC_VER
			return _strnicmp(str, what, strlen(what)) == 0;
#else
			return strncasecmp(str, what, strlen(what)) == 0;
#endif
		}

		static std::string getTableName(ODBO * self)
		{
			const char * sql = self->list->sql.toChar();
			Lib::skipSpaces(sql);
			if(Lib::is(sql, "insert")){
				sql += strlen("insert");
				Lib::skipSpaces(sql);
				if(Lib::is(sql, "into")){
					sql += strlen("into");
					Lib::skipSpaces(sql);
					const char * table = sql;
					while(*sql && (OS_IS_ALPHA(*sql) || *sql == '_' || OS_IS_ALNUM(*sql))){
						sql++;
					}
					return std::string(table, sql - table);
				}
			}
			return std::string();
		}
	};

	OS_GET_SELF(ODBO*);
	if(!self->handle){
		triggerError(os, OS_TEXT("closed db"));
		return 0;
	}

	long value = -1;
	if(self->type == "mysql"){
		*self->handle << "select last_insert_id()", soci::into(value);
	}else if(self->type == "sqlite"){
		*self->handle << "select last_insert_rowid()", soci::into(value);
	}else if(self->type == "mssql"){
		std::string sequence_name = params > 0 ? os->toString(-params+0).toChar() : Lib::getTableName(self);
		*self->handle << ("select ident_current('" + sequence_name + "')"), soci::into(value);
	}else{
		std::string sequence_name = params > 0 ? os->toString(-params+0).toChar() : Lib::getTableName(self);
		self->handle->get_last_insert_id(sequence_name, value);
	}
	os->pushNumber(value); // sqlite3_last_insert_rowid(self->db));
	return 1;
}

int ODBO_OS::ODBO::query(OS * os, int params, int, int, void * user_param)
{
	OS_GET_SELF(ODBO*);
	if(params < 1){
		triggerError(os, OS_TEXT("sql query required"));
		return 0;
	}
	if(!self->handle){
		triggerError(os, OS_TEXT("closed db"));
		return 0;
	}
	ODBOStatement * stmt = self->prepare(os->toString(-params+0));
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

void ODBO_OS::initExtension(OS* os)
{
#ifdef WIN32
	soci::register_factory_mysql();
	soci::register_factory_odbc();
#else
#if defined(SOCI_MYSQL_FOUND) && SOCI_MYSQL_FOUND != 0
	soci::register_factory_mysql();
#endif
#if defined(SOCI_ODBC_FOUND) && SOCI_ODBC_FOUND != 0
	soci::register_factory_odbc();
#endif
#endif
	{
		OS::FuncDef funcs[] = {
			{OS_TEXT("__newinstance"), ODBO::__newinstance},
			{OS_TEXT("query"), ODBO::query},
			{OS_TEXT("__get@lastInsertId"), ODBO::getLastInsertId},
			{OS_TEXT("getLastInsertId"), ODBO::getLastInsertId},
			def(OS_TEXT("__get@type"), &ODBO::getType),
			def(OS_TEXT("begin"), &ODBO::begin),
			def(OS_TEXT("commit"), &ODBO::commit),
			def(OS_TEXT("rollback"), &ODBO::rollback),
			def(OS_TEXT("close"), &ODBO::close),
			{}
		};
		registerUserClass<ODBO>(os, funcs);
	}
	{
		OS::FuncDef funcs[] = {
			// {OS_TEXT("__construct"), ODBOStatement::__construct},
			{OS_TEXT("__iter"), ODBOStatement::__iter},
			{OS_TEXT("bind"), ODBOStatement::bind},
			{OS_TEXT("execute"), ODBOStatement::execute},
			{OS_TEXT("fetch"), ODBOStatement::fetch},
			{}
		};
		registerUserClass<ODBOStatement>(os, funcs, NULL, false);
	}
#define OS_AUTO_TEXT(...) OS_TEXT(#__VA_ARGS__)
	os->eval(OS_AUTO_TEXT(
		ODBOException = extends Exception {
		}
		function ODBOStatement.fetchAll(){
			var r = []
			for(var row; row = @fetch();){
				r.push(row)
			}
			return r
		}
		function ODBO.execute(sql, params){
			return @query(sql, params).execute()
		}
		function ODBO.fetch(sql, params){
			return @query(sql, params).fetch()
		}
		function ODBO.fetchAll(sql, params){
			return @query(sql, params).fetchAll()
		}
	));
}

void initODBOExtension(OS* os)
{
	ODBO_OS::initExtension(os);
}

} // namespace ObjectScript
