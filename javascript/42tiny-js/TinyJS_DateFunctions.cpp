/*
 * 42TinyJS
 *
 * A fork of TinyJS with the goal to makes a more JavaScript/ECMA compliant engine
 *
 * Authored By Armin Diedering <armin@diedering.de>
 *
 * Copyright (C) 2010-2015 ardisoft
 *
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <cstdlib>
#include <sstream>
#include <cstdio>
#include <iomanip>

#include <ctime>
#include "TinyJS.h"

#ifdef _WIN32
#	include <windows.h>
#elif defined(HAVE_GETTIMEOFDAY)
#	include <sys/time.h>
#endif


using namespace std;

#ifndef _MSC_VER

#define localtime_s(tm, time) localtime_r(time, tm)
#define gmtime_s(tm, time) gmtime_r(time, tm)
#define asctime_s(buf, bufsize, tm) asctime_r(tm, buf)
#define _get_timezone(pTimezone) do{*pTimezone=timezone;}while(0)
#define sprintf_s sprintf

#endif

////////////////////////////////////////////////////////////////////////// 
/// CScriptTime
//////////////////////////////////////////////////////////////////////////

class CScriptTime {
public:
	CScriptTime(bool isLocalTime=true);
	CScriptTime(int64_t Time, bool isLocalTime=true);

	static int64_t UTC(int32_t Year, int32_t Month, int32_t Day=1, int32_t Hour=0, int32_t Min=0, int32_t Sec=0, int32_t MSec=0);
	static int64_t now();
	static bool ParseISODate(const char *s, int64_t *result);
	static bool ParseDate(const char *s, int64_t *result);
	static bool Parse(const string &DateString, int64_t *result) { return ParseDate(DateString.c_str(), result); }

	bool isValid() { return tm_isvalid; }
	void setInvalide() { setTime(0); tm_isvalid= false; }

	int64_t setTime(int32_t Year, int32_t Month, int32_t Day=1, int32_t Hour=0, int32_t Min=0, int32_t Sec=0, int32_t MSec=0);
	int64_t setTime(int64_t Time);
	int64_t getTime() const					{ return tm_time; }
	operator int64_t() const				{ return getTime(); }

	int64_t setDate(int32_t Date)			{ tm_mday = Date; return fix(); }
	int32_t getDate() const					{ return tm_mday; }

	int64_t setUTCDate(int32_t Date)		{ return setTime(CScriptTime(*this, false).setDate(Date)); }
	int32_t getUTCDate() const				{ return CScriptTime(*this, false).tm_mday; }


	int32_t getDay() const					{ return tm_wday; }
	int32_t getUTCDay() const				{ return CScriptTime(*this, false).tm_wday; }

	int64_t setYear(int32_t Year);
	int32_t getYear() const					{ return tm_year - 1900; }

	int64_t setFullYear(int32_t Year);
	int64_t setFullYear(int32_t Year, int32_t Month);
	int64_t setFullYear(int32_t Year, int32_t Month, int32_t Day);
	int32_t getFullYear() const													{ return tm_year; }

	int64_t setUTCFullYear(int32_t Year)										{ return setTime(CScriptTime(*this, false).setFullYear(Year)); }
	int64_t setUTCFullYear(int32_t Year, int32_t Month)					{ return setTime(CScriptTime(*this, false).setFullYear(Year, Month)); }
	int64_t setUTCFullYear(int32_t Year, int32_t Month, int32_t Day)	{ return setTime(CScriptTime(*this, false).setFullYear(Year, Month, Day)); }
	int32_t getUTCFullYear() const												{ return CScriptTime(*this, false).tm_year; }


	int64_t setHours(int32_t Hour)				{ tm_hour = Hour; return fix(); }
	int32_t getHours() const						{ return tm_hour; }
	int64_t setUTCHours(int32_t Hour)			{ return setTime(CScriptTime(*this, false).setHours(Hour)); }
	int32_t getUTCHours() const					{ return CScriptTime(*this, false).tm_hour; }

	int64_t setMilliseconds(int32_t Msec)		{ tm_msec = Msec; return fix(); }
	int32_t getMilliseconds() const				{ return tm_msec; }
	int64_t setUTCMilliseconds(int32_t Msec)	{ return setTime(CScriptTime(*this, false).setMilliseconds(Msec)); }
	int32_t getUTCMilliseconds() const			{ return CScriptTime(*this, false).tm_msec; }

	int64_t setMinutes(int32_t Min)				{ tm_min = Min; return fix(); }
	int32_t getMinutes() const						{ return tm_min; }
	int64_t setUTCMinutes(int32_t Min)			{ return setTime(CScriptTime(*this, false).setMinutes(Min)); }
	int32_t getUTCMinutes() const					{ return CScriptTime(*this, false).tm_min; }

	int64_t setMonth(int32_t Month)				{ tm_mon = Month; return fix(); }
	int32_t getMonth() const						{ return tm_mon; }
	int64_t setUTCMonth(int32_t Month)			{ return setTime(CScriptTime(*this, false).setMonth(Month)); }
	int32_t getUTCMonth() const					{ return CScriptTime(*this, false).tm_mon; }

	int64_t setSeconds(int32_t Sec)				{ tm_sec = Sec; return fix(); }
	int32_t getSeconds() const						{ return tm_sec; }
	int64_t setUTCSeconds(int32_t Sec)			{ return setTime(CScriptTime(*this, false).setSeconds(Sec)); }
	int32_t getUTCSeconds() const					{ return CScriptTime(*this, false).tm_sec; }

	int32_t getTimezoneOffset()					{ return tm_offset/60; }

	string toDateString() const;
	string toTimeString() const;

	string castToString() const;
	operator string() const							{ return castToString(); }
	string toUTCString() const						{ return CScriptTime(*this, false).castToString(); } // corresponds toGMTString()
	string toISOString() const;					// corresponds toJSON()



	static int32_t leapsThruEndOf(int32_t y) { return (y+4)/4 - y/100 + y/400; }
	static int32_t isLeapYear(int32_t y) { return (!(y % 4) && (y % 100)) || !(y % 400); }
	static const int32_t monthLengths[2][12];
	static const int32_t firstDayOfMonth[2][13];
private:
	bool tm_islocal;
	bool tm_isvalid;
	int32_t tm_offset;  /* offset to UTC time */
	int32_t tm_msec;
	int32_t tm_sec;     /* seconds after the minute - [0,59] */
	int32_t tm_min;     /* minutes after the hour - [0,59] */
	int32_t tm_hour;    /* hours since midnight - [0,23] */
	int32_t tm_mday;    /* day of the month - [1,31] */
	int32_t tm_mon;     /* months since January - [0,11] */
	int32_t tm_year;    /* years since 0000 */
	int32_t tm_wday;    /* days since Sunday - [0,6] */
	int32_t tm_yday;    /* days since January 1 - [0,365] */
	//	int32_t tm_isdst;   /* daylight savings time flag */
	int64_t tm_time;
	int64_t fix();
};

const int32_t CScriptTime::monthLengths[2][12] = {
	{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
	{31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
};
const int32_t CScriptTime::firstDayOfMonth[2][13] = {
	{0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365},
	{0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366}
};

CScriptTime::CScriptTime(bool isLocalTime/*=true*/) : tm_islocal(isLocalTime), tm_isvalid(true) {
	setTime(0);
}
CScriptTime::CScriptTime(int64_t Time, bool isLocalTime/*=true*/) : tm_islocal(isLocalTime), tm_isvalid(true) {
	setTime(Time);
}
int64_t CScriptTime::setTime(int64_t Time) {
	long tz, prevent_endless_loop=0;
	_get_timezone(&tz);
	tm_time = Time-tz*1000;
recalc:
	tm_time += 62167219200000;
	tm_mon = tm_year = tm_wday = tm_yday = 0;
	tm_mday = (int32_t)(tm_time/(24*60*60*1000));
	tm_time -= ((int64_t)tm_mday++)*24*60*60*1000;
	tm_hour = (int32_t)(tm_time/(60*60*1000));
	tm_time -= tm_hour*60*60*1000;
	tm_min = (int32_t)(tm_time/60000);
	tm_time -= tm_min*60000;
	tm_sec = (int32_t)(tm_time/1000);
	tm_time -= tm_sec*1000;
	tm_msec = (int32_t)tm_time;
	fix();
	if(tm_time != Time && prevent_endless_loop++ == 0) {
		tm_time = Time + (Time-tm_time) - tz*1000;
		goto recalc;
	}
	return tm_time;
}

int64_t CScriptTime::setTime( int32_t Year, int32_t Month, int32_t Day, int32_t Hour, int32_t Min, int32_t Sec, int32_t MSec ) { 
	if(0 <= Year && Year<=99) Year += 1900;
	tm_msec	= MSec;
	tm_sec	= Sec;
	tm_min	= Min;
	tm_hour	= Hour;
	tm_mday	= Day;
	tm_mon	= Month;
	tm_year	= Year;
	return fix();
}
int64_t CScriptTime::UTC(int32_t Year, int32_t Month, int32_t Day/*=1*/, int32_t Hour/*=0*/, int32_t Min/*=0*/, int32_t Sec/*=0*/, int32_t MSec/*=0*/) {
	return CScriptTime(false).setTime(Year, Month, Day, Hour, Min, Sec, MSec);
}

int64_t CScriptTime::now() {
#ifdef _WIN32
	FILETIME ft;
	int64_t tmpres = 0;
	memset(&ft, 0, sizeof(ft));

	GetSystemTimeAsFileTime(&ft);

	tmpres = ft.dwHighDateTime;
	tmpres <<= 32;
	tmpres |= ft.dwLowDateTime;

	/*converting file time to unix epoch*/
	tmpres /= 10000;  /*convert into milliseconds*/
	return tmpres - 11644473600000LL; 
#else /*_WIN32*/
#	ifdef HAVE_GETTIMEOFDAY
	timeval tv;
	gettimeofday(&tv, NULL); // if gettimeofday not available unset HAVE_GETTIMEOFDAY in config.h
	return int64_t(tv.tv_sec)*1000 + tv.tv_usec/1000;
#	else /*HAVE_GETTIMEOFDAY*/
	time_t t; 
	return 1000LL * time(&t);
#	endif /*HAVE_GETTIMEOFDAY*/
#endif /*_WIN32*/

}

bool CScriptTime::ParseISODate(const char *s, int64_t *result) {
	char *end;
	int32_t yearMul = 1, tzMul = 1;
	int32_t year=1970, month=1, day=1, hour=0, min=0, sec=0, msec=0;
	int32_t tzHour=0, tzMin=0;
	bool isLocalTime = false;

	if(*s == '+' || *s == '-') {
		if(*s++ == '-') yearMul = -1;
		if('0' > *s || *s > '9') return false;
		year = strtol(s, &end, 10); if(end-s != 6) return false; else s = end;
	} else if(*s != 'T') {
		if('0' > *s || *s > '9') return false;
		year = strtol(s, &end, 10); if(end-s != 4) return false; else s = end;
	}
	if(*s == '-') {
		++s; 
		if('0' > *s || *s > '9') return false;
		month = strtol(s, &end, 10); if(end-s != 2) return false; else s = end;
		if(*s == '-') {
			++s; 
			if('0' > *s || *s > '9') return false;
			day = strtol(s, &end, 10); if(end-s != 2) return false; else s = end;
		}
	}
	if(*s && *s++ == 'T') {
		if('0' > *s || *s > '9') return false;
		hour = strtol(s, &end, 10); if(end-s != 2) return false; else s = end;
		if(*s == 0 || *s++ != ':') return false;
		if('0' > *s || *s > '9') return false;
		min = strtol(s, &end, 10); if(end-s != 2) return false; else s = end;
		if(*s == ':') {
			++s;
			if('0' > *s || *s > '9') return false;
			sec = strtol(s, &end, 10); if(end-s != 2) return false; else s = end;
			if(*s == '.') {
				if('0' > s[1] || s[1] > '9') return false;
				msec = int32_t(strtod (s, (char**)&s) * 1000);
			}
		}
		if(*s == 'Z')
			++s;
		else if(*s == '-' || *s == '+') {
			if(*s++ == '-') tzMul = -1;
			if('0' > *s || *s > '9') return false;
			tzHour = (*s++-'0') * 10;
			if('0' > *s || *s > '9') return false;
			tzHour += *s++-'0';
			if(*s == ':') ++s;
			if('0' > *s || *s > '9') return false;
			tzMin = (*s++-'0') * 10;
			if('0' > *s || *s > '9') return false;
			tzMin += *s++-'0';
		} else
			isLocalTime = true;
	}
	if(*s || year > 275943 // ceil(1e8/365) + 1970
		|| (month == 0 || month > 12)
		|| (day == 0 || day > monthLengths[isLeapYear(year)][month])
		|| hour > 24 || ((hour == 24) && (min > 0 || sec > 0))
		|| min > 59 || sec > 59 || tzHour > 23 || tzMin > 59) return false;
	month -= 1; /* convert month to 0-based */
	*result =  CScriptTime(isLocalTime).setTime(yearMul*year, month, day, hour, min, sec, msec);
	if(!isLocalTime)
		*result -= tzMul * ((tzHour * 60 + tzMin) * 60 * 1000);
	return true;
}
bool CScriptTime::ParseDate(const char *s, int64_t *result)
{
	static struct{ const char* key; int32_t value; } keywords[] = {
		{ "AM", -1}, { "PM", -2},
		{ "MONDAY", 0}, { "TUESDAY", 0}, { "WEDNESAY", 0}, { "THURSDAY", 0}, { "FRIDAY", 0}, { "SATURDAY", 0}, { "SUNDAY", 0}, 
		{ "JANUARY", 1}, { "FEBRUARY", 2}, { "MARCH", 3}, { "APRIL", 4}, { "MAY", 5}, { "JUNE", 6}, 
		{ "JULY", 7}, { "AUGUST", 8}, { "SEPTEMBER", 9}, { "OCTOBER", 10}, { "NOVEMBER", 11}, { "DECEMBER", 12}, 
		{ "GMT", 10000+0}, { "UT", 10000+0}, { "UTC", 10000+0},
		{ "CEST", 10000-2*60}, { "CET", 10000-1*60},
		{ "EST", 10000+5*60}, { "EDT", 10000+4*60},
		{ "CST", 10000+6*60}, { "CDT", 10000+5*60},
		{ "MST", 10000+7*60}, { "MDT", 10000+6*60},
		{ "PST", 10000+8*60}, { "PDT", 10000+7*60},
		{ 0, 0}
	};

	if (ParseISODate(s, result))
		return true;

	int32_t year = -1, mon = -1, mday = -1, hour = -1, min = -1, sec = -1, tzOffset = -1;
	int32_t prevc = 0;
	bool seenPlusMinus = false;
	bool seenMonthName = false;
	while(*s) {
		int c = *s++;
		if (c <= ' ' || c == ',' || c == '-') {
			if (c == '-' && '0' <= *s && *s <= '9')
				prevc = c;
			continue;
		}
		if (c == '(') { /* comments) */
			int depth = 1;
			while(*s) {
				c = *s++;
				if (c == '(') {
					depth++;
				} else if (c == ')') {
					if (--depth <= 0)
						break;
				}
			}
			continue;
		}
		if ('0' <= c && c <= '9') {
			int n = c - '0';
			while (*s && '0' <= (c = *s++) && c <= '9') {
				n = n * 10 + c - '0';
			}

			/*
			* Allow TZA before the year, so 'Wed Nov 05 21:49:11 GMT-0800 1997'
			* works.
			*
			* Uses of seenPlusMinus allow ':' in TZA, so Java no-timezone style
			* of GMT+4:30 works.
			*/

			if ((prevc == '+' || prevc == '-')/*  && year>=0 */) {
				/* Make ':' case below change tzOffset. */
				seenPlusMinus = true;

				/* offset */
				if (n < 24)
					n = n * 60; /* EG. "GMT-3" */
				else
					n = n % 100 + n / 100 * 60; /* eg "GMT-0430" */

				if (prevc == '+')       /* plus means east of GMT */
					n = -n;

				if (tzOffset != 0 && tzOffset != -1)
					return false;

				tzOffset = n;
			} else if (prevc == '/' && mon >= 0 && mday >= 0 && year < 0) {
				if (c <= ' ' || c == ',' || c == '/' || *s==0)
					year = n;
				else
					return false;
			} else if (c == ':') {
				if (hour < 0)
					hour = /*byte*/ n;
				else if (min < 0)
					min = /*byte*/ n;
				else
					return false;
			} else if (c == '/') {
				/*
				* Until it is determined that mon is the actual month, keep
				* it as 1-based rather than 0-based.
				*/
				if (mon < 0)
					mon = /*byte*/ n;
				else if (mday < 0)
					mday = /*byte*/ n;
				else
					return false;
			} else if (*s && c != ',' && c > ' ' && c != '-' && c != '(') {
				return false;
			} else if (seenPlusMinus && n < 60) {  /* handle GMT-3:30 */
				if (tzOffset < 0)
					tzOffset -= n;
				else
					tzOffset += n;
			} else if (hour >= 0 && min < 0) {
				min = /*byte*/ n;
			} else if (prevc == ':' && min >= 0 && sec < 0) {
				sec = /*byte*/ n;
			} else if (mon < 0) {
				mon = /*byte*/n;
			} else if (mon >= 0 && mday < 0) {
				mday = /*byte*/ n;
			} else if (mon >= 0 && mday >= 0 && year < 0) {
				year = n;
			} else {
				return false;
			}
			prevc = 0;
		} else if (c == '/' || c == ':' || c == '+' || c == '-') {
			prevc = c;
		} else {
			const char *st = s-1;
			int k;
			while (*s) {
				if (!(('A' <= *s && *s <= 'Z') || ('a' <= *s && *s <= 'z')))
					break;
				++s;
			}

			if (s <= st + 1)
				return false;
			for(k=0; keywords[k].key; ++k) {
				const char *cmp_l = st, *cmp_r = keywords[k].key;
				while(cmp_l < s && toupper(*cmp_l++) == *cmp_r++);
				if(cmp_l == s) {
					int32_t action = keywords[k].value;
					if (action != 0) {
						if (action < 0) {
							/*
							* AM/PM. Count 12:30 AM as 00:30, 12:30 PM as
							* 12:30, instead of blindly adding 12 if PM.
							*/
							if (hour > 12 || hour < 0)
								return false;

							if (action == -1 && hour == 12) /* am */
								hour = 0;
							else if (action == -2 && hour != 12) /* pm */
								hour += 12;
						} else if (action <= 13) { /* month! */
							/*
							* Adjust mon to be 1-based until the final values
							* for mon, mday and year are adjusted below.
							*/
							if (seenMonthName)
								return false;

							seenMonthName = true;

							if (mon < 0) {
								mon = action;
							} else if (mday < 0) {
								mday = mon;
								mon = action;
							} else if (year < 0) {
								year = mon;
								mon = action;
							} else {
								return false;
							}
						} else {
							tzOffset = action - 10000;
						}
					}
					break;
				}
			}

			if (keywords[k].key == 0)
				return false;

			prevc = 0;
		}
	}

	if (year < 0 || mon < 0 || mday < 0)
		return false;

	/*
	 * Case 1. The input string contains an English month name.
	 *         The form of the string can be month f l, or f month l, or
	 *         f l month which each evaluate to the same date.
	 *         If f and l are both greater than or equal to 70, or
	 *         both less than 70, the date is invalid.
	 *         The year is taken to be the greater of the values f, l.
	 *         If the year is greater than or equal to 70 and less than 100,
	 *         it is considered to be the number of years after 1900.
	 * Case 2. The input string is of the form "f/m/l" where f, m and l are
	 *         integers, e.g. 7/16/45.
	 *         Adjust the mon, mday and year values to achieve 100% MSIE
	 *         compatibility.
	 *         a. If 0 <= f < 70, f/m/l is interpreted as month/day/year.
	 *            i.  If year < 100, it is the number of years after 1900
	 *            ii. If year >= 100, it is the number of years after 0.
	 *         b. If 70 <= f < 100
	 *            i.  If m < 70, f/m/l is interpreted as
	 *                year/month/day where year is the number of years after
	 *                1900.
	 *            ii. If m >= 70, the date is invalid.
	 *         c. If f >= 100
	 *            i.  If m < 70, f/m/l is interpreted as
	 *                year/month/day where year is the number of years after 0.
	 *            ii. If m >= 70, the date is invalid.
	 */
	if (seenMonthName) {
		if ((mday >= 70 && year >= 70) || (mday < 70 && year < 70))
			return false;

		if (mday > year) {
			int temp = year;
			year = mday;
			mday = temp;
		}
		if (year >= 70 && year < 100) {
			year += 1900;
		}
	} else if (mon < 70) { /* (a) month/day/year */
		if (year < 100) {
			year += 1900;
		}
	} else if (mon < 100) { /* (b) year/month/day */
		if (mday < 70) {
			int temp = year;
			year = mon + 1900;
			mon = mday;
			mday = temp;
		} else {
			return false;
		}
	} else { /* (c) year/month/day */
		if (mday < 70) {
			int temp = year;
			year = mon;
			mon = mday;
			mday = temp;
		} else {
			return false;
		}
	}

	mon -= 1; /* convert month to 0-based */
	if (sec < 0)
		sec = 0;
	if (min < 0)
		min = 0;
	if (hour < 0)
		hour = 0;

	*result =  CScriptTime(tzOffset == -1).setTime(year, mon, mday, hour, min, sec);
	if(tzOffset != -1)
		*result += tzOffset * 60 * 1000;
//    double msec = date_msecFromDate(year, mon, mday, hour, min, sec, 0);

//    if (tzOffset == -1) /* no time zone specified, have to use local */
//       msec = UTC(msec, dtInfo);
//    else
//        msec += tzOffset * msPerMinute;

//    *result = msec;
	 return true;
}

static const char *day_names[] = {"Sun","Mon","Tue","Wed","Thu","Fri","Sat"};
static const char *month_names[] = {"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"};
string CScriptTime::toDateString() const {
	char buffer[100];
	sprintf_s(buffer, "%s %s %02d %04d", day_names[tm_wday], month_names[tm_mon], tm_mday, tm_year);
	return buffer;
}
string CScriptTime::toTimeString() const {
	char buffer[100];
	const char *sign;
	long offset;
	if(tm_offset < 0) { sign = "+"; offset = -tm_offset/60; } 
	else { sign = "-"; offset = tm_offset/60; }
	offset =	(tm_offset/60)*100+tm_offset%60;
	sprintf_s(buffer, "%02d:%02d:%02d GMT%s%04ld", tm_hour, tm_min, tm_sec, sign, offset);
	return buffer;
}
string CScriptTime::castToString() const {
	char buffer[100];
	const char *sign;
	long offset;
	if(tm_offset < 0) { sign = "+"; offset = -tm_offset/60; } 
	else { sign = "-"; offset = tm_offset/60; }
	offset =	(offset/60)*100+offset%60;
	if(tm_islocal)
		sprintf_s(buffer, "%s %s %02d %04d %02d:%02d:%02d GMT%s%04ld", day_names[tm_wday], month_names[tm_mon], tm_mday, tm_year, tm_hour, tm_min, tm_sec, sign, offset);
	else
		sprintf_s(buffer, "%s, %02d %s %04d %02d:%02d:%02d GMT", day_names[tm_wday], tm_mday, month_names[tm_mon], tm_year, tm_hour, tm_min, tm_sec);
	return buffer;
}
string CScriptTime::toISOString() const {
	char buffer[100];
	CScriptTime utc(*this, false);
	sprintf_s(buffer, "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ", utc.tm_year, utc.tm_mon+1, utc.tm_mday, utc.tm_hour, utc.tm_min, utc.tm_sec, utc.tm_msec);
	return buffer;
}


int64_t CScriptTime::setYear( int32_t Year ) {
	if(0 <= Year && Year<=99) Year += 1900;
	tm_year = Year;
	return fix();
}
int64_t CScriptTime::setFullYear( int32_t Year ) {
	tm_year	= Year;
	return fix();
}
int64_t CScriptTime::setFullYear( int32_t Year, int32_t Month ) {
	tm_mon	= Month;
	tm_year	= Year;
	return fix();
}

int64_t CScriptTime::setFullYear( int32_t Year, int32_t Month, int32_t Day ) {
	tm_mday	= Day;
	tm_mon	= Month;
	tm_year	= Year;
	return fix();
}



int64_t CScriptTime::fix()
{
	int prevent_endless_loop = 0;
recalc:
	int64_t time_of_year;
	tm_offset = 0;
	tm_time = -62167219200000;
	if(tm_msec >= 1000) {
		tm_sec += tm_msec / 1000;
		tm_msec %= 1000;
	} else if(tm_msec < 0) {
		tm_sec += (tm_msec / 1000) - 1;
		tm_msec = (tm_msec % 1000) + 1000;
	}
	tm_time += tm_msec;
	if(tm_sec >= 60) {
		tm_min += tm_sec / 60;
		tm_sec %= 60;
	} else if(tm_sec < 0) {
		tm_min += (tm_sec / 60) - 1;
		tm_sec = (tm_sec % 60) + 60;
	}
	tm_time += tm_sec * 1000;
	if(tm_min >= 60) {
		tm_hour += tm_min / 60;
		tm_min %= 60;
	} else if(tm_min < 0) {
		tm_hour += (tm_min / 60) - 1;
		tm_min = (tm_min % 60) + 60;
	}
	tm_time += tm_min * 60 * 1000;

	int days = tm_mday-1;
	if(tm_hour >= 24) {
		days += tm_hour / 24;
		tm_hour %= 24;
	} else if(tm_hour < 0) {
		days += (tm_hour / 24) - 1;
		tm_hour = (tm_hour % 24) + 24;
	}
	time_of_year = (tm_time += tm_hour * 60 * 60 * 1000) + 62167219200000;
	if(tm_mon >= 12) {
		tm_year += tm_mon / 12;
		tm_mon = tm_mon % 12;
	} else if(tm_mon < 0) {
		tm_year += (tm_mon / 12) - 1;
		tm_mon = (tm_mon % 12) + 12;
	}
	days += tm_year*365 + leapsThruEndOf(tm_year-1);

	//	for(int i=0, isLeap=isLeapYear(tm_year) ; i<tm_mon; ++i)
	//		days += monthLengths[isLeap][i];
	days += firstDayOfMonth[isLeapYear(tm_year)][tm_mon];

	tm_time += int64_t(days) * 24 * 60 * 60 * 1000;
	tm_wday = (days+6) % 7;

	tm_year = days / 365;
	days %= 365;
	days -= leapsThruEndOf(tm_year-1);
	while(days < 0) {
		tm_year -= 1;
		days += 365 + isLeapYear(tm_year);
	}
	time_of_year += int64_t(tm_yday = days) * 24 * 60 * 60 * 1000;
	int month, isLeap; 
	for (month = 0, isLeap=isLeapYear(tm_year); month < 11; month++) {
		int newdays = days - monthLengths[isLeap][month];
		if (newdays < 0)
			break;
		days = newdays;
	}
	tm_mon = month;
	tm_mday = days + 1;
	if(tm_islocal) {
		time_t time4dst;
		if(tm_time < 0.0 || tm_time > 2145916800000) {
			static const time_t yearStartingWith[2][7] = {
				{252460800 /*1978*/, 94694400 /*1973*/, 126230400 /*1974*/, 157766400 /*1975*/, 347155200 /*1981*/, 31536000 /*1971*/, 220924800 /*1977*/},
				{441763200 /*1984*/, 820454400 /*1996*/, 315532800 /*1980*/, 694224000 /*1992*/, 189302400 /*1976*/, 567993600 /*1988*/, 63072000 /*1972*/}
			};
			int32_t days = tm_year*365 + leapsThruEndOf(tm_year-1);
			time4dst = yearStartingWith[isLeapYear(tm_year)][(days+6) % 7] + time_of_year/1000;

		} else
			time4dst = time_t(tm_time/1000);
		long tz; _get_timezone(&tz);
		struct tm tm;
		tm_offset = tz;
		time4dst += tm_offset;
		localtime_s(&tm, &time4dst); 
		if(tm.tm_isdst) {
			tm.tm_isdst = 0;
			tm_offset += int32_t(time4dst - mktime(&tm));
			time4dst += tm_offset - tz;
			localtime_s(&tm, &time4dst);
			if(!tm.tm_isdst && prevent_endless_loop++ == 0) {
				tm_sec -= tm_offset - tz;
				goto recalc;
			}
		}
	}
	tm_isvalid = true;
	return tm_time+=tm_offset*1000;
}



////////////////////////////////////////////////////////////////////////// 
/// CScriptVarDate
//////////////////////////////////////////////////////////////////////////

define_ScriptVarPtr_Type(Date);
class CScriptVarDate : public CScriptVarObject, public CScriptTime {
protected:
	CScriptVarDate(CTinyJS *Context);
	CScriptVarDate(const CScriptVarDate &Copy) : CScriptVarObject(Copy) {} ///< Copy protected -> use clone for public
public:
	virtual ~CScriptVarDate();
	virtual CScriptVarPtr clone();
	virtual bool isDate(); // { return true; }
	virtual CScriptVarPrimitivePtr toPrimitive(CScriptResult &execute); 

	virtual CScriptVarPtr toString_CallBack(CScriptResult &execute, int radix=0);
	virtual CScriptVarPtr valueOf_CallBack();

	friend inline define_newScriptVar_NamedFnc(Date, CTinyJS *Context);
private:
};
inline define_newScriptVar_NamedFnc(Date, CTinyJS *Context) { return new CScriptVarDate(Context); }

////////////////////////////////////////////////////////////////////////// 
// CScriptVarDate
//////////////////////////////////////////////////////////////////////////


CScriptVarDate::CScriptVarDate(CTinyJS *Context) : CScriptVarObject(Context, Context->getRoot()->findChildByPath("Date.prototype")) {
	
}

CScriptVarDate::~CScriptVarDate() {}
CScriptVarPtr CScriptVarDate::clone() { return new CScriptVarDate(*this); }
bool CScriptVarDate::isDate() { return true; }

CScriptVarPrimitivePtr CScriptVarDate::toPrimitive(CScriptResult &execute) {
	// for the Date Object toPrimitive without hint is hintString instead hintNumber
	return toPrimitive_hintString(execute);
}
CScriptVarPtr CScriptVarDate::toString_CallBack( CScriptResult &execute, int radix/*=0*/ ) {
	if(isValid()) {
		return newScriptVar(castToString());
	} else
		return newScriptVar("Invalid Date");
}
CScriptVarPtr CScriptVarDate::valueOf_CallBack() { 
	if(isValid())
		return newScriptVar((double)getTime());
	else
		return constScriptVar(NaN); 
}

void test() {
	CScriptTime tm;
	tm.setTime(1970, 2, 29, 1, 59, 59, 999);
	tm.setTime(tm.getTime());
	tm.setTime(1970, 2, 29, 2);
	tm.setTime(tm.getTime());
	tm.setTime(1970, 2, 29, 2, 59, 59);
	tm.setTime(tm.getTime());
	string s = tm.castToString();
	tm.setTime(1970, 3, 29, 3);
	tm.setTime(tm.getTime());
	tm.setTime(1970, 9, 25, 1, 59, 59, 999);
	tm.setTime(tm.getTime());
	tm.setTime(1970, 9, 25, 2);
	tm.setTime(tm.getTime());
	tm.setTime(1970, 9, 25, 2, 59, 59);
	tm.setTime(tm.getTime());
	tm.setTime(1970, 9, 25, 3);
	tm.setTime(tm.getTime());
	do{}while(0);
#if 0
	time_t end_time;
	time(&end_time);

	int last_year = -1;
	for(time_t t = -60*60*24*365; t<end_time; t+=30) {
		struct tm gmt, my;
		time_t _t = t+3600;
		gmtime_s(&gmt, &_t);
//		time2tm(((double)t)*1000+3600000, &my);
		my.tm_yday = gmt.tm_yday;
		if(memcmp(&gmt, &my, sizeof(gmt)))
			do{}while(0);
		if(last_year != gmt.tm_year) {
			printf("%d\n", gmt.tm_year+1900);
			last_year = gmt.tm_year;
		}

	}
#endif
}


static void scDate(const CFunctionsScopePtr &c, void *data) {
	test();
	c->setReturnVar(c->newScriptVar(CScriptTime(CScriptTime::now()).castToString()));
}

static void scDate_Constructor(const CFunctionsScopePtr &c, void *data) {
	CScriptVarDatePtr returnVar = ::newScriptVarDate(c->getContext());
	c->setReturnVar(returnVar);
	int ArgumentsLength = c->getArgumentsLength();
	if(ArgumentsLength == 1) {
		CScriptVarPtr arg0 = c->getArgument(0);
		CNumber v = arg0->toNumber().floor();
		if(v.isFinite()) {
			returnVar->setTime((int64_t)v.toDouble()); // TODO check range
		} else {
			int64_t result;
			if(CScriptVarDate::Parse(arg0->toString(), &result))
				returnVar->setTime(result);
			else
				returnVar->setInvalide();
		}
	} else if(ArgumentsLength > 1) {
		int32_t args[7] = { 1970, 0, 1, 0, 0, 0, 0};
		for(int i=0; i<min(ArgumentsLength,7); ++i) {
			CNumber v = c->getArgument(i)->toNumber().floor();
			if(v.isInt32() || v.isNegativeZero())
				args[i] = v.toInt32();
			else {
				returnVar->setInvalide();
				return;
			}
		}
		returnVar->setTime(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
	} else
		returnVar->setTime(CScriptTime::now());
}
static void scDate_UTC(const CFunctionsScopePtr &c, void *data) {
	int ArgumentsLength = c->getArgumentsLength();
	int32_t args[7] = { 1900, 0, 1, 0, 0, 0, 0};
	for(int i=0; i<min(ArgumentsLength,7); ++i) {
		CNumber v = c->getArgument(i)->toNumber().floor();
		if(v.isInt32() || v.isNegativeZero())
			args[i] = v.toInt32();
		else {
			c->setReturnVar(c->constScriptVar(NaN));
			return;
		}
	}
	c->setReturnVar(c->newScriptVar((double)CScriptTime::UTC(args[0], args[1], args[2], args[3], args[4], args[5], args[6])));

}
static void scDate_now(const CFunctionsScopePtr &c, void *data) {
	c->setReturnVar(c->newScriptVar((double)CScriptTime::now()));
}
static void scDate_parse(const CFunctionsScopePtr &c, void *data) {
	int64_t result;
	if(CScriptTime::Parse(c->getArgument(0)->toString(), &result))
		c->setReturnVar(c->newScriptVar((double)result));
	else
		c->setReturnVar(c->constScriptVar(NaN));
}
static inline void scDateThrowTypeError(const CFunctionsScopePtr &c, const string &Fnc) {
	c->throwError(TypeError, Fnc + " method called on incompatible Object");
}
#define DATE_PROTOTYPE_GET(FNC) \
	static void scDate_prototype_##FNC(const CFunctionsScopePtr &c, void *data) {	\
	CScriptVarDatePtr Date(c->getArgument("this"));											\
	if(!Date) scDateThrowTypeError(c, #FNC);													\
	if(Date->isValid())																				\
		c->setReturnVar(c->newScriptVar(Date->FNC()));										\
	else																									\
		c->setReturnVar(c->constScriptVar(NaN));												\
}

#define DATE_PROTOTYPE_SET(FNC) \
	static void scDate_prototype_##FNC(const CFunctionsScopePtr &c, void *data) {	\
	CScriptVarDatePtr Date(c->getArgument("this"));											\
	if(!Date) scDateThrowTypeError(c, #FNC);													\
	CNumber v = c->getArgument(0)->toNumber().floor();										\
	if(v.isInt32() || v.isNegativeZero()) {													\
		double value = (double)Date->FNC(v.toInt32());										\
		if(Date->isValid()) {																		\
			c->setReturnVar(c->newScriptVar(value));											\
			return;																						\
		}																									\
	}																										\
	Date->setInvalide();																				\
	c->setReturnVar(c->constScriptVar(NaN));													\
}

DATE_PROTOTYPE_SET(setDate)
DATE_PROTOTYPE_GET(getDate)
DATE_PROTOTYPE_SET(setUTCDate)
DATE_PROTOTYPE_GET(getUTCDate)
DATE_PROTOTYPE_GET(getDay)
DATE_PROTOTYPE_GET(getUTCDay)

static void scDate_prototype_setFullYear(const CFunctionsScopePtr &c, void *data) {
	CScriptVarDatePtr Date(c->getArgument("this"));
	if(!Date) scDateThrowTypeError(c, "setFullYear");
	int ArgumentsLength = c->getArgumentsLength();
	int32_t args[3] = { 0, Date->getMonth(), Date->getDate() };
	double value;
	for(int i=0; i<max(min(ArgumentsLength,3),1); ++i) {
		CNumber v = c->getArgument(i)->toNumber().floor();
		if(v.isInt32() || v.isNegativeZero())
			args[i] = v.toInt32();
		else
			goto invalid;
	}
	value = (double)Date->setFullYear(args[0], args[1], args[2]);
	if(Date->isValid()) {
		c->setReturnVar(c->newScriptVar(value));
		return;
	}
invalid:
	Date->setInvalide();
	c->setReturnVar(c->constScriptVar(NaN));
}

DATE_PROTOTYPE_GET(getFullYear)

static void scDate_prototype_setUTCFullYear(const CFunctionsScopePtr &c, void *data) {
	CScriptVarDatePtr Date(c->getArgument("this"));
	if(!Date) scDateThrowTypeError(c, "setUTCFullYear");
	int ArgumentsLength = c->getArgumentsLength();
	int32_t args[3] = { 0, Date->getUTCMonth(), Date->getUTCDate() };
	double value;
	for(int i=0; i<max(min(ArgumentsLength,3),1); ++i) {
		CNumber v = c->getArgument(i)->toNumber().floor();
		if(v.isInt32() || v.isNegativeZero())
			args[i] = v.toInt32();
		else
			goto invalid;
	}
	value = (double)Date->setUTCFullYear(args[0], args[1], args[2]);
	if(Date->isValid()) {
		c->setReturnVar(c->newScriptVar(value));
		return;
	}
invalid:
	Date->setInvalide();
	c->setReturnVar(c->constScriptVar(NaN));
}

DATE_PROTOTYPE_GET(getUTCFullYear)
DATE_PROTOTYPE_SET(setYear)
DATE_PROTOTYPE_GET(getYear)
DATE_PROTOTYPE_SET(setHours)
DATE_PROTOTYPE_GET(getHours)
DATE_PROTOTYPE_SET(setUTCHours)
DATE_PROTOTYPE_GET(getUTCHours)
DATE_PROTOTYPE_SET(setMilliseconds)
DATE_PROTOTYPE_GET(getMilliseconds)
DATE_PROTOTYPE_SET(setUTCMilliseconds)
DATE_PROTOTYPE_GET(getUTCMilliseconds)
DATE_PROTOTYPE_SET(setMinutes)
DATE_PROTOTYPE_GET(getMinutes)
DATE_PROTOTYPE_SET(setUTCMinutes)
DATE_PROTOTYPE_GET(getUTCMinutes)
DATE_PROTOTYPE_SET(setMonth)
DATE_PROTOTYPE_GET(getMonth)
DATE_PROTOTYPE_SET(setUTCMonth)
DATE_PROTOTYPE_GET(getUTCMonth)
DATE_PROTOTYPE_SET(setSeconds)
DATE_PROTOTYPE_GET(getSeconds)
DATE_PROTOTYPE_SET(setUTCSeconds)
DATE_PROTOTYPE_GET(getUTCSeconds)
DATE_PROTOTYPE_SET(setTime)
DATE_PROTOTYPE_GET(getTime)
DATE_PROTOTYPE_GET(getTimezoneOffset)

// ----------------------------------------------- Register Functions
extern "C" void _registerDateFunctions(CTinyJS *tinyJS) {
	CScriptVarPtr var = tinyJS->addNative("function Date(year, month, day, hour, minute, second, millisecond)", scDate, 0, SCRIPTVARLINK_CONSTANT); 
	CScriptVarPtr datePrototype = var->findChild(TINYJS_PROTOTYPE_CLASS);
	datePrototype->addChild("valueOf", tinyJS->objectPrototype_valueOf, SCRIPTVARLINK_BUILDINDEFAULT);
	datePrototype->addChild("toString", tinyJS->objectPrototype_toString, SCRIPTVARLINK_BUILDINDEFAULT);
	tinyJS->addNative("function Date.__constructor__()", scDate_Constructor, 0, SCRIPTVARLINK_CONSTANT);
	tinyJS->addNative("function Date.UTC()", scDate_UTC, 0, SCRIPTVARLINK_CONSTANT);
	tinyJS->addNative("function Date.now()", scDate_now, 0, SCRIPTVARLINK_CONSTANT);
	tinyJS->addNative("function Date.parse()", scDate_parse, 0, SCRIPTVARLINK_CONSTANT);
#define DATE_PROTOTYPE_NATIVE(FNC) tinyJS->addNative("function Date.prototype."#FNC"()", scDate_prototype_##FNC, 0, SCRIPTVARLINK_CONSTANT)
	DATE_PROTOTYPE_NATIVE(setDate);
	DATE_PROTOTYPE_NATIVE(getDate);
	DATE_PROTOTYPE_NATIVE(setUTCDate);
	DATE_PROTOTYPE_NATIVE(getUTCDate);
	DATE_PROTOTYPE_NATIVE(getDay);
	DATE_PROTOTYPE_NATIVE(getUTCDay);
	DATE_PROTOTYPE_NATIVE(setFullYear);
	DATE_PROTOTYPE_NATIVE(getFullYear);
	DATE_PROTOTYPE_NATIVE(setUTCFullYear);
	DATE_PROTOTYPE_NATIVE(getUTCFullYear);
	DATE_PROTOTYPE_NATIVE(setYear);
	DATE_PROTOTYPE_NATIVE(getYear);
	DATE_PROTOTYPE_NATIVE(setHours);
	DATE_PROTOTYPE_NATIVE(getHours);
	DATE_PROTOTYPE_NATIVE(setUTCHours);
	DATE_PROTOTYPE_NATIVE(getUTCHours);
	DATE_PROTOTYPE_NATIVE(setMilliseconds);
	DATE_PROTOTYPE_NATIVE(getMilliseconds);
	DATE_PROTOTYPE_NATIVE(setUTCMilliseconds);
	DATE_PROTOTYPE_NATIVE(getUTCMilliseconds);
	DATE_PROTOTYPE_NATIVE(setMinutes);
	DATE_PROTOTYPE_NATIVE(getMinutes);
	DATE_PROTOTYPE_NATIVE(setUTCMinutes);
	DATE_PROTOTYPE_NATIVE(getUTCMinutes);
	DATE_PROTOTYPE_NATIVE(setMonth);
	DATE_PROTOTYPE_NATIVE(getMonth);
	DATE_PROTOTYPE_NATIVE(setUTCMonth);
	DATE_PROTOTYPE_NATIVE(getUTCMonth);
	DATE_PROTOTYPE_NATIVE(setSeconds);
	DATE_PROTOTYPE_NATIVE(getSeconds);
	DATE_PROTOTYPE_NATIVE(setUTCSeconds);
	DATE_PROTOTYPE_NATIVE(getUTCSeconds);
	DATE_PROTOTYPE_NATIVE(setTime);
	DATE_PROTOTYPE_NATIVE(getTime);
	DATE_PROTOTYPE_NATIVE(getTimezoneOffset);

}

