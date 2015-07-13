#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "os-datetime.h"
#include "../objectscript.h"
#include "../os-binder.h"
#include <limits.h>

#if !defined HAVE_FTIME && !defined HAVE_FTIME_DISABLED
#define HAVE_FTIME
#endif

/* #if defined _MSC_VER && !defined HAVE_FTIME
#define HAVE_FTIME
#endif */

# if defined(HAVE_SYS_TIME_H) && defined(TIME_WITH_SYS_TIME)
#  include <sys/time.h>
#  include <time.h>
# else
#  include <time.h>
# endif
# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif
# ifdef HAVE_FTIME
#  include <sys/timeb.h>
# endif

/* We need floor() and ceil() for ticks conversions. */
#include <math.h>

namespace ObjectScript {

/* Table with day offsets for each month (0-based, without and with leap) */
static int month_offset[2][13] = {
	{ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 },
	{ 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 }
};

/* Table of number of days in a month (0-based, without and with leap) */
static int days_in_month[2][12] = {
	{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
	{ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
};

                                /*     jan  feb  mar  apr  may  jun  jul  aug  sep  oct  nov  dec */
static int d_table_common[13]  = {  0,   0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334 };
static int d_table_leap[13]    = {  0,   0,  31,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335 };
static int ml_table_common[13] = {  0,  31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31 };
static int ml_table_leap[13]   = {  0,  31,  29,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31 }; 

static int m_table_common[13] = { -1, 0, 3, 3, 6, 1, 4, 6, 2, 5, 0, 3, 5 }; /* 1 = jan */
static int m_table_leap[13] =   { -1, 6, 2, 3, 6, 1, 4, 6, 2, 5, 0, 3, 5 }; /* 1 = jan */ 

static const char *mon_full_names[] = {
	"January", "February", "March", "April",
	"May", "June", "July", "August",
	"September", "October", "November", "December"
};

static const char *mon_short_names[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

static const char *day_full_names[] = {
	"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
};

static const char *day_short_names[] = {
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};

static int mktime_works = 0;

/* Flag telling us whether the module was initialized or not. */
static bool is_initialized = false;

static bool is_posix_conform = false;		/* Does the system use POSIX
											time_t values ? */

static bool is_double_stack_problem = false;	/* Does the system
												have a problem
												passing doubles on
												the stack ? */
/* Seconds in a day (as double) */
#define SECONDS_PER_DAY ((double) 86400.0)

/* abstime value limit (as double). The limit itself does not belong
to the range of accepted values. Includes one leap second per
day. */
#define MAX_ABSTIME_VALUE ((double) 86401.0)

/* Flags for the calendar ID: */
#define CALENDAR_GREGORIAN	0
#define CALENDAR_JULIAN	1

#if defined __GNUC__ || defined IW_SDK

static int OS_VSNPRINTF(OS_CHAR * str, size_t size, const OS_CHAR *format, va_list va)
{
	return vsnprintf(str, size, format, va);
}

#else

static int OS_VSNPRINTF(OS_CHAR * str, size_t size, const OS_CHAR *format, va_list va)
{
	return vsnprintf_s(str, size, size/sizeof(OS_CHAR), format, va);
}

#endif

static int OS_SNPRINTF(OS_CHAR * str, size_t size, const OS_CHAR *format, ...)
{
	va_list va;
	va_start(va, format);
	int ret = OS_VSNPRINTF(str, size, format, va);
	va_end(va);
	return ret;
}

#define timelib_sll OS_INT64

static timelib_sll century_value(timelib_sll j)
{
	timelib_sll i = j - 17;
	timelib_sll c = (4 - i * 2 + (i + 1) / 4) % 7;

	return c < 0 ? c + 7 : c;
}

static bool timelib_is_leap(timelib_sll y);

static timelib_sll timelib_days_in_month(timelib_sll y, timelib_sll m)
{
	return timelib_is_leap(y) ? ml_table_leap[m] : ml_table_common[m];
} 

static timelib_sll timelib_day_of_week_ex(timelib_sll y, timelib_sll m, timelib_sll d, int iso)
{
	timelib_sll c1, y1, m1, dow;

	/* Only valid for Gregorian calendar, commented out as we don't handle
	 * julian calendar. We just return the 'wrong' day of week to be
	 * consistent.
	if (y < 1753) {
		return -1;
	} */
	c1 = century_value(y / 100);
	y1 = (y % 100);
	m1 = timelib_is_leap(y) ? m_table_leap[m] : m_table_common[m];
	dow = (c1 + y1 + m1 + (y1 / 4) + d) % 7;
	if (iso) {
		if (dow == 0) {
			dow = 7;
		}
	}
	return dow;
}

static timelib_sll timelib_day_of_week(timelib_sll y, timelib_sll m, timelib_sll d)
{
	return timelib_day_of_week_ex(y, m, d, 0);
} 

static timelib_sll timelib_iso_day_of_week(timelib_sll y, timelib_sll m, timelib_sll d)
{
	return timelib_day_of_week_ex(y, m, d, 1);
} 

static timelib_sll timelib_day_of_year(timelib_sll y, timelib_sll m, timelib_sll d)
{
	return (timelib_is_leap(y) ? d_table_leap[m] : d_table_common[m]) + d - 1;
} 

static void timelib_isoweek_from_date(timelib_sll y, timelib_sll m, timelib_sll d, timelib_sll *iw, timelib_sll *iy)
{
	int y_leap, prev_y_leap, doy, jan1weekday, weekday;

	y_leap = timelib_is_leap(y);
	prev_y_leap = timelib_is_leap(y-1);
	doy = (int)timelib_day_of_year(y, m, d) + 1;
	if (y_leap && m > 2) {
		doy++;
	}
	jan1weekday = (int)timelib_day_of_week(y, 1, 1);
	weekday = (int)timelib_day_of_week(y, m, d);
	if (weekday == 0) weekday = 7;
	if (jan1weekday == 0) jan1weekday = 7;
	/* Find if Y M D falls in YearNumber Y-1, WeekNumber 52 or 53 */
	if (doy <= (8 - jan1weekday) && jan1weekday > 4) {
		*iy = y - 1;
		if (jan1weekday == 5 || (jan1weekday == 6 && prev_y_leap)) {
			*iw = 53;
		} else {
			*iw = 52;
		}
	} else {
		*iy = y;
	}
	/* 8. Find if Y M D falls in YearNumber Y+1, WeekNumber 1 */
	if (*iy == y) {
		int i;

		i = y_leap ? 366 : 365;
		if ((i - (doy - y_leap)) < (4 - weekday)) {
			*iy = y + 1;
			*iw = 1;
			return;
		}
	}
	/* 9. Find if Y M D falls in YearNumber Y, WeekNumber 1 through 53 */
	if (*iy == y) {
		int j;

		j = doy + (7 - weekday) + (jan1weekday - 1);
		*iw = j / 7;
		if (jan1weekday > 4) {
			*iw -= 1;
		}
	}
}

class DateTimeOS: public OS
{
public:

	static void triggerError(OS * os, const OS::String& msg)
	{
		os->getGlobal(OS_TEXT("Exception"));
		os->pushGlobals();
		os->pushString(msg);
		os->callFT(1, 1);
		os->setException();
	}

	static void triggerError(OS * os, const char * msg)
	{
		os->getGlobal(OS_TEXT("Exception"));
		os->pushGlobals();
		os->pushString(msg);
		os->callFT(1, 1);
		os->setException();
	}

	static void initExtension(OS * os);

	/* Returns the current time in Unix ticks.

	The function tries to use the most accurate API available on the
	system.

	-1.0 is returned in case of an error.
	*/
	static double currentTime()
	{
	# if defined(HAVE_CLOCK_GETTIME)

		/* Use clock_gettime(), which has ns resolution */
		struct timespec ts;

		if(!clock_gettime(CLOCK_REALTIME, &ts))
			return ((double)ts.tv_sec + (double)ts.tv_nsec * 1e-9);
		else
			return -1.0;

	# elif defined(HAVE_GETTIMEOFDAY)

		/* Use gettimeofday(), which has us resolution */
		struct timeval tv;

	#  ifdef GETTIMEOFDAY_NO_TZ
		if(!gettimeofday(&tv))
	#  else
		if(!gettimeofday(&tv, 0))
	#  endif
			return ((double)tv.tv_sec + (double)tv.tv_usec * 1e-6);
		else
			return -1.0;

	# elif defined(HAVE_FTIME)

		/* Use ftime(), which provides ms resolution */
		struct timeb tb;

		ftime(&tb);
		return ((double)tb.time + (double)tb.millitm * 1e-3);

	# else

		/* Use time(), which usually only has seconds resolution */
		time_t ticks;

		time(&ticks);
		return (double)ticks;

	# endif
	}

	/* Try to determine the clock resolution. */
	static double clockResolution()
	{
	# if defined(HAVE_CLOCK_GETTIME)
	#  if defined(HAVE_CLOCK_GETRES)

		/* clock_gettime() is supposed to have ns resolution, but apparently
		this is not true on all systems. */
		struct timespec ts;

		if(!clock_getres(CLOCK_REALTIME, &ts))
			return ((double)ts.tv_sec + (double)ts.tv_nsec * 1e-9);
		else
			return -1.0;
	#  else
		/* We'll have to believe the man-page */
		return 1e-9;
	#  endif

	# elif defined(HAVE_GETTIMEOFDAY)

		/* gettimeofday() has us resolution according to the man-page */
		return 1e-6;

	# elif defined(HAVE_FTIME)

		/* ftime() provides ms resolution according to the man-page*/
		return 1e-3;

	# else

		/* time() usually only has seconds resolution */
		return 1.0;

	# endif
	}

	/* Fix a second value for display as string.

	Seconds are rounded to the nearest microsecond in order to avoid
	cases where e.g. 3.42 gets displayed as 03.41 or 3.425 is diplayed
	as 03.42.

	Special care is taken for second values which would cause rounding
	to 60.00 -- these values are truncated to 59.99 to avoid the value
	of 60.00 due to rounding to show up even when the indictated time
	does not point to a leap second. The same is applied for rounding
	towards 61.00 (leap seconds).

	The second value returned by this function should be formatted
	using '%05.2f' (which rounds to 2 decimal places).
	*/
	static double fixSecondDisplay(double second)
	{
		/* Special case for rounding towards 60. */
		if(second >= 59.995 && second < 60.0)
			return 59.99;

		/* Special case for rounding towards 61. */
		if(second >= 60.995 && second < 61.0)
			return 60.99;

		/* Round to the nearest microsecond */
		second = (second * 1e6 + 0.5) / 1e6;

		return second;
	}

	/* These functions work for positive *and* negative years for
	compilers which round towards zero and ones that always round down
	to the nearest integer. */

	/* Return 1/0 iff year points to a leap year in calendar. */
	static bool isLeapyear(long year, int calendar)
	{
		if(calendar == CALENDAR_GREGORIAN)
			return (year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0));
		else
			return (year % 4 == 0);
	}

	/* Return the day of the week for the given absolute date. */
	static int dayOfWeek(long absdate)
	{
		int day_of_week;
		if(absdate >= 1)
			day_of_week = (absdate - 1) % 7;
		else
			day_of_week = 6 - ((-absdate) % 7);
		return day_of_week;
	}

	/* Return the year offset, that is the absolute date of the day
	31.12.(year-1) in the given calendar.

	For the Julian calendar we shift the absdate (which is measured
	using the Gregorian Epoch) value by two days because the Epoch
	(0001-01-01) in the Julian calendar lies 2 days before the Epoch in
	the Gregorian calendar.

	Years around the Epoch (mathematical approach, not necessarily
	historically correct):

	Year  0005 (5 AD) - yearoffset: 1461,not a leap year
	Year  0004 (4 AD) - yearoffset: 1095,leap year
	Year  0003 (3 AD) - yearoffset: 730, not a leap year
	Year  0002 (2 AD) - yearoffset: 365, not a leap year
	Year  0001 (1 AD) - yearoffset: 0, not a leap year
	Year  0000 (1 BC) - yearoffset: -366, leap year
	Year -0001 (2 BC) - yearoffset: -731, not a leap year
	Year -0002 (3 BC) - yearoffset: -1096, not a leap year
	Year -0003 (4 BC) - yearoffset: -1461, not a leap year
	Year -0004 (5 BC) - yearoffset: -1827, leap year
	Year -0005 (6 BC) - yearoffset: -2192, not a leap year
	*/
	static long yearOffset(long year, int calendar)
	{
		if(year >= 1){
			/* For years >= 1, we can simply count the number of days
			between the Epoch and the given year */
			year--;
			if(calendar == CALENDAR_GREGORIAN)
				return year*365 + year/4 - year/100 + year/400;
			else
				return year*365 + year/4 - 2;
		} 
		/* For years <= 0, we need to reverse the sign of the year (to
		avoid integer rounding issues with negative numbers on some
		platforms) and compensate for the year 0 being a leap
		year */
		year = -year;
		if(calendar == CALENDAR_GREGORIAN)
			return -(year*365 + year/4 - year/100 + year/400) - 366;
		else
			return -(year*365 + year/4) - 366 - 2;
	}

	/* Normalize the data and calculate the absolute date, year offset and
	whether the year is a leap year or not.

	Returns -1 in case of an error, 0 otherwise.
	*/
	static int normalizedDate(OS * os, long year,
		int month,
		int day,
		int calendar,
		long *absdate_output,
		long *yearoffset_output,
		int *leap_output,
		long *normalized_year,
		int *normalized_month,
		int *normalized_day)
	{
		/* Range check */
		if(year <= -(LONG_MAX / 366) || year >= (LONG_MAX / 366)){
			triggerError(os, OS::String::format(os, "year out of range: %ld", year));
			return -1;
		}

		/* Is it a leap year ? */
		bool leap = isLeapyear(year, calendar);

		/* Negative month values indicate months relative to the years end */
		if(month < 0)
			month += 13;
		if(month < 1 || month > 12){
			triggerError(os, OS::String::format(os, "month out of range (1-12): %i", month));
			return -1;
		}

		/* Negative values indicate days relative to the months end */
		if(day < 0)
			day += days_in_month[leap][month - 1] + 1;
		if(day < 1 || day > days_in_month[leap][month - 1]){
			triggerError(os, OS::String::format(os, "day out of range: %i", day));
			return -1;
		}

		long yearoffset = yearOffset(year, calendar);
		long absdate = day + month_offset[leap][month - 1] + yearoffset;

		/*
		DPRINTF("normalizedDate: "
			"year=%ld month=%i day=%i yearoffset=%ld leap=%i absdate=%ld\n",
			year, month, day, yearoffset, leap, absdate);
		*/

		if(absdate_output)
			*absdate_output = absdate;
		if(yearoffset_output)
			*yearoffset_output = yearoffset;
		if(leap_output)
			*leap_output = leap;
		if(normalized_year)
			*normalized_year = year;
		if(normalized_month)
			*normalized_month = month;
		if(normalized_day)
			*normalized_day = day;
		return 0;
	}

	/* This function checks whether the system uses the POSIX time_t rules
	(which do not support leap seconds) or a time package with leap
	second support enabled. Return 1 if it uses POSIX time_t values, 0
	otherwise.

	POSIX: 1986-12-31 23:59:59 UTC == 536457599

	With leap seconds:		  == 536457612

	(since there were 13 leapseconds in the years 1972-1985 according
	to the tz package available from ftp://elsie.nci.nih.gov/pub/)

	*/

	static bool checkPOSIX(void)
	{
		time_t ticks = 536457599;
		struct tm *tm;

		memset(&tm, 0, sizeof(tm));
		tm = gmtime(&ticks);
		if(tm == NULL)
			return 0;
		if(tm->tm_hour == 23 &&
			tm->tm_min == 59 &&
			tm->tm_sec == 59 &&
			tm->tm_mday == 31 &&
			tm->tm_mon == 11 &&
			tm->tm_year == 86)
			return true;
		else
			return false;
	}

	static bool checkDoubleStackProblem(double value)
	{
		return value == SECONDS_PER_DAY;
	}

	/* This global is set to
	-1 if mktime() auto-corrects the value of the DST flag to whatever the
	value should be for the given point in time (which is bad)
	0 if the global has not yet been initialized
	1 if mktime() does not correct the value and returns proper values
	*/
	static int initMktimeWorks(OS * os)
	{
		struct tm tm;
		time_t a,b;

		/* Does mktime() in general and specifically DST = -1 work ? */
		memset(&tm, 0, sizeof(tm));
		tm.tm_mday = 1;
		tm.tm_mon = 5;
		tm.tm_year = 98;
		tm.tm_isdst = -1;
		a = mktime(&tm);
		if(a == (time_t)-1){
			triggerError(os, "mktime() returned an error (June)");
			return -1;
		}
		memset(&tm, 0, sizeof(tm));
		tm.tm_mday = 1;
		tm.tm_mon = 0;
		tm.tm_year = 98;
		tm.tm_isdst = -1;
		a = mktime(&tm);
		if(a == (time_t)-1){
			triggerError(os, "mktime() returned an error (January)");
			return -1;
		}

		/* Some mktime() implementations return (time_t)-1 when setting
		DST to anything other than -1. Others adjust DST without
		looking at the given setting. */

		/* a = (Summer, DST = 0) */
		memset(&tm, 0, sizeof(tm));
		tm.tm_mday = 1;
		tm.tm_mon = 5;
		tm.tm_year = 98;
		tm.tm_isdst = 0;
		a = mktime(&tm);
		if(a == (time_t)-1){
			mktime_works = -1;
			return 0;
		}

		/* b = (Summer, DST = 1) */
		memset(&tm, 0, sizeof(tm));
		tm.tm_mday = 1;
		tm.tm_mon = 5;
		tm.tm_year = 98;
		tm.tm_isdst = 1;
		b = mktime(&tm);
		if(a == (time_t)-1 || a == b){
			mktime_works = -1;
			return 0;
		}

		/* a = (Winter, DST = 0) */
		memset(&tm, 0, sizeof(tm));
		tm.tm_mday = 1;
		tm.tm_mon = 0;
		tm.tm_year = 98;
		tm.tm_isdst = 0;
		a = mktime(&tm);
		if(a == (time_t)-1){
			mktime_works = -1;
			return 0;
		}

		/* b = (Winter, DST = 1) */
		memset(&tm, 0, sizeof(tm));
		tm.tm_mday = 1;
		tm.tm_mon = 0;
		tm.tm_year = 98;
		tm.tm_isdst = 1;
		b = mktime(&tm);
		if(a == (time_t)-1 || a == b){
			mktime_works = -1;
			return 0;
		}

		mktime_works = 1;
		return 0;
	}

	struct DateTime
	{
		OS * os;

		/* Representation used to do calculations */
		long absdate;	/* number of days since 31.12. in the year 1 BC calculated in the Gregorian calendar. */
		double abstime; /* seconds since 0:00:00.00 (midnight) on the day pointed to by absdate */

		/* COM Date representation */
		double comdate;
    
		/* Broken down values (set at creation time and using the calendar
		   specified in the calendar flag); depend on the calendar used. */
		long year;				/* starting from year 1 */
		OS_INT8 month;			/* 1-12 */
		OS_INT8 day;			/* 1-31 */
		OS_INT8 hour;			/* 0-24 */
		OS_INT8 minute;			/* 0-59 */
		double second;			/* 0-60.999... */

		OS_INT8 day_of_week;	/* 0 (Monday) - 6 (Sunday) */
		OS_INT16 day_of_year;	/* 1-366 */

		OS_BYTE calendar;		/* Calendar ID; for possible values see above. */

		struct Now{};

		DateTime(OS * p_os, DateTime * dt)
		{
			os = p_os;
			absdate = dt->absdate;
			abstime = dt->abstime;
			comdate = dt->comdate;
			year = dt->year;
			month = dt->month;
			day = dt->day;
			hour = dt->hour;
			minute = dt->minute;
			second = dt->second;
			day_of_week = dt->day_of_week;
			day_of_year = dt->day_of_year;
			calendar = dt->calendar;
		}

		DateTime(OS * p_os);
		DateTime(OS * p_os, const Now&);
		DateTime(OS * p_os, long year,
			int month,
			int day,
			int hour,
			int minute,
			double second,
			int calendar);

		DateTime * clone();

		#ifndef HAVE_TIMEGM
		/* Calculates the conversion of the datetime instance to Unix ticks.

		For instances pointing to localtime, localticks will hold the
		corresponding Unix ticks value. In case the instance points to GMT
		time, gmticks will hold the correct ticks value.

		In both cases, gmtoffset will hold the GMT offset (local-GMT).

		Returns -1 (and sets an exception) to indicate errors; 0
		otherwise. 

		Note:

		There's some integer rounding error in the mktime() function that
		triggers near MAXINT on Solaris. The error was reported by Joe Van
		Andel <vanandel@ucar.edu> among others:

		Ooops: 2038-01-18 22:52:31.00 t = 2147467951 diff = -4294857600.0

		On 64-bit Alphas running DEC OSF, Tony Ibbs <tony@lsl.co.uk>
		reports:

		Ooops: 1901-12-13 21:57:57.00 t = 2147487973 diff = -4294967296.0
		...(the diffs stay the same)...
		Ooops: 1969-12-31 10:10:54.00 t = 4294917550 diff = -4294967296.0

		Note the years ! Some rollover is happening near 2^31-1 even
		though Alphas happen to use 64-bits. This could be a bug in this
		function or in DEC's mktime() implementation.
		*/
		int calcTicks(double *localticks, double *gmticks, double *gmtoffset)
		{
			struct tm tm;
			struct tm *gmt;
			time_t ticks;
			double offset;

			if(calendar != CALENDAR_GREGORIAN){
				triggerError(os, "can only convert the Gregorian calendar to ticks");
				return -1;
			}
			if((long)((int)year) != year){
				triggerError(os, "year out of range for ticks conversion");
				return -1;
			}

			/* Calculate floor()ed ticks value  */
			memset(&tm, 0, sizeof(tm));
			tm.tm_hour = (int)hour;
			tm.tm_min = (int)minute;
			tm.tm_sec = (int)second;
			tm.tm_mday = (int)day;
			tm.tm_mon = (int)month - 1;
			tm.tm_year = (int)year - 1900;
			tm.tm_wday = -1;
			tm.tm_yday = (int)day_of_year - 1;
			tm.tm_isdst = -1; /* unknown */
			ticks = mktime(&tm);
			if(ticks == (time_t)-1 && tm.tm_wday == -1){
				/* XXX Hack to allow conversion during DST switching. */
				tm.tm_hour = 0;
				tm.tm_min = 0;
				tm.tm_sec = 0;
				ticks = mktime(&tm);
				if(ticks == (time_t)-1 && tm.tm_wday == -1){
					triggerError(os, "cannot convert value to a Unix ticks value");
					return -1;
				}
				ticks += ((int)hour * 3600
					+ (int)minute * 60
					+ (int)second);
			}

			/* Add fraction for localticks */
			*localticks = ((double)ticks
				+ (abstime - floor(abstime)));

			/* Now compare local time and GMT time */
			gmt = gmtime(&ticks);
			if(gmt == NULL){
				triggerError(os, "cannot convert value to a Unix ticks value");
				return -1;
			}

			/* Check whether we have the same day and prepare offset */
			if(gmt->tm_mday != tm.tm_mday){
				double localdate = (tm.tm_year * 10000 + 
					tm.tm_mon *  100 +
					tm.tm_mday);
				double gmdate = (gmt->tm_year * 10000 +
					gmt->tm_mon * 100 + 
					gmt->tm_mday);
				if(localdate < gmdate)
					offset = -SECONDS_PER_DAY;
				else
					offset = SECONDS_PER_DAY;
			}
			else
				offset = 0.0;

			/* Calculate difference in seconds */
			offset += ((hour - gmt->tm_hour) * 3600.0
				+ (minute - gmt->tm_min) * 60.0
				+ (floor(second) - (double)gmt->tm_sec));
			*gmtoffset = offset;
			*gmticks = *localticks + offset;
			return 0;
		}
		#endif

		/* Sets the date part of the DateTime object using the indicated
		calendar. 

		XXX This could also be done using some integer arithmetics rather
		than with this iterative approach... */
		int _setAbsDate(long absdate, int calendar = CALENDAR_GREGORIAN)
		{
			long year;
			long yearoffset;
			int dayoffset;
			int *monthoffset;

			// DPRINTF("setAbsDate(datetime=%x,absdate=%ld,calendar=%i)\n", datetime,absdate,calendar);

			/* Approximate year */
			if(calendar == CALENDAR_GREGORIAN)
				year = (long)(((double)absdate) / 365.2425);
			else
				year = (long)(((double)absdate) / 365.25);
			
			if(absdate > 0)
				year++;

			bool leap = false;
			/* Apply corrections to reach the correct year */
			while (1){
				/* Calculate the year offset */
				yearoffset = yearOffset(year, calendar);
				// DPRINTF(" trying year = %ld yearoffset = %ld\n", year, yearoffset);

				/* Backward correction: absdate must be greater than the yearoffset */
				if(yearoffset >= absdate){
					year--;
					// DPRINTF(" backward correction\n");
					continue;
				}

				dayoffset = absdate - yearoffset;
				leap = DateTimeOS::isLeapyear(year, calendar);

				/* Forward correction: years only have 365/366 days */
				if(dayoffset > 365){
					if(leap && dayoffset > 366){
						year++;
						// DPRINTF(" forward correction (leap year)\n");
						continue;
					}
					else if(!leap){
						year++;
						// DPRINTF(" forward correction (non-leap year)\n");
						continue;
					}
				}

				/* Done */
				// DPRINTF(" using year = %ld leap = %i dayoffset = %i\n", year, leap, dayoffset);
				break;
			}

			DateTime * datetime = this;
			datetime->year = year;
			datetime->calendar = calendar;

			/* Now iterate to find the month */
			monthoffset = month_offset[leap];
			{
				int month;
				for (month = 1; month < 13; month++)
					if(monthoffset[month] >= dayoffset)
						break;
				datetime->month = month;
				datetime->day = dayoffset - month_offset[leap][month-1];
			}

			datetime->day_of_week = dayOfWeek(absdate);
			datetime->day_of_year = dayoffset;
			return 0;
		}

		int setAbsDate(long absdate)
		{
			return setAbsDateTime(absdate, abstime, calendar);
		}

		/* Sets the time part of the DateTime object. */
		int _setAbsTime(double abstime)
		{
			int inttime;
			int hour,minute;
			double second;

			// DPRINTF("SetFromAbsTime(datetime=%x,abstime=%.20f)\n", (long)datetime,abstime);

			inttime = (int)abstime;
			if(inttime == 86400){
				/* Special case for leap seconds */
				hour = 23;
				minute = 59;
				second = 60.0 + abstime - (double)inttime;
			} else {
				hour = inttime / 3600;
				minute = (inttime % 3600) / 60;
				second = abstime - (double)(hour*3600 + minute*60);
			}

			DateTime * datetime = this;
			datetime->abstime = abstime;
			datetime->hour = hour;
			datetime->minute = minute;
			datetime->second = second;

			return 0;
		}

		int setAbsTime(double abstime)
		{
			return setAbsDateTime(absdate, abstime, calendar);
		}

		double getAbsTime()
		{
			return abstime;
		}

		/* Set the instance's value using the given date and time. calendar
		may be set to the flags: CALENDAR_GREGORIAN,
		JULIAN_CALENDAR to indicate the calendar to be used. */
		int setDateAndTime(
			long year,
			int month,
			int day,
			int hour,
			int minute,
			double second,
			int calendar)
		{
			DateTime * datetime = this;
			double comdate;

			if(year == 0 && month == 0 && day == 0
				&& hour == 0 && minute == 0 && second == 0)
			{
				datetime->year = 0;
				datetime->month = 0;
				datetime->day = 0;
				datetime->hour = 0;
				datetime->minute = 0;
				datetime->second = 0;
				datetime->absdate = 0;
				datetime->abstime = 0;
				datetime->comdate = 0;
				datetime->day_of_week = 0;
				datetime->day_of_year = 0;
				datetime->calendar = calendar;
				return 0;
			}

			/*
			DPRINTF("SetFromDateAndTime("
				"datetime=%x year=%ld month=%i day=%i "
				"hour=%i minute=%i second=%f calendar=%i)\n",
				datetime,year,month,day,hour,minute,second,calendar);
			*/

			/* Calculate the absolute date */
			{
				long yearoffset, absdate;
				if(normalizedDate(os, year, month, day, 
					calendar,
					&absdate, &yearoffset, NULL,
					&year, &month, &day) < 0)
				{
					return -1;
				}
				/*
				DPRINTF("SetFromDateAndTime: "
					"yearoffset=%ld absdate=%ld "
					"year=%ld month=%i day=%i (normalized)\n",
					yearoffset,absdate,
					year,month,day);
				*/
				datetime->absdate = absdate;

				datetime->year = year;
				datetime->month = month;
				datetime->day = day;

				datetime->day_of_week = dayOfWeek(absdate);
				datetime->day_of_year = (short)(absdate - yearoffset);

				datetime->calendar = calendar;

				comdate = (double)absdate - 693594.0;
			}

			/* Calculate the absolute time */
			{
				if(hour < 0 || hour > 23){
					triggerError(os, OS::String::format(os, "hour out of range (0-23): %i", hour));
					return -1;
				}
				if(minute < 0 || minute > 59){
					triggerError(os, OS::String::format(os, "minute out of range (0-59): %i", minute));
					return -1;
				}
				if(!(second >= (double)0.0 && 
					(second < (double)60.0 || 
					(hour == 23 && minute == 59 && 
					second < (double)61.0))))
				{
					triggerError(os, OS::String::format(os, "second out of range (0.0 - <60.0; <61.0 for 23:59): %i", (int)second));
					return -1;
				}

				datetime->abstime = (double)(hour*3600 + minute*60) + second;

				datetime->hour = hour;
				datetime->minute = minute;
				datetime->second = second;

				if(comdate < 0.0)
					comdate -= datetime->abstime / SECONDS_PER_DAY;
				else
					comdate += datetime->abstime / SECONDS_PER_DAY;
				datetime->comdate = comdate;
			}
			return 0;
		}

		/* Set the instance's value using the given absolute date and
		time. The calendar used is the Gregorian. */
		int setAbsDateTime(long absdate, double abstime, int calendar)
		{
			DateTime * datetime = this;

			/* Bounds check */
			if(!(abstime >= 0.0 && abstime < MAX_ABSTIME_VALUE)){
				triggerError(os, OS::String::format(os, "abstime out of range (0.0 - 86401.0): %i", (int)abstime));
				return -1;
			}

			datetime->absdate = absdate;
			datetime->abstime = abstime;

			/* Calculate COM date */
			{
				double comdate = (double)(datetime->absdate - 693594);
				if(comdate < 0)
					comdate -= datetime->abstime / SECONDS_PER_DAY;
				else
					comdate += datetime->abstime / SECONDS_PER_DAY;
				datetime->comdate = comdate;
			}

			/* Calculate the date */
			if(_setAbsDate(datetime->absdate, calendar) < 0)
				return -1;

			/* Calculate the time */
			if(_setAbsTime(datetime->abstime) < 0)
				return -1;

			return 0;
		}

		long getAbsDate()
		{
			return absdate;
		}

		/* Set the instance's value using the given Windows COM date.  The
		calendar used is the Gregorian. */
		int setCOMDate(double comdate)
		{
			long absdate;
			double abstime;

			DateTime * datetime = this;
			datetime->comdate = comdate;

			/* XXX should provide other means to calculate the broken down
			values for these huge values. */
			if(-(double)LONG_MAX > comdate || comdate > (double)LONG_MAX){
				triggerError(os, OS::String::format(os, "DateTime COM date out of range: %i", (int)comdate));
				return -1;
			}

			absdate = (long)comdate;
			abstime = (comdate - (double)absdate) * SECONDS_PER_DAY;
			if(abstime < 0)
				abstime = -abstime;
			absdate += 693594;

			// DPRINTF("SetFromCOMDate: absdate=%ld abstime=%f\n", absdate, abstime);

			datetime->absdate = absdate;
			datetime->abstime = abstime;

			/* Calculate the date */
			if(_setAbsDate(absdate, CALENDAR_GREGORIAN) < 0)
				return -1;

			/* Calculate the time */
			if(_setAbsTime(abstime) < 0)
				return -1;

			return 0;
		}

		double getCOMDate()
		{
			return comdate;
		}

		void setCOMTime(double value)
		{
			setCOMDate(value / SECONDS_PER_DAY);
		}

		double getCOMTime()
		{
			return comdate * SECONDS_PER_DAY;
		}

		/* Creates a new DateTime instance using datetime as basis by adding
		the given offsets to the value of datetime and then re-normalizing
		them.

		The resulting DateTime instance will use the same calendar as
		datetime. */
		int addAbsDateTimeOffset(DateTime * datetime, long absdate_offset, double abstime_offset)
		{
			long days;
			long absdate = datetime->absdate + absdate_offset;
			double abstime = datetime->abstime + abstime_offset;

			/* Normalize */
			if(abstime < 0 && abstime >= -SECONDS_PER_DAY){
				abstime += SECONDS_PER_DAY;
				absdate -= 1;
			}
			if(abstime >= SECONDS_PER_DAY && abstime < 2*SECONDS_PER_DAY){
				abstime -= SECONDS_PER_DAY;
				absdate += 1;
			}
			/* Some compilers and/or processors (e.g. gcc 2.95.3 on Mandrake)
			have troubles with getting rounding right even though 86400.0
			IS exactly representable using IEEE floats... that's why we are
			extra careful here. */
			while(abstime < 0){
				days = (long)(-abstime / SECONDS_PER_DAY);
				if(days == 0)
					days = 1;
				days++;
				abstime += days * SECONDS_PER_DAY;
				absdate -= days;
			}
			while(abstime >= SECONDS_PER_DAY){
				days = (long)(abstime / SECONDS_PER_DAY);
				if(days == 0)
					days = 1;
				abstime -= days * SECONDS_PER_DAY;
				absdate += days;
			}
			if(is_double_stack_problem && abstime >= (double)8.63999999999999854481e+04){
				/* DPRINTF("FromDateTimeAndOffset: "
					"triggered double work-around: "
					"abstime is %.20f, diff %.20e, as int %i\n", 
					abstime,
					abstime - SECONDS_PER_DAY,
					(int)abstime); */
				absdate += 1;
				abstime = 0.0;
			}
			return setAbsDateTime(absdate, abstime, datetime->calendar);
		}

		int setAbsDays(double absdays)
		{
			double fabsdays = floor(absdays);
			if(!(fabsdays > -LONG_MAX && fabsdays < LONG_MAX)){
				triggerError(os, OS::String::format(os, "absdays out of range: %i", (int)absdays));
				return -1;
			}
			long absdate = (long)fabsdays + 1;
			double abstime = (absdays - fabsdays) * SECONDS_PER_DAY;
			return setAbsDateTime(absdate, abstime, CALENDAR_GREGORIAN);
		}

		int setTmStruct(struct tm *tm)
		{
			return setDateAndTime(
				tm->tm_year + 1900,
				tm->tm_mon + 1,
				tm->tm_mday,
				tm->tm_hour,
				tm->tm_min,
				(double)tm->tm_sec,
				CALENDAR_GREGORIAN);
		}

		int setTicks(double ticks)
		{
			DateTime * datetime = this;
			time_t tticks = (time_t)ticks;

			/* Conversion is done to local time */
			struct tm * tm = localtime(&tticks);
			if(tm == NULL){
				triggerError(os, "could not convert ticks value to local time");
				return -1;
			}
			/* Add fraction */
			double seconds = floor((double)tm->tm_sec) + (ticks - floor(ticks));
			return setDateAndTime(
				tm->tm_year + 1900,
				tm->tm_mon + 1,
				tm->tm_mday,
				tm->tm_hour,
				tm->tm_min,
				seconds,
				CALENDAR_GREGORIAN);
		}

		int setGMTicks(double ticks)
		{
			DateTime * datetime = this;
			time_t tticks = (time_t)ticks;

			/* Conversion is done to GMT time */
			struct tm * tm = gmtime(&tticks);
			/* Add fraction */
			double seconds = floor((double)tm->tm_sec) + (ticks - floor(ticks));
			return setDateAndTime(
				tm->tm_year + 1900,
				tm->tm_mon + 1,
				tm->tm_mday,
				tm->tm_hour,
				tm->tm_min,
				seconds,
				CALENDAR_GREGORIAN);
		}

		int setNow()
		{
			return setTicks(currentTime());
		}

		struct tm * toTmStruct(struct tm * tm)
		{
			DateTime * datetime = this;
			if((long)((int)datetime->year) != datetime->year){
				triggerError(os, "year out of range for tm struct conversion");
				return NULL;
			}

			memset(tm, 0, sizeof(struct tm));
			tm->tm_hour = (int)datetime->hour;
			tm->tm_min = (int)datetime->minute;
		#if ROUND_SECONDS_IN_TM_STRUCT
			tm->tm_sec = (int)(datetime->second + 0.5); /* Round the value */
		#else
			tm->tm_sec = (int)datetime->second;
		#endif
			tm->tm_mday = (int)datetime->day;
			tm->tm_mon = (int)datetime->month - 1;
			tm->tm_year = (int)datetime->year - 1900;
			tm->tm_wday = ((int)datetime->day_of_week + 1) % 7;
			tm->tm_yday = (int)datetime->day_of_year - 1;
			tm->tm_isdst = -1; /* unknown */
			return tm;
		}

		/* Returns the ticks value for datetime assuming it stores a datetime
		value in local time. 

		offsets is subtracted from the resulting ticks value (this can be
		used to implement DST handling). 

		dst is passed to the used mktime() C lib API and can influence the
		calculation: dst == 1 means that the datetime value should be
		interpreted with DST on, dst == 0 with DST off. Note that this
		doesn't work on all platforms. dst == -1 means: use the DST value
		in affect at the given point in time.
		*/
		double getTicksWithOffset(double offset, int dst)
		{
			struct tm tm;
			time_t tticks;
			double ticks;

			DateTime * datetime = this;
			if(datetime->calendar != CALENDAR_GREGORIAN){
				triggerError(os, "can only convert the Gregorian calendar to ticks");
				return -1;
			}
			if((long)((int)datetime->year) != datetime->year){
				triggerError(os, "year out of range for ticks conversion");
				return -1;
			}

			memset(&tm, 0, sizeof(tm));
			tm.tm_hour = (int)datetime->hour;
			tm.tm_min = (int)datetime->minute;
			tm.tm_sec = (int)datetime->second;
			tm.tm_mday = (int)datetime->day;
			tm.tm_mon = (int)datetime->month - 1;
			tm.tm_year = (int)datetime->year - 1900;
			tm.tm_wday = -1;
			tm.tm_yday = (int)datetime->day_of_year - 1;
			tm.tm_isdst = dst;
			/* mktime uses local time ! */
			tticks = mktime(&tm);
			if(tticks == (time_t)-1 && tm.tm_wday == -1){
				triggerError(os, "cannot convert value to a time value");
				return -1;
			}
			/* Check if mktime messes up DST */
			if(dst >= 0 && mktime_works <= 0){
				if(mktime_works == 0){
					if(initMktimeWorks(os) < 0)
						return -1;
				}
				if(mktime_works < 0){
					triggerError(os, "mktime() doesn't support setting DST to anything but -1");
					return -1;
				}
			}
			/* Add fraction and turn into a double and subtract offset */
			ticks = (double)tticks
				+ (datetime->abstime - floor(datetime->abstime))
				- offset;
			return ticks;
		}

		double getTicks()
		{
			return getTicksWithOffset(0, -1);
		}

		/* Returns the ticks value for datetime assuming it stores a UTC
		datetime value. 

		offsets is subtracted from the resulting ticks value before
		returning it. This is useful to implement time zone handling.
		*/
		double getGMTicksWithOffset(double offset)
		{
			DateTime * datetime = this;
			if(datetime->calendar != CALENDAR_GREGORIAN){
				triggerError(os, "can only convert the Gregorian calendar to ticks");
				return -1;
			}

			/* For POSIX style calculations there's nothing much to do... */
			if(is_posix_conform){
				return ((datetime->absdate - 719163) * SECONDS_PER_DAY 
					+ datetime->abstime
					- offset);
			}

		#ifdef HAVE_TIMEGM
			{
				/* Use timegm() API */
				struct tm tm;
				time_t tticks;

				Py_Assert((long)((int)datetime->year) == datetime->year,
					RangeError,
					"year out of range for ticks conversion");

				/* Use timegm() if not POSIX conform: the time package knows about
				leap seconds so we use that information too. */
				memset(&tm, 0, sizeof(tm));
				tm.tm_hour = (int)datetime->hour;
				tm.tm_min = (int)datetime->minute;
				tm.tm_sec = (int)datetime->second;
				tm.tm_mday = (int)datetime->day;
				tm.tm_mon = (int)datetime->month - 1;
				tm.tm_year = (int)datetime->year - 1900;
				tm.tm_wday = ((int)datetime->day_of_week + 1) % 7;
				tm.tm_yday = (int)datetime->day_of_year - 1;
				tm.tm_isdst = 0;
				/* timegm uses UTC ! */
				tticks = timegm(&tm);
				Py_Assert(tticks != (time_t)-1,
					Error,
					"cannot convert value to a time value");
				/* Add fraction and turn into a double */
				return ((double)tticks
					+ (datetime->abstime - floor(datetime->abstime))
					- offset);
			}
		#else
			{
				/* Work around with a trick... */
				double localticks, gmticks, gmtoffset;
				if(calcTicks(&localticks,&gmticks,&gmtoffset) < 0)
					return -1;
				return gmticks - offset;
			}
		#endif
		}

		double getGMTicks()
		{
			return getGMTicksWithOffset(0);
		}

		/* Returns the UTC offset at the given time; assumes local time is
		stored in the instance. */
		double getGMTOffset()
		{
			DateTime * datetime = this;
			double gmticks = getGMTicks();
			if(gmticks == -1.0 && os->isExceptionSet())
				return -1;
			double ticks = getTicksWithOffset(0, -1);
			if(ticks == -1.0 && os->isExceptionSet())
				return -1;
			return gmticks - ticks;
		}

		void setGMTOffsetRO()
		{
			os->setException("GMTOffset is readonly property");
		}

		/* Return the instance's value in absolute days: days since 0001-01-01
		0:00:00 using fractions for parts of a day. */
		double getAbsDays()
		{
			DateTime * datetime = this;
			return ((double)(datetime->absdate - 1) + 
				datetime->abstime / SECONDS_PER_DAY);
		}

		/* Returns the DST setting for the given DateTime instance assuming it
		refers to local time. -1 is returned in case it cannot be
		determined, 0 if it is not active, 1 if it is. For calendars other
		than the Gregorian the function always returns -1. 

		XXX If mktime() returns -1 for isdst, try harder using the hack in
		timegm.py.
		*/
		int getDST()
		{
			struct tm tm;
			time_t ticks;

			DateTime * datetime = this;
			if(datetime->calendar != CALENDAR_GREGORIAN)
				return -1;
			if((long)((int)datetime->year) != datetime->year)
				return -1;

			memset(&tm, 0, sizeof(tm));
			tm.tm_hour = (int)datetime->hour;
			tm.tm_min = (int)datetime->minute;
			tm.tm_sec = (int)datetime->second;
			tm.tm_mday = (int)datetime->day;
			tm.tm_mon = (int)datetime->month - 1;
			tm.tm_year = (int)datetime->year - 1900;
			tm.tm_wday = -1;
			tm.tm_isdst = -1;
			ticks = mktime(&tm);
			if(ticks == (time_t)-1 && tm.tm_wday == -1)
				return -1;
			return tm.tm_isdst;
		}

		const OS_CHAR * getEnglishSuffix()
		{
			if(day >= 10 && day <= 19){
				return "th";
			}else{
				switch(day % 10){
				case 1: return "st";
				case 2: return "nd";
				case 3: return "rd";
				}
			}
			return "th";
		}

		OS::String format(const OS::String& fmt_str)
		{
			struct Lib {
				DateTime * dt;
				int offset;

				int getGMTOffset()
				{
					if(dt){
						offset = (int)dt->getGMTOffset();
						dt = NULL;
					}
					return offset;
				}

			} lib = {this};

			Core::Buffer buf(os);
			OS_CHAR buffer[128];
			
			timelib_sll isoweek, isoyear;
			int length, rfc_colon, offset, week_year_set = 0;

			const OS_CHAR * fmt = fmt_str.toChar();
			int fmt_len = fmt_str.getLen();
			for(int i = 0; i < fmt_len; i++){
				rfc_colon = 0;
				switch (fmt[i]) {
					/* day */
				case 'd': length = OS_SNPRINTF(buffer, 32, "%02d", (int)day); break;
				case 'D': length = OS_SNPRINTF(buffer, 32, "%s", day_short_names[day_of_week]); break;
				case 'j': length = OS_SNPRINTF(buffer, 32, "%d", (int)day); break;
				case 'l': length = OS_SNPRINTF(buffer, 32, "%s", day_full_names[day_of_week]); break;
				case 'S': length = OS_SNPRINTF(buffer, 32, "%s", getEnglishSuffix()); break;
				case 'w': length = OS_SNPRINTF(buffer, 32, "%d", (int)day_of_week); break;
				case 'N': length = OS_SNPRINTF(buffer, 32, "%d", (int)timelib_iso_day_of_week(year, month, day)); break;
				case 'z': length = OS_SNPRINTF(buffer, 32, "%d", (int)day_of_year); break;

					/* week */
				case 'W':
					if(!week_year_set) { timelib_isoweek_from_date(year, month, day, &isoweek, &isoyear); week_year_set = 1; }
					length = OS_SNPRINTF(buffer, 32, "%02d", (int)isoweek); break; /* iso weeknr */
				case 'o':
					if(!week_year_set) { timelib_isoweek_from_date(year, month, day, &isoweek, &isoyear); week_year_set = 1; }
					length = OS_SNPRINTF(buffer, 32, "%d", (int)isoyear); break; /* iso year */

					/* month */
				case 'F': length = OS_SNPRINTF(buffer, 32, "%s", mon_full_names[month - 1]); break;
				case 'm': length = OS_SNPRINTF(buffer, 32, "%02d", (int)month); break;
				case 'M': length = OS_SNPRINTF(buffer, 32, "%s", mon_short_names[month - 1]); break;
				case 'n': length = OS_SNPRINTF(buffer, 32, "%d", (int)month); break;
				case 't': length = OS_SNPRINTF(buffer, 32, "%d", (int)timelib_days_in_month(year, month)); break;

					/* year */
				case 'L': length = OS_SNPRINTF(buffer, 32, "%d", (int)DateTimeOS::isLeapyear(year, calendar)); break;
				case 'y': length = OS_SNPRINTF(buffer, 32, "%02d", (int)year % 100); break;
				case 'Y': length = OS_SNPRINTF(buffer, 32, "%s%04d", year < 0 ? "-" : "", (int)(year < 0 ? -year : year)); break;

					/* time */
				case 'a': length = OS_SNPRINTF(buffer, 32, "%s", hour >= 12 ? "pm" : "am"); break;
				case 'A': length = OS_SNPRINTF(buffer, 32, "%s", hour >= 12 ? "PM" : "AM"); break;
				case 'B': {
					long sse = (long)getTicks();
					int retval = (((((long)sse)-(((long)sse) - ((((long)sse) % 86400) + 3600))) * 10) / 864);			
					while (retval < 0) {
						retval += 1000;
					}
					retval = retval % 1000;
					length = OS_SNPRINTF(buffer, 32, "%03d", retval);
					break;
				}
				case 'g': length = OS_SNPRINTF(buffer, 32, "%d", (hour % 12) ? (int) hour % 12 : 12); break;
				case 'G': length = OS_SNPRINTF(buffer, 32, "%d", (int) hour); break;
				case 'h': length = OS_SNPRINTF(buffer, 32, "%02d", (hour % 12) ? (int) hour % 12 : 12); break;
				case 'H': length = OS_SNPRINTF(buffer, 32, "%02d", (int) hour); break;
				case 'i': length = OS_SNPRINTF(buffer, 32, "%02d", (int) minute); break;
				case 's': length = OS_SNPRINTF(buffer, 32, "%02d", (int) second); break;
				case 'u': length = OS_SNPRINTF(buffer, 32, "%06d", (int) floor(second * 1000000 + 0.5)); break;

					/* timezone */
				case 'I': 
#if 1
					length = 0;
#else
					length = OS_SNPRINTF(buffer, 32, "%d", localtime ? offset->is_dst : 0);
#endif
					break;

				case 'P': rfc_colon = 1; /* break intentionally missing */
				case 'O': 
					offset = lib.getGMTOffset();
					length = OS_SNPRINTF(buffer, 32, "%c%02d%s%02d",
						offset < 0 ? '-' : '+', abs(offset / 3600),
						rfc_colon ? ":" : "",
						abs((offset % 3600) / 60)
						);
					break;
				case 'T': 
					offset = lib.getGMTOffset();
					length = OS_SNPRINTF(buffer, 32, "GMT%c%02d%02d",
						offset < 0 ? '-' : '+', abs(offset / 3600), abs((offset % 3600) / 60));
					break;

				case 'e': 
#if 1
					length = 0;
#else
					if (!localtime) {
						length = OS_SNPRINTF(buffer, 32, "%s", "UTC");
					} else {
						switch (t->zone_type) {
						case TIMELIB_ZONETYPE_ID:
							length = OS_SNPRINTF(buffer, 32, "%s", t->tz_info->name);
							break;
						case TIMELIB_ZONETYPE_ABBR:
							length = OS_SNPRINTF(buffer, 32, "%s", offset->abbr);
							break;
						case TIMELIB_ZONETYPE_OFFSET:
							length = OS_SNPRINTF(buffer, 32, "%c%02d:%02d",
								((offset->offset < 0) ? '-' : '+'),
								abs(offset->offset / 3600),
								abs((offset->offset % 3600) / 60)
								);
							break;
						}
					}
#endif
					break;

				case 'Z': 
					offset = lib.getGMTOffset();
					length = OS_SNPRINTF(buffer, 32, "%d", offset);
					break;

					/* full date/time */
				case 'c': 
					offset = lib.getGMTOffset();
					length = OS_SNPRINTF(buffer, 96, "%04d-%02d-%02dT%02d:%02d:%02d%c%02d:%02d",
						(int)year, (int)month, (int)day,
						(int)hour, (int)minute, (int)second,
						offset < 0 ? '-' : '+', abs(offset / 3600), abs((offset % 3600) / 60)
						);
					break;
				case 'r': 
					offset = lib.getGMTOffset();
					length = OS_SNPRINTF(buffer, 96, "%3s, %02d %3s %04d %02d:%02d:%02d %c%02d%02d",
						day_short_names[day_of_week],
						(int)day, mon_short_names[month - 1],
						(int)year, (int)hour, (int)minute, (int)second,
						offset < 0 ? '-' : '+', abs(offset / 3600), abs((offset % 3600) / 60)
						);
					break;
				case 'R':
					{
						offset = lib.getGMTOffset();
						DateTime n(os);
						n.setCOMDate(this->comdate - (double)offset / 3600.0 / 24.0);

						length = OS_SNPRINTF(buffer, 96, "%3s, %02d %3s %04d %02d:%02d:%02d GMT",
							day_short_names[n.day_of_week],
							(int)n.day, mon_short_names[n.month - 1],
							(int)n.year, (int)n.hour, (int)n.minute, (int)n.second);
						break;
					}
				case 'U': length = OS_SNPRINTF(buffer, 32, "%lld", (timelib_sll)getTicks()); break;

				case '\\': if (i < fmt_len) i++; /* break intentionally missing */

				default: buffer[0] = fmt[i]; buffer[1] = '\0'; length = 1; break;			
				}
				buf.append(buffer, length);
			}
			return buf.toStringOS();
		}

		/* Returns a Python string containing the locale's timezone name for
		the given DateTime instance (assuming it refers to local time).
		"???"  is returned in case it cannot be determined.  */
		OS::String getTimezoneString()
		{
			DateTime * datetime = this;
			if(datetime->calendar != CALENDAR_GREGORIAN)
				return OS::String(os, "???");
			if((long)((int)datetime->year) != datetime->year)
				return OS::String(os, "???");

		#ifndef HAVE_STRFTIME
			return OS::String(os, "???");
		#else
			struct tm tm;
			time_t ticks;
			char tz[255];

			memset(&tm, 0, sizeof(tm));
			tm.tm_hour = (int)datetime->hour;
			tm.tm_min = (int)datetime->minute;
			tm.tm_sec = (int)datetime->second;
			tm.tm_mday = (int)datetime->day;
			tm.tm_mon = (int)datetime->month - 1;
			tm.tm_year = (int)datetime->year - 1900;
			tm.tm_wday = -1;
			tm.tm_isdst = getDST();
			ticks = mktime(&tm);
			if(ticks == (time_t)-1 && tm.tm_wday == -1)
				return OS::String(os, "???");
			strftime(tz,sizeof(tm),"%Z",&tm);
			return OS::String(os, tz);
		#endif
		}

		/* Returns the ISO week notation for the given DateTime instance as
		tuple (year,isoweek,isoday). The algorithm also Works for negative
		dates.

		XXX Check this algorithm for the Julian calendar.
		*/
		static int getISOWeekTuple(OS * os, int params, int, int, void * user_param);

		/* Return a string identifying the used calendar. */
		OS::String getCalendarString()
		{
			DateTime * datetime = this;
			switch (datetime->calendar){
			case CALENDAR_GREGORIAN:
				return OS::String(os, "GREGORIAN");
			}
			return OS::String(os, "JULIAN");
		}

		bool isLeapyear()
		{
			return DateTimeOS::isLeapyear(year, calendar);
		}

		void setIsLeapyearRO()
		{
			os->setException("isLeapyear is readonly property");
		}

		long getYear()
		{
			return year;
		}
		void setYear(long year)
		{
			setDateAndTime(year, month, day, hour, minute, second, calendar);
		}

		int getMonth()
		{
			return month;
		}
		void setMonth(int month)
		{
			setDateAndTime(year, month, day, hour, minute, second, calendar);
		}

		int getDay()
		{
			return day;
		}
		void setDay(int day)
		{
			setDateAndTime(year, month, day, hour, minute, second, calendar);
		}

		int getHour()
		{
			return hour;
		}
		void setHour(int hour)
		{
			setDateAndTime(year, month, day, hour, minute, second, calendar);
		}

		int getMinute()
		{
			return minute;
		}
		void setMinute(int minute)
		{
			setDateAndTime(year, month, day, hour, minute, second, calendar);
		}

		double getSecond()
		{
			return second;
		}
		void setSecond(double second)
		{
			setDateAndTime(year, month, day, hour, minute, second, calendar);
		}

		int getDayOfWeek()
		{
			return day_of_week;
		}

		void setDayOfWeekRO()
		{
			os->setException("dayOfWeek is readonly property");
		}

		int getDayOfYear()
		{
			return day_of_year;
		}

		void setDayOfYearRO()
		{
			os->setException("dayOfYear is readonly property");
		}

		int getCalendar()
		{
			return calendar;
		}

		void setCalendarRO()
		{
			os->setException("calendar is readonly property");
		}

		OS::String toJson()
		{
			OS::Core::Buffer buf(os);
			os->appendQuotedString(buf, toString());
			return buf.toStringOS();
		}

		/* Writes a string representation to buffer. If the string does not
		fit the buffer, nothing is written. */
		OS::String toString()
		{
			char buffer[64];
			DateTime * self = this;
			double second = fixSecondDisplay(self->second);
			if(self->year >= 0)
				sprintf(buffer,"%04li-%02i-%02i %02i:%02i:%06.3f",
					(long)self->year,(int)self->month,(int)self->day,
					(int)self->hour,(int)self->minute,
					(float)second);
			else
				sprintf(buffer,"-%04li-%02i-%02i %02i:%02i:%06.3f",
					(long)-self->year,(int)self->month,(int)self->day,
					(int)self->hour,(int)self->minute,
					(float)second);
			return OS::String(os, buffer);
		}

		/* Returns a string indicating the date in ISO format. */
		OS::String toDateString()
		{
			char buffer[50];
			DateTime * self = this;
			if(self->year >= 0)
				sprintf(buffer,"%04li-%02i-%02i",
				(long)self->year,(int)self->month,(int)self->day);
			else
				sprintf(buffer,"-%04li-%02i-%02i",
				(long)-self->year,(int)self->month,(int)self->day);
			return OS::String(os, buffer);
		}

		/* Returns a string indicating the time in ISO format. */
		OS::String toTimeString()
		{
			char buffer[50];
			DateTime * self = this;
			double second = fixSecondDisplay(self->second);
			sprintf(buffer,"%02i:%02i:%06.3f",
				(int)self->hour,(int)self->minute,(float)second);
			return OS::String(os, buffer);
		}

		#ifdef HAVE_STRFTIME
		Py_C_Function( strftime,
			"strftime(formatstr)")
		{
			PyObject *v;
			char *fmt = 0;
			char *output = 0;
			Py_ssize_t len_output,size_output = STRFTIME_OUTPUT_SIZE;
			struct tm tm;

			Py_GetArg("|s",fmt);

			if(!fmt)
				/* We default to the locale's standard date/time format */
				fmt = "%c";

			Py_Assert((long)((int)datetime->year) == datetime->year,
				RangeError,
				"year out of range for strftime() formatting");

			/* Init tm struct */
			memset(&tm, 0, sizeof(tm));
			tm.tm_mday = (int)datetime->day;
			tm.tm_mon = (int)datetime->month - 1;
			tm.tm_year = (int)datetime->year - 1900;
			tm.tm_hour = (int)datetime->hour;
			tm.tm_min = (int)datetime->minute;
		#if ROUND_SECONDS_IN_TM_STRUCT
			tm.tm_sec = (int)(datetime->second + 0.5); /* Round the value */
		#else
			tm.tm_sec = (int)datetime->second;
		#endif
			tm.tm_wday = ((int)datetime->day_of_week + 1) % 7;
			tm.tm_yday = (int)datetime->day_of_year - 1;
			tm.tm_isdst = DST(datetime);

			output = new(char,size_output);

			while (1){
				if(output == NULL){
					PyErr_NoMemory();
					goto onError;
				}
				len_output = strftime(output,size_output,fmt,&tm);
				if(len_output == size_output){
					size_output *= 2;
					output = resize(output,char,size_output);
				}
				else
					break;
			}
			v = PyString_FromStringAndSize(output,len_output);
			if(v == NULL)
				goto onError;
			free(output);
			return v;

		onError:
			if(output)
				free(output);
			return NULL;
		}
		#endif

		int compare(DateTime * other)
		{
			DateTime * self = this;
			if (self == other)
				return 0;

			/* Short-cut */
			long d0 = self->absdate, d1 = other->absdate;
			double t0 = self->abstime, t1 = other->abstime;

			return (d0 < d1) ? -1 : (d0 > d1) ? 1 :
				(t0 < t1) ? -1 : (t0 > t1) ? 1 : 0;
		}

		static int __newinstance(OS * os, int params, int, int, void * user_param);
		static int now(OS * os, int params, int, int, void * user_param);
	};

};

template <> struct CtypeName<DateTimeOS::DateTime>{ static const OS_CHAR * getName(){ return OS_TEXT("DateTime"); } };
template <> struct CtypeValue<DateTimeOS::DateTime*>: public CtypeUserClass<DateTimeOS::DateTime*>{};
template <> struct UserDataDestructor<DateTimeOS::DateTime>
{
	static void dtor(ObjectScript::OS * os, void * data, void * user_param)
	{
		OS_ASSERT(data && dynamic_cast<DateTimeOS::DateTime*>((DateTimeOS::DateTime*)data));
		DateTimeOS::DateTime * buf = (DateTimeOS::DateTime*)data;
		buf->~DateTime();
		os->free(buf);
	}
};

DateTimeOS::DateTime::DateTime(OS * p_os)
{
	os = p_os;
#if 1
	year = 0;
	month = 0;
	day = 0;
	hour = 0;
	minute = 0;
	second = 0;
	absdate = 0;
	abstime = 0;
	comdate = 0;
	day_of_week = 0;
	day_of_year = 0;
	calendar = CALENDAR_GREGORIAN;
#else
	setAbsDateTime(0, 0, CALENDAR_GREGORIAN);
#endif
}

DateTimeOS::DateTime::DateTime(OS * p_os, const Now&)
{
	os = p_os;
	setNow();
}

DateTimeOS::DateTime::DateTime(OS * p_os, long year,
			int month,
			int day,
			int hour,
			int minute,
			double second,
			int calendar)
{
	os = p_os;
	setDateAndTime(year, month, day, hour, minute, second, calendar);
}

int DateTimeOS::DateTime::getISOWeekTuple(OS * os, int params, int, int, void * user_param)
{
	OS_GET_SELF(DateTime*);
	DateTime * datetime = self;
	int week;
	long year = datetime->year;
	int day;

	/* Estimate */
	week = (datetime->day_of_year-1) - datetime->day_of_week + 3;
	if(week >= 0)
		week = week / 7 + 1;
	day = datetime->day_of_week + 1;
	// DPRINTF("ISOWeekTuple: estimated year, week, day = %ld, %i, %i\n",year,week,day);

	/* Verify */
	if(week < 0){
		/* The day lies in last week of the previous year */
		year--;
		if((week > -2) || 
			(week == -2 && DateTimeOS::isLeapyear(year, datetime->calendar)))
			week = 53;
		else	    
			week = 52;
	}
	else if(week == 53){
		/* Check if the week belongs to year or year+1 */
		if(31-datetime->day + datetime->day_of_week < 3){
			week = 1;
			year++;
		}
	}
	// DPRINTF("ISOWeekTuple: corrected year, week, day = %ld, %i, %i\n",year,week,day);
	os->pushNumber(year);
	os->pushNumber(week);
	os->pushNumber(day);
	return 3;
}

DateTimeOS::DateTime * DateTimeOS::DateTime::clone()
{
	return new (os->malloc(sizeof(DateTime) OS_DBG_FILEPOS)) DateTime(os, this);
}

int DateTimeOS::DateTime::now(OS * os, int params, int, int, void * user_param)
{
	DateTime * dt = new (os->malloc(sizeof(DateTime) OS_DBG_FILEPOS)) DateTime(os, DateTime::Now());
	pushCtypeValue(os, dt);
	return 1;
}

int DateTimeOS::DateTime::__newinstance(OS * os, int params, int, int, void * user_param)
{
	if(params < 1){
		DateTime * dt = new (os->malloc(sizeof(DateTime) OS_DBG_FILEPOS)) DateTime(os);
		pushCtypeValue(os, dt);
		return 1;
	}
	if(os->isObject(-params+0)){
		int offs = os->getAbsoluteOffs(-params+0);
		
		DateTime * dt = new (os->malloc(sizeof(DateTime) OS_DBG_FILEPOS)) DateTime(os);
		pushCtypeValue(os, dt);
		int res_offs = os->getAbsoluteOffs(-1);

		os->pushStackValue(offs);
		while(os->nextIteratorStep()){
			int key_offs = os->getAbsoluteOffs(-2);
			os->pushStackValue(res_offs);
			os->pushStackValue(key_offs + 0);
			os->pushStackValue(key_offs + 1);
			os->setProperty();
			os->pop(2);
		}
		os->pop();
		return 1;
	}
	/* os->getGlobal("ODBODateTime");
	bool isODBODateTime = os->is(-params+0-1, -1);
	os->pop();
	if(isODBODateTime){
		os->getProperty(-params+0, "year");		long year = (long)os->popDouble();
		os->getProperty(-params+0, "month");	int month = os->popInt()-1;
		os->getProperty(-params+0, "day");		int day = os->popInt();
		os->getProperty(-params+0, "hour");		int hour = os->popInt();
		os->getProperty(-params+0, "minute");	int minute = os->popInt();
		os->getProperty(-params+0, "second");	double second = os->popDouble();
		int calendar = CALENDAR_GREGORIAN;
		DateTime * dt = new (os->malloc(sizeof(DateTime) OS_DBG_FILEPOS)) DateTime(os, year, month, day, hour, minute, second, calendar);
		pushCtypeValue(os, dt);
		return 1;
	} */

	long year = (long)os->toDouble(-params+0);
	int month = params >= 2 ? os->toInt(-params+1) : 1;
	int day = params >= 3 ? os->toInt(-params+2) : 1;
	int hour = params >= 4 ? os->toInt(-params+3) : 0;
	int minute = params >= 5 ? os->toInt(-params+4) : 0;
	double second = params >= 6 ? os->toDouble(-params+5) : 0;
	int calendar = params >= 7 ? os->toInt(-params+6) : CALENDAR_GREGORIAN;
	
	DateTime * dt = new (os->malloc(sizeof(DateTime) OS_DBG_FILEPOS)) DateTime(os, year, month, day, hour, minute, second, calendar);
	pushCtypeValue(os, dt);
	return 1;
}

void DateTimeOS::initExtension(OS * os)
{
	if(!is_initialized){
		is_posix_conform = checkPOSIX();
		is_double_stack_problem = checkDoubleStackProblem(SECONDS_PER_DAY - (double)7.27e-12);

		is_initialized = true;
		// if(initMktimeWorks(os) < 0){
		//	return;
		// }
	}
	{
		OS::FuncDef funcs[] = {
			{OS_TEXT("__newinstance"), DateTime::__newinstance},
			{OS_TEXT("now"), DateTime::now},
			def(OS_TEXT("clone"), &DateTime::clone),
			def(OS_TEXT("valueOf"), &DateTime::toString),
			def(OS_TEXT("toJson"), &DateTime::toJson),
			def(OS_TEXT("format"), &DateTime::format),

			def(OS_TEXT("__get@absdays"), &DateTime::getAbsDays),
			def(OS_TEXT("__set@absdays"), &DateTime::setAbsDays),

			def(OS_TEXT("__get@GMTOffset"), &DateTime::getGMTOffset),
			def(OS_TEXT("__set@GMTOffset"), &DateTime::setGMTOffsetRO),

			def(OS_TEXT("__get@GMTicks"), &DateTime::getGMTicks),
			def(OS_TEXT("__set@GMTicks"), &DateTime::setGMTicks),
			def(OS_TEXT("getGMTicksWithOffset"), &DateTime::getGMTicksWithOffset),

			def(OS_TEXT("__get@ticks"), &DateTime::getTicks),
			def(OS_TEXT("__set@ticks"), &DateTime::setTicks),
			def(OS_TEXT("getTicksWithOffset"), &DateTime::getTicksWithOffset), // DST is incorrect

			def(OS_TEXT("addAbsDateTimeOffset"), &DateTime::addAbsDateTimeOffset),

			def(OS_TEXT("__get@comdate"), &DateTime::getCOMDate),
			def(OS_TEXT("__set@comdate"), &DateTime::setCOMDate),

			def(OS_TEXT("__get@comtime"), &DateTime::getCOMTime),
			def(OS_TEXT("__set@comtime"), &DateTime::setCOMTime),

			def(OS_TEXT("setAbsDateTime"), &DateTime::setAbsDateTime),
			def(OS_TEXT("setDateAndTime"), &DateTime::setDateAndTime),

			def(OS_TEXT("__get@abstime"), &DateTime::getAbsTime),
			def(OS_TEXT("__set@abstime"), &DateTime::setAbsTime),

			def(OS_TEXT("__get@absdate"), &DateTime::getAbsDate),
			def(OS_TEXT("__set@absdate"), &DateTime::setAbsDate),
			
			def(OS_TEXT("__get@isLeapyear"), &DateTime::isLeapyear),
			def(OS_TEXT("__set@isLeapyear"), &DateTime::setIsLeapyearRO),

			def(OS_TEXT("__get@year"), &DateTime::getYear),
			def(OS_TEXT("__set@year"), &DateTime::setYear),
			
			def(OS_TEXT("__get@month"), &DateTime::getMonth),
			def(OS_TEXT("__set@month"), &DateTime::setMonth),
			
			def(OS_TEXT("__get@day"), &DateTime::getDay),
			def(OS_TEXT("__set@day"), &DateTime::setDay),
			
			def(OS_TEXT("__get@hour"), &DateTime::getHour),
			def(OS_TEXT("__set@hour"), &DateTime::setHour),
			
			def(OS_TEXT("__get@minute"), &DateTime::getMinute),
			def(OS_TEXT("__set@minute"), &DateTime::setMinute),
			
			def(OS_TEXT("__get@second"), &DateTime::getSecond),
			def(OS_TEXT("__set@second"), &DateTime::setSecond),
			
			def(OS_TEXT("__get@dayOfWeek"), &DateTime::getDayOfWeek),
			def(OS_TEXT("__set@dayOfWeek"), &DateTime::setDayOfWeekRO),
			
			def(OS_TEXT("__get@dayOfYear"), &DateTime::getDayOfYear),
			def(OS_TEXT("__set@dayOfYear"), &DateTime::setDayOfYearRO),
			
			def(OS_TEXT("__get@calendar"), &DateTime::getCalendar),
			def(OS_TEXT("__set@calendar"), &DateTime::setCalendarRO),
			{}
		};
		registerUserClass<DateTime>(os, funcs);
	}
}

void initDateTimeExtension(OS* os)
{
	DateTimeOS::initExtension(os);
}

double getTimeSec()
{
	return DateTimeOS::currentTime();
}

bool timelib_is_leap(timelib_sll year)
{
	return DateTimeOS::isLeapyear((long)year, CALENDAR_GREGORIAN);
}

} // namespace ObjectScript

