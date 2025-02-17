/*******************************************************************************
* Copyright (c) 2009 Luaj.org. All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
* THE SOFTWARE.
******************************************************************************/
package org.luaj.vm2.lib;

import org.luaj.vm2.*;

import java.io.IOException;
import java.time.format.TextStyle;
import java.util.*;

/**
 * Subclass of {@link LibFunction} which implements the standard lua {@code os}
 * library.
 * <p>
 * It is a usable base with simplified stub functions for library functions that
 * cannot be implemented uniformly on Jse and Jme.
 * <p>
 * This can be installed as-is on either platform, or extended and refined to be
 * used in a complete Jse implementation.
 * <p>
 * Because the nature of the {@code os} library is to encapsulate os-specific
 * features, the behavior of these functions varies considerably from their
 * counterparts in the C platform.
 * <p>
 * The following functions have limited implementations of features that are not
 * supported well on Jme:
 * <ul>
 * <li>{@code execute()}</li>
 * <li>{@code remove()}</li>
 * <li>{@code rename()}</li>
 * <li>{@code tmpname()}</li>
 * </ul>
 * <p>
 * Typically, this library is included as part of a call to either
 * {@link org.luaj.vm2.lib.jse.JsePlatform#standardGlobals()} or
 * {@link org.luaj.vm2.lib.jme.JmePlatform#standardGlobals()}
 *
 * <pre>
 * {
 * 	&#64;code
 * 	Globals globals = JsePlatform.standardGlobals();
 * 	System.out.println(globals.get("os").get("time").call());
 * }
 * </pre>
 *
 * In this example the platform-specific {@link org.luaj.vm2.lib.jse.JseOsLib}
 * library will be loaded, which will include the base functionality provided by
 * this class.
 * <p>
 * To instantiate and use it directly, link it into your globals table via
 * {@link LuaValue#load(LuaValue)} using code such as:
 *
 * <pre>
 * {
 * 	&#64;code
 * 	Globals globals = new Globals();
 * 	globals.load(new JseBaseLib());
 * 	globals.load(new PackageLib());
 * 	globals.load(new OsLib());
 * 	System.out.println(globals.get("os").get("time").call());
 * }
 * </pre>
 * <p>
 *
 * @see LibFunction
 * @see org.luaj.vm2.lib.jse.JseOsLib
 * @see org.luaj.vm2.lib.jse.JsePlatform
 * @see org.luaj.vm2.lib.jme.JmePlatform
 * @see <a href=
 *      "http://www.lua.org/manual/5.1/manual.html#5.8">http://www.lua.org/manual/5.1/manual.html#5.8</a>
 */
public class OsLib extends TwoArgFunction {
	public static final String TMP_PREFIX = ".luaj";
	public static final String TMP_SUFFIX = "tmp";

	private static final int CLOCK     = 0;
	private static final int DATE      = 1;
	private static final int DIFFTIME  = 2;
	private static final int EXECUTE   = 3;
	private static final int EXIT      = 4;
	private static final int GETENV    = 5;
	private static final int REMOVE    = 6;
	private static final int RENAME    = 7;
	private static final int SETLOCALE = 8;
	private static final int TIME      = 9;
	private static final int TMPNAME   = 10;

	private static final String[] NAMES = { "clock", "date", "difftime", "execute", "exit", "getenv", "remove",
			"rename", "setlocale", "time", "tmpname", };

	private static final long t0       = System.currentTimeMillis();
	private static long       tmpnames = t0;

	protected Globals globals;

	/**
	 * Create and OsLib instance.
	 */
	public OsLib() {
	}

	/**
	 * Perform one-time initialization on the library by creating a table
	 * containing the library functions, adding that table to the supplied
	 * environment, adding the table to package.loaded, and returning table as
	 * the return value.
	 *
	 * @param modname the module name supplied if this is loaded via 'require'.
	 * @param env     the environment to load into, typically a Globals
	 *                instance.
	 */
	@Override
	public LuaValue call(LuaValue modname, LuaValue env) {
		globals = env.checkglobals();
		LuaTable os = new LuaTable();
		for (int i = 0; i < NAMES.length; ++i)
			os.set(NAMES[i], new OsLibFunc(i, NAMES[i]));
		env.set("os", os);
		if (!env.get("package").isnil())
			env.get("package").get("loaded").set("os", os);
		return os;
	}

	class OsLibFunc extends VarArgFunction {
		public OsLibFunc(int opcode, String name) {
			this.opcode = opcode;
			this.name = name;
		}

		@Override
		public Varargs invoke(Varargs args) {
			try {
				switch (opcode) {
				case CLOCK:
					return valueOf(clock());
				case DATE: {
					String s = args.optjstring(1, "%c");
					long t = args.isnumber(2)? args.tolong(2): time(null);
					if (s.equals("*t")) {
						Calendar d = Calendar.getInstance();
						d.setTime(new Date(t*1000));
						LuaTable tbl = LuaValue.tableOf();
						tbl.set("year", LuaValue.valueOf(d.get(Calendar.YEAR)));
						tbl.set("month", LuaValue.valueOf(d.get(Calendar.MONTH)+1));
						tbl.set("day", LuaValue.valueOf(d.get(Calendar.DAY_OF_MONTH)));
						tbl.set("hour", LuaValue.valueOf(d.get(Calendar.HOUR_OF_DAY)));
						tbl.set("min", LuaValue.valueOf(d.get(Calendar.MINUTE)));
						tbl.set("sec", LuaValue.valueOf(d.get(Calendar.SECOND)));
						tbl.set("wday", LuaValue.valueOf(d.get(Calendar.DAY_OF_WEEK)));
						tbl.set("yday", LuaValue.valueOf(d.get(Calendar.DAY_OF_YEAR)));
						tbl.set("isdst", LuaValue.valueOf(isDaylightSavingsTime(d)));
						return tbl;
					}
					return valueOf(date(s, t == -1? time(null): t));
				}
				case DIFFTIME:
					return valueOf(difftime(args.checkdouble(1), args.checkdouble(2)));
				case EXECUTE:
					return execute(args.optjstring(1, null));
				case EXIT:
					exit(args.optint(1, 0));
					return NONE;
				case GETENV: {
					final String val = getenv(args.checkjstring(1));
					return val != null? valueOf(val): NIL;
				}
				case REMOVE:
					remove(args.checkjstring(1));
					return LuaValue.TRUE;
				case RENAME:
					rename(args.checkjstring(1), args.checkjstring(2));
					return LuaValue.TRUE;
				case SETLOCALE: {
					String s = setlocale(args.optjstring(1, null), args.optjstring(2, "all"));
					return s != null? valueOf(s): NIL;
				}
				case TIME:
					return valueOf(time(args.opttable(1, null)));
				case TMPNAME:
					return valueOf(tmpname());
				}
				return NONE;
			} catch (IOException e) {
				return varargsOf(NIL, valueOf(e.getMessage()));
			}
		}
	}

	/**
	 * @return an approximation of the amount in seconds of CPU time used by the
	 *         program. For luaj this simple returns the elapsed time since the
	 *         OsLib class was loaded.
	 */
	protected double clock() {
		return (System.currentTimeMillis()-t0)/1000.;
	}

	/**
	 * Returns the number of seconds from time t1 to time t2. In POSIX, Windows,
	 * and some other systems, this value is exactly t2-t1.
	 *
	 * @param t2
	 * @param t1
	 * @return diffeence in time values, in seconds
	 */
	protected double difftime(double t2, double t1) {
		return t2-t1;
	}

	/**
	 * If the time argument is present, this is the time to be formatted (see
	 * the os.time function for a description of this value). Otherwise, date
	 * formats the current time.
	 *
	 * Date returns the date as a string, formatted according to the same rules
	 * as ANSII strftime, but without support for %g, %G, or %V.
	 *
	 * When called without arguments, date returns a reasonable date and time
	 * representation that depends on the host system and on the current locale
	 * (that is, os.date() is equivalent to os.date("%c")).
	 *
	 * @param format
	 * @param timeInSec time since epoch, or -1 if not supplied
	 * @return a LString or a LTable containing date and time, formatted
	 *         according to the given string format.
	 */
	private static String date(String format, long timeInSec) {
		Calendar d = Calendar.getInstance();
		d.setTime(new Date(timeInSec*1000));
		if (format.startsWith("!")) {
			timeInSec -= timeZoneOffset(d);
			d.setTime(new Date(timeInSec*1000));
			format = format.substring(1);
		}
		byte[] fmt = format.getBytes();
		final int n = fmt.length;

		Buffer result = new Buffer(n);
		for (int i = 0; i < n; i++) {
			byte c = fmt[i];
			switch (c) {
			case '\n':
				result.append("\n");
				break;
			case '%':
				if (++i >= n)
					break;
				String conv = Character.toString((char) fmt[i]);
				if (CONVERTERS.containsKey(conv)) {
					result.append(CONVERTERS.get(conv).convert(d));
				} else {
					LuaValue.argerror(1, "invalid conversion specifier '%" + conv + "'");
				}
				break;
			default:
				result.append(c);
				break;
			}
		}
		return result.tojstring();
	}

	private static final String[] WeekdayNameAbbrev = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
	private static final String[] WeekdayName       = { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
			"Friday", "Saturday" };
	private static final String[] MonthNameAbbrev   = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
			"Oct", "Nov", "Dec" };
	private static final String[] MonthName         = { "January", "February", "March", "April", "May", "June", "July",
			"August", "September", "October", "November", "December" };

	private static interface DateConversion {
		public String convert(Calendar d);
	}

	private static final Map<String, DateConversion> CONVERTERS = new HashMap<>();
	static {
		CONVERTERS.put("%", d -> "%");
		CONVERTERS.put("a", d -> WeekdayNameAbbrev[d.get(Calendar.DAY_OF_WEEK)-1]);
		CONVERTERS.put("A", d -> WeekdayName[d.get(Calendar.DAY_OF_WEEK)-1]);
		CONVERTERS.put("b", d -> MonthNameAbbrev[d.get(Calendar.MONTH)]);
		CONVERTERS.put("B", d -> MonthName[d.get(Calendar.MONTH)]);
		CONVERTERS.put("c", d -> date("%a %b %e %H:%M:%S %Y", d.getTimeInMillis()/1000L));
		CONVERTERS.put("C", d -> String.valueOf(d.get(Calendar.YEAR)).substring(0, 2));
		CONVERTERS.put("d", d -> String.valueOf(100+d.get(Calendar.DAY_OF_MONTH)).substring(1));
		CONVERTERS.put("D", d -> date("%m/%d/%y", d.getTimeInMillis()/1000L));
		CONVERTERS.put("e", d -> String.format("%2d", d.get(Calendar.DAY_OF_MONTH)));
		CONVERTERS.put("F", d -> date("%Y-%m-%d", d.getTimeInMillis()/1000L));
		CONVERTERS.put("g", d -> String.valueOf(d.get(Calendar.YEAR)).substring(2));
		CONVERTERS.put("G", d -> String.valueOf(d.get(Calendar.YEAR)));
		CONVERTERS.put("h", d -> MonthNameAbbrev[d.get(Calendar.MONTH)]);
		CONVERTERS.put("H", d -> String.valueOf(100+d.get(Calendar.HOUR_OF_DAY)).substring(1));
		CONVERTERS.put("I", d -> String.valueOf(100+d.get(Calendar.HOUR_OF_DAY)%12).substring(1));
		// day of year
		CONVERTERS.put("j", d -> {
			Calendar y0 = beginningOfYear(d);
			int dayOfYear = (int) ((d.getTimeInMillis()-y0.getTimeInMillis())/(24*3600L*1000L));
			return String.valueOf(1001+dayOfYear).substring(1);
		});
		CONVERTERS.put("m", d -> String.valueOf(101+d.get(Calendar.MONTH)).substring(1));
		CONVERTERS.put("M", d -> String.valueOf(100+d.get(Calendar.MINUTE)).substring(1));
		CONVERTERS.put("n", d -> "\n");
		CONVERTERS.put("p", d -> d.get(Calendar.HOUR_OF_DAY) < 12? "AM": "PM");
		CONVERTERS.put("r", d -> date("%I:%M:%S %p", d.getTimeInMillis()/1000L));
		CONVERTERS.put("R", d -> date("%H:%M", d.getTimeInMillis()/1000L));
		CONVERTERS.put("S", d -> String.valueOf(100+d.get(Calendar.SECOND)).substring(1));
		CONVERTERS.put("t", d -> "\t");
		CONVERTERS.put("T", d -> date("%H:%M:%S", d.getTimeInMillis()/1000L));
		CONVERTERS.put("u", d -> String.valueOf((d.get(Calendar.DAY_OF_WEEK)+6)%7));
		CONVERTERS.put("U", d -> String.valueOf(weekNumber(d, 0)));
		CONVERTERS.put("V", d -> String.valueOf(weekNumber(d, 0)));
		CONVERTERS.put("w", d -> String.valueOf((d.get(Calendar.DAY_OF_WEEK)+6)%7));
		CONVERTERS.put("W", d -> String.valueOf(weekNumber(d, 1)));
		CONVERTERS.put("x", d -> date("%m/%d/%y", d.getTimeInMillis()/1000L));
		CONVERTERS.put("X", d -> date("%H:%M:%S", d.getTimeInMillis()/1000L));
		CONVERTERS.put("y", d -> String.valueOf(d.get(Calendar.YEAR)).substring(2));
		CONVERTERS.put("Y", d -> String.valueOf(d.get(Calendar.YEAR)));
		CONVERTERS.put("z", d -> {
			final int tzo = timeZoneOffset(d)/60;
			return "%+03d%02d".formatted(tzo / 60, Math.abs(tzo % 60));
		});
		CONVERTERS.put("Z", d -> d.getTimeZone().toZoneId().getDisplayName(TextStyle.SHORT, Locale.getDefault()));
	}

	private static Calendar beginningOfYear(Calendar d) {
		Calendar y0 = Calendar.getInstance();
		y0.setTime(d.getTime());
		y0.set(Calendar.MONTH, 0);
		y0.set(Calendar.DAY_OF_MONTH, 1);
		y0.set(Calendar.HOUR_OF_DAY, 0);
		y0.set(Calendar.MINUTE, 0);
		y0.set(Calendar.SECOND, 0);
		y0.set(Calendar.MILLISECOND, 0);
		return y0;
	}

	private static int weekNumber(Calendar d, int startDay) {
		Calendar y0 = beginningOfYear(d);
		y0.set(Calendar.DAY_OF_MONTH, 1+(startDay+8-y0.get(Calendar.DAY_OF_WEEK))%7);
		if (y0.after(d)) {
			y0.set(Calendar.YEAR, y0.get(Calendar.YEAR)-1);
			y0.set(Calendar.DAY_OF_MONTH, 1+(startDay+8-y0.get(Calendar.DAY_OF_WEEK))%7);
		}
		long dt = d.getTime().getTime()-y0.getTime().getTime();
		return 1+(int) (dt/(7L*24L*3600L*1000L));
	}

	private static int timeZoneOffset(Calendar d) {
		int localStandarTimeMillis = (d.get(Calendar.HOUR_OF_DAY)*3600+d.get(Calendar.MINUTE)*60+d.get(Calendar.SECOND))
			*1000;
		return d.getTimeZone().getOffset(1, d.get(Calendar.YEAR), d.get(Calendar.MONTH), d.get(Calendar.DAY_OF_MONTH),
			d.get(Calendar.DAY_OF_WEEK), localStandarTimeMillis)/1000;
	}

	private boolean isDaylightSavingsTime(Calendar d) {
		return d.getTimeZone().useDaylightTime();
	}

	/**
	 * This function is equivalent to the C function system. It passes command
	 * to be executed by an operating system shell. It returns a status code,
	 * which is system-dependent. If command is absent, then it returns nonzero
	 * if a shell is available and zero otherwise.
	 *
	 * @param command command to pass to the system
	 */
	protected Varargs execute(String command) {
		return varargsOf(NIL, valueOf("exit"), ONE);
	}

	/**
	 * Calls the C function exit, with an optional code, to terminate the host
	 * program.
	 *
	 * @param code exit code of the program
	 */
	protected void exit(int code) {
		System.exit(code);
	}

	/**
	 * Returns the value of the process environment variable varname, or the
	 * System property value for varname, or null if the variable is not defined
	 * in either environment.
	 *
	 * The default implementation, which is used by the JmePlatform, only
	 * queryies System.getProperty().
	 *
	 * The JsePlatform overrides this behavior and returns the environment
	 * variable value using System.getenv() if it exists, or the System property
	 * value if it does not.
	 *
	 * A SecurityException may be thrown if access is not allowed for 'varname'.
	 *
	 * @param varname name of the environment property
	 * @return String value, or null if not defined
	 */
	protected String getenv(String varname) {
		return System.getProperty(varname);
	}

	/**
	 * Deletes the file or directory with the given name. Directories must be
	 * empty to be removed. If this function fails, it throws and IOException
	 *
	 * @param filename
	 * @throws IOException if it fails
	 */
	protected void remove(String filename) throws IOException {
		throw new IOException("not implemented");
	}

	/**
	 * Renames file or directory named oldname to newname. If this function
	 * fails,it throws and IOException
	 *
	 * @param oldname old file name
	 * @param newname new file name
	 * @throws IOException if it fails
	 */
	protected void rename(String oldname, String newname) throws IOException {
		throw new IOException("not implemented");
	}

	/**
	 * Sets the current locale of the program. <code>locale</code> is a string specifying a
	 * locale; category is an optional string describing which category to change:
	 * <code>"all"</code>, <code>"collate"</code>, <code>"ctype"</code>, <code>"monetary"</code>,
	 * <code>"numeric"</code>, or <code>"time"</code>;
	 * the default category is <code>"all"</code>.
	 *
	 * If locale is the empty string, the current locale is set to an implementation-defined
	 * native locale. If locale is the string <code>"C"</code>, the current locale is set to
	 * the standard C locale.
	 *
	 * When called with null as the first argument, this function only returns the name of the
	 * current locale for the given category.
	 *
	 * @param locale   
	 * @param category category of the locale to modify
	 * @return the name of the new locale, or null if the request cannot be
	 *         honored.
	 */
	protected String setlocale(String locale, String category) {
		return "C";
	}

	/**
	 * Returns the current time when called without arguments, or a time
	 * representing the date and time specified by the given table. This table
	 * must have fields year, month, and day, and may have fields hour, min,
	 * sec, and isdst (for a description of these fields, see the os.date
	 * function).
	 *
	 * @param table
	 * @return long value for the time
	 */
	protected long time(LuaTable table) {
		java.util.Date d;
		if (table == null) {
			d = new java.util.Date();
		} else {
			Calendar c = Calendar.getInstance();
			c.set(Calendar.YEAR, table.get("year").checkint());
			c.set(Calendar.MONTH, table.get("month").checkint()-1);
			c.set(Calendar.DAY_OF_MONTH, table.get("day").checkint());
			c.set(Calendar.HOUR_OF_DAY, table.get("hour").optint(12));
			c.set(Calendar.MINUTE, table.get("min").optint(0));
			c.set(Calendar.SECOND, table.get("sec").optint(0));
			c.set(Calendar.MILLISECOND, 0);
			d = c.getTime();
		}
		return d.getTime()/1000L;
	}

	/**
	 * Returns a string with a file name that can be used for a temporary file.
	 * The file must be explicitly opened before its use and explicitly removed
	 * when no longer needed.
	 *
	 * On some systems (POSIX), this function also creates a file with that
	 * name, to avoid security risks. (Someone else might create the file with
	 * wrong permissions in the time between getting the name and creating the
	 * file.) You still have to open the file to use it and to remove it (even
	 * if you do not use it).
	 *
	 * @return String filename to use
	 */
	protected String tmpname() {
		synchronized (OsLib.class) {
			return TMP_PREFIX+tmpnames+++TMP_SUFFIX;
		}
	}
}
