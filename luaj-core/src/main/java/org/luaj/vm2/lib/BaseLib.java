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
import java.io.InputStream;

/**
 * Subclass of {@link LibFunction} which implements the lua basic library
 * functions.
 * <p>
 * This contains all library functions listed as "basic functions" in the lua
 * documentation for JME. The functions dofile and loadfile use the
 * {@link Globals#finder} instance to find resource files. Since JME has no file
 * system by default, {@link BaseLib} implements {@link ResourceFinder} using
 * {@link Class#getResource(String)}, which is the closest equivalent on JME.
 * The default loader chain in {@link PackageLib} will use these as well.
 * <p>
 * To use basic library functions that include a {@link ResourceFinder} based on
 * directory lookup, use {@link org.luaj.vm2.lib.jse.JseBaseLib} instead.
 * <p>
 * Typically, this library is included as part of a call to either
 * {@link org.luaj.vm2.lib.jse.JsePlatform#standardGlobals()} or
 * {@link org.luaj.vm2.lib.jme.JmePlatform#standardGlobals()}
 *
 * <pre>
 * {
 * 	&#64;code
 * 	Globals globals = JsePlatform.standardGlobals();
 * 	globals.get("print").call(LuaValue.valueOf("hello, world"));
 * }
 * </pre>
 * <p>
 * For special cases where the smallest possible footprint is desired, a minimal
 * set of libraries could be loaded directly via {@link Globals#load(LuaValue)}
 * using code such as:
 *
 * <pre>
 * {
 * 	&#64;code
 * 	Globals globals = new Globals();
 * 	globals.load(new JseBaseLib());
 * 	globals.get("print").call(LuaValue.valueOf("hello, world"));
 * }
 * </pre>
 *
 * Doing so will ensure the library is properly initialized and loaded into the
 * globals table.
 * <p>
 * This is a direct port of the corresponding library in C.
 *
 * @see org.luaj.vm2.lib.jse.JseBaseLib
 * @see ResourceFinder
 * @see Globals#finder
 * @see LibFunction
 * @see org.luaj.vm2.lib.jse.JsePlatform
 * @see org.luaj.vm2.lib.jme.JmePlatform
 * @see <a href="http://www.lua.org/manual/5.2/manual.html#6.1">Lua 5.2 Base Lib
 *      Reference</a>
 */
public class BaseLib extends TwoArgFunction implements ResourceFinder {

	Globals globals;

	/**
	 * Perform one-time initialization on the library by adding base functions
	 * to the supplied environment, and returning it as the return value.
	 *
	 * @param modname the module name supplied if this is loaded via 'require'.
	 * @param env     the environment to load into, which must be a Globals
	 *                instance.
	 */
	@Override
	public LuaValue call(LuaValue modname, LuaValue env) {
		globals = env.checkglobals();
		globals.finder = this;
		globals.baselib = this;
		env.set("_G", env);
		env.set("_VERSION", Lua._VERSION);
		env.set("assert", new Assert());
		env.set("collectgarbage", new CollectGarbage());
		env.set("dofile", new DoFile());
		env.set("error", new Error());
		env.set("getmetatable", new GetMetatable());
		env.set("load", new Load());
		env.set("loadfile", new LoadFile());
		env.set("pcall", new PCall());
		env.set("print", new Print(this));
		env.set("rawequal", new RawEqual());
		env.set("rawget", new RawGet());
		env.set("rawlen", new RawLen());
		env.set("rawset", new RawSet());
		env.set("select", new Select());
		env.set("setmetatable", new SetMetatable());
		env.set("tonumber", new ToNumber());
		env.set("tostring", new ToString());
		env.set("type", new Type());
		env.set("xpcall", new XPCall());

		Next next;
		env.set("next", next = new Next());
		env.set("pairs", new Pairs(next));
		env.set("ipairs", new IPairs());

		return env;
	}

	/**
	 * ResourceFinder implementation
	 *
	 * Tries to open the file as a resource, which can work for JSE and JME.
	 */
	@Override
	public InputStream findResource(String filename) {
		return getClass().getResourceAsStream(filename.startsWith("/")? filename: "/" + filename);
	}

	// "assert", // ( v [,message] ) -> v, message | ERR
	static final class Assert extends VarArgFunction {
		@Override
		public Varargs invoke(Varargs args) {
			if (!args.arg1().toboolean())
				error(args.narg() > 1? args.optjstring(2, "assertion failed!"): "assertion failed!");
			return args;
		}
	}

	// "collectgarbage", // ( opt [,arg] ) -> value
	static final class CollectGarbage extends VarArgFunction {
		@Override
		public Varargs invoke(Varargs args) {
			String s = args.optjstring(1, "collect");
			if ("collect".equals(s)) {
				System.gc();
				return ZERO;
			} else if ("count".equals(s)) {
				Runtime rt = Runtime.getRuntime();
				long used = rt.totalMemory()-rt.freeMemory();
				return varargsOf(valueOf(used/1024.), valueOf(used%1024));
			} else if ("step".equals(s)) {
				System.gc();
				return LuaValue.TRUE;
			} else {
				argerror(1, "invalid option '" + s + "'");
			}
			return NIL;
		}
	}

	// "dofile", // ( filename ) -> result1, ...
	final class DoFile extends VarArgFunction {
		@Override
		public Varargs invoke(Varargs args) {
			args.argcheck(args.isstring(1) || args.isnil(1), 1, "filename must be string or nil");
			String filename = args.isstring(1)? args.tojstring(1): null;
			Varargs v = filename == null? loadStream(globals.STDIN, "=stdin", "bt", globals)
				: loadFile(args.checkjstring(1), "bt", globals);
			return v.isnil(1)? error(v.tojstring(2)): v.arg1().invoke();
		}
	}

	// "error", // ( message [,level] ) -> ERR
	static final class Error extends TwoArgFunction {
		@Override
		public LuaValue call(LuaValue arg1, LuaValue arg2) {
			if (arg1.isnil())
				throw new LuaError(NIL);
			if (!arg1.isstring() || arg2.optint(1) == 0)
				throw new LuaError(arg1);
			throw new LuaError(arg1.tojstring(), arg2.optint(1));
		}
	}

	// "getmetatable", // ( object ) -> table
	static final class GetMetatable extends LibFunction {
		@Override
		public LuaValue call() {
			return argerror(1, "value expected");
		}

		@Override
		public LuaValue call(LuaValue arg) {
			LuaValue mt = arg.getmetatable();
			return mt != null? mt.rawget(METATABLE).optvalue(mt): NIL;
		}
	}

	// "load", // ( ld [, source [, mode [, env]]] ) -> chunk | nil, msg
	final class Load extends VarArgFunction {
		@Override
		public Varargs invoke(Varargs args) {
			LuaValue ld = args.arg1();
			if (!ld.isstring() && !ld.isfunction()) {
				throw new LuaError(
					"bad argument #1 to 'load' (string or function expected, got " + ld.typename() + ")");
			}
			String source = args.optjstring(2, ld.isstring()? ld.tojstring(): "=(load)");
			String mode = args.optjstring(3, "bt");
			LuaValue env = args.optvalue(4, globals);
			return loadStream(ld.isstring()? ld.strvalue().toInputStream(): new StringInputStream(ld.checkfunction()),
				source, mode, env);
		}
	}

	// "loadfile", // ( [filename [, mode [, env]]] ) -> chunk | nil, msg
	final class LoadFile extends VarArgFunction {
		@Override
		public Varargs invoke(Varargs args) {
			args.argcheck(args.isstring(1) || args.isnil(1), 1, "filename must be string or nil");
			String filename = args.isstring(1)? args.tojstring(1): null;
			String mode = args.optjstring(2, "bt");
			LuaValue env = args.optvalue(3, globals);
			return filename == null? loadStream(globals.STDIN, "=stdin", mode, env): loadFile(filename, mode, env);
		}
	}

	// "pcall", // (f, arg1, ...) -> status, result1, ...
	final class PCall extends VarArgFunction {
		@Override
		public Varargs invoke(Varargs args) {
			LuaValue func = args.checkvalue(1);
			if (globals != null && globals.debuglib != null)
				globals.debuglib.onCall(this);
			try {
				return varargsOf(TRUE, func.invoke(args.subargs(2)));
			} catch (LuaError le) {
				final LuaValue m = le.getMessageObject();
				return varargsOf(FALSE, m != null? m: NIL);
			} catch (Exception e) {
				final String m = e.getMessage();
				return varargsOf(FALSE, valueOf(m != null? m: e.toString()));
			} finally {
				if (globals != null && globals.debuglib != null)
					globals.debuglib.onReturn();
			}
		}
	}

	// "print", // (...) -> void
	final class Print extends VarArgFunction {
		final BaseLib baselib;

		Print(BaseLib baselib) {
			this.baselib = baselib;
		}

		@Override
		public Varargs invoke(Varargs args) {
			LuaValue tostring = globals.get("tostring");
			for (int i = 1, n = args.narg(); i <= n; i++) {
				if (i > 1)
					globals.STDOUT.print('\t');
				LuaString s = tostring.call(args.arg(i)).strvalue();
				globals.STDOUT.print(s.tojstring());
			}
			globals.STDOUT.print('\n');
			return NONE;
		}
	}

	// "rawequal", // (v1, v2) -> boolean
	static final class RawEqual extends LibFunction {
		@Override
		public LuaValue call() {
			return argerror(1, "value expected");
		}

		@Override
		public LuaValue call(LuaValue arg) {
			return argerror(2, "value expected");
		}

		@Override
		public LuaValue call(LuaValue arg1, LuaValue arg2) {
			return valueOf(arg1.raweq(arg2));
		}
	}

	// "rawget", // (table, index) -> value
	static final class RawGet extends TableLibFunction {
		@Override
		public LuaValue call(LuaValue arg) {
			return argerror(2, "value expected");
		}

		@Override
		public LuaValue call(LuaValue arg1, LuaValue arg2) {
			return arg1.checktable().rawget(arg2);
		}
	}

	// "rawlen", // (v) -> value
	static final class RawLen extends LibFunction {
		@Override
		public LuaValue call(LuaValue arg) {
			return valueOf(arg.rawlen());
		}
	}

	// "rawset", // (table, index, value) -> table
	static final class RawSet extends TableLibFunction {
		@Override
		public LuaValue call(LuaValue table) {
			return argerror(2, "value expected");
		}

		@Override
		public LuaValue call(LuaValue table, LuaValue index) {
			return argerror(3, "value expected");
		}

		@Override
		public LuaValue call(LuaValue table, LuaValue index, LuaValue value) {
			LuaTable t = table.checktable();
			if (!index.isvalidkey())
				argerror(2, "table index is nil");
			t.rawset(index, value);
			return t;
		}
	}

	// "select", // (f, ...) -> value1, ...
	static final class Select extends VarArgFunction {
		@Override
		public Varargs invoke(Varargs args) {
			int n = args.narg()-1;
			if (args.arg1().equals(valueOf("#")))
				return valueOf(n);
			int i = args.checkint(1);
			if (i == 0 || i < -n)
				argerror(1, "index out of range");
			return args.subargs(i < 0? n+i+2: i+1);
		}
	}

	// "setmetatable", // (table, metatable) -> table
	static final class SetMetatable extends TableLibFunction {
		@Override
		public LuaValue call(LuaValue table) {
			return argerror(2, "nil or table expected");
		}

		@Override
		public LuaValue call(LuaValue table, LuaValue metatable) {
			final LuaValue mt0 = table.checktable().getmetatable();
			if (mt0 != null && !mt0.rawget(METATABLE).isnil())
				error("cannot change a protected metatable");
			return table.setmetatable(metatable.isnil()? null: metatable.checktable());
		}
	}

	// "tonumber", // (e [,base]) -> value
	static final class ToNumber extends LibFunction {
		@Override
		public LuaValue call(LuaValue e) {
			return e.tonumber();
		}

		@Override
		public LuaValue call(LuaValue e, LuaValue base) {
			if (base.isnil())
				return e.tonumber();
			final int b = base.checkint();
			if (b < 2 || b > 36)
				argerror(2, "base out of range");
			return e.checkstring().tonumber(b);
		}
	}

	// "tostring", // (e) -> value
	static final class ToString extends LibFunction {
		@Override
		public LuaValue call(LuaValue arg) {
			LuaValue h = arg.metatag(TOSTRING);
			if (!h.isnil())
				return h.call(arg);
			LuaValue v = arg.tostring();
			if (!v.isnil())
				return v;
			return valueOf(arg.tojstring());
		}
	}

	// "type",  // (v) -> value
	static final class Type extends LibFunction {
		@Override
		public LuaValue call(LuaValue arg) {
			return valueOf(arg.typename());
		}
	}

	// "xpcall", // (f, err) -> result1, ...
	final class XPCall extends VarArgFunction {
		@Override
		public Varargs invoke(Varargs args) {
			final LuaThread t = globals.running;
			final LuaValue preverror = t.errorfunc;
			t.errorfunc = args.checkvalue(2);
			try {
				if (globals != null && globals.debuglib != null)
					globals.debuglib.onCall(this);
				try {
					return varargsOf(TRUE, args.arg1().invoke(args.subargs(3)));
				} catch (LuaError le) {
					final LuaValue m = le.getMessageObject();
					return varargsOf(FALSE, m != null? m: NIL);
				} catch (Exception e) {
					final String m = e.getMessage();
					return varargsOf(FALSE, valueOf(m != null? m: e.toString()));
				} finally {
					if (globals != null && globals.debuglib != null)
						globals.debuglib.onReturn();
				}
			} finally {
				t.errorfunc = preverror;
			}
		}
	}

	// "pairs" (t) -> iter-func, t, nil
	static final class Pairs extends VarArgFunction {
		final Next next;

		Pairs(Next next) {
			this.next = next;
		}

		@Override
		public Varargs invoke(Varargs args) {
			return varargsOf(next, args.checktable(1), NIL);
		}
	}

	// // "ipairs", // (t) -> iter-func, t, 0
	static final class IPairs extends VarArgFunction {
		INext inext = new INext();

		@Override
		public Varargs invoke(Varargs args) {
			return varargsOf(inext, args.checktable(1), ZERO);
		}
	}

	// "next"  ( table, [index] ) -> next-index, next-value
	static final class Next extends VarArgFunction {
		@Override
		public Varargs invoke(Varargs args) {
			return args.checktable(1).next(args.arg(2));
		}
	}

	// "inext" ( table, [int-index] ) -> next-index, next-value
	static final class INext extends VarArgFunction {
		@Override
		public Varargs invoke(Varargs args) {
			return args.checktable(1).inext(args.arg(2));
		}
	}

	/**
	 * Load from a named file, returning the chunk or nil,error of can't load
	 *
	 * @param env
	 * @param mode
	 * @return Varargs containing chunk, or NIL,error-text on error
	 */
	public Varargs loadFile(String filename, String mode, LuaValue env) {
		InputStream is = globals.finder.findResource(filename);
		if (is == null)
			return varargsOf(NIL, valueOf("cannot open " + filename + ": No such file or directory"));
		try {
			return loadStream(is, "@" + filename, mode, env);
		} finally {
			try {
				is.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	public Varargs loadStream(InputStream is, String chunkname, String mode, LuaValue env) {
		try {
			if (is == null)
				return varargsOf(NIL, valueOf("not found: " + chunkname));
			return globals.load(is, chunkname, mode, env);
		} catch (Exception e) {
			return varargsOf(NIL, valueOf(e.getMessage()));
		}
	}

	private static class StringInputStream extends InputStream {
		final LuaValue func;
		byte[]         bytes;
		int            offset, remaining = 0;

		StringInputStream(LuaValue func) {
			this.func = func;
		}

		@Override
		public int read() throws IOException {
			if (remaining < 0)
				return -1;
			if (remaining == 0) {
				LuaValue s = func.call();
				if (s.isnil())
					return remaining = -1;
				LuaString ls = s.strvalue();
				bytes = ls.m_bytes;
				offset = ls.m_offset;
				remaining = ls.m_length;
				if (remaining <= 0)
					return -1;
			}
			--remaining;
			return 0xFF & bytes[offset++];
		}
	}
}
