/*******************************************************************************
* Copyright (c) 2010 Luaj.org. All rights reserved.
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
package org.luaj.vm2.luajc;

import org.apache.bcel.Const;
import org.apache.bcel.generic.*;
import org.luaj.vm2.*;
import org.luaj.vm2.lib.*;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class JavaBuilder {

	private enum Types {
		CHAR        (Type.CHAR),
		INT         (Type.INT),
		DOUBLE      (Type.DOUBLE),
		STRING      (String.class),
		BUFFER      (Buffer.class),
		VARARGS     (Varargs.class),
		LUAVALUE    (LuaValue.class),
		LUASTRING   (LuaString.class),
		LUAINTEGER  (LuaInteger.class),
		LUANUMBER   (LuaNumber.class),
		LUABOOLEAN  (LuaBoolean.class),
		LUATABLE    (LuaTable.class),
		FUNCV       (VarArgFunction.class),
		FUNC0       (ZeroArgFunction.class),
		FUNC1       (OneArgFunction.class),
		FUNC2       (TwoArgFunction.class),
		FUNC3       (ThreeArgFunction.class),
		JSEPLATFORM ("org.luaj.vm2.lib.jse.JsePlatform"),
		;
		private final String name;
		private final ObjectType object;
		private final BasicType basic;
		private final ArrayType array;
		Types(Class<?> klass) {
			this(klass.getName());
		}

		Types(String className){
			this.name = className;
			this.object = new ObjectType(name);
			this.array = new ArrayType(object, 1);
			this.basic = null;
		}

		Types(BasicType type) {
			this.name = type.getSignature();
			this.basic = type;
			this.array = new ArrayType(type, 1);
			this.object = null;
		}
	}

	// argument list types
	private enum ArgTypes {
		NONE                  (new Type[]{ }                                                                    , new String[]{ }),
		INT                   (new Type[]{ Types.INT.basic }                                                    , new String[]{ "i" }),
		DOUBLE                (new Type[]{ Types.DOUBLE.basic }                                                 , new String[]{ "d" }),
		STRING                (new Type[]{ Types.STRING.basic }                                                 , new String[]{ "s" }),
		CHARARRAY             (new Type[]{ Types.CHAR.array }                                                   , new String[]{ "chars" }),
		BUFFER                (new Type[]{ Types.BUFFER.object }                                                , new String[]{ "buffer" }),
		STRINGARRAY           (new Type[]{ Types.STRING.array }                                                 , new String[]{ "strings" }),
		LUAVALUE_STRINGARRAY  (new Type[]{ Types.LUAVALUE.object, Types.STRING.array }                          , new String[]{ "arg", "strings" }),
		VARARGS               (new Type[]{ Types.VARARGS.object }                                               , new String[]{ "args" }),
		LUAVALUE              (new Type[]{ Types.LUAVALUE.object }                                              , new String[]{ "arg" }),
		LUAVALUE_VARARGS      (new Type[]{ Types.LUAVALUE.object, Types.VARARGS.object }                        , new String[]{ "arg1", "args"}),
		LUAVALUE_2            (new Type[]{ Types.LUAVALUE.object, Types.LUAVALUE.object }                       , new String[]{ "arg1", "arg2" }),
		LUAVALUE_2_VARARGS    (new Type[]{ Types.LUAVALUE.object, Types.LUAVALUE.object, Types.VARARGS.object } , new String[]{ "arg1", "arg2", "args" }),
		LUAVALUE_3            (new Type[]{ Types.LUAVALUE.object, Types.LUAVALUE.object, Types.LUAVALUE.object }, new String[]{ "arg1", "arg2", "arg3" }),
		LUAVALUEARRAY         (new Type[]{ Types.LUAVALUE.array }                                               , new String[]{ "values" }),
		LUAVALUEARRAY_VARARGS (new Type[]{ Types.LUAVALUE.array, Types.VARARGS.object }                         , new String[]{ "values", "args" }),
		INT_LUAVALUE          (new Type[]{ Type.INT, Types.LUAVALUE.object }                                    , new String[]{ "i", "arg" }),
		INT_VARARGS           (new Type[]{ Type.INT, Types.VARARGS.object }                                     , new String[]{ "i", "args" }),
		INT_INT               (new Type[]{ Type.INT, Type.INT }                                                 , new String[]{ "i", "j" }),
		;
		private final Type[] types;
		private final String[] defaultNames;

		ArgTypes(Type[] types, String[] defaultNames) {
			this.types = types;
			this.defaultNames = defaultNames;
		}
	}

	// names, arg types for main prototype classes
	private enum MainPrototype {
		Func0(Types.FUNC0.name, Types.LUAVALUE.object, ArgTypes.NONE,       "call"    ),
		Func1(Types.FUNC1.name, Types.LUAVALUE.object, ArgTypes.LUAVALUE,   "call"    ),
		Func2(Types.FUNC2.name, Types.LUAVALUE.object, ArgTypes.LUAVALUE_2, "call"    ),
		Func3(Types.FUNC3.name, Types.LUAVALUE.object, ArgTypes.LUAVALUE_3, "call"    ),
		FuncV(Types.FUNCV.name, Types.VARARGS.object,  ArgTypes.VARARGS,    "onInvoke"),
		;
		public final String superClassName;
		public final ObjectType returnType;
		public final Type[] argTypes;
		public final String[] argNames;
		public final String implName;

		MainPrototype(String superClassName, ObjectType returnType, ArgTypes args, String implName) {
			this.superClassName = superClassName;
			this.returnType = returnType;
			this.argTypes = args.types;
			this.argNames = args.defaultNames;
			this.implName = implName;
		}
	}
	// varable naming
	private static final String PREFIX_CONSTANT     = "k";
	private static final String PREFIX_UPVALUE      = "u";
	private static final String PREFIX_PLAIN_SLOT   = "s";
	private static final String PREFIX_UPVALUE_SLOT = "a";
	private static final String NAME_VARRESULT      = "v";

	// basic info
	private final ProtoInfo pi;
	private final Prototype p;
	private final String    classname;

	// bcel variables
	private final ClassGen           cg;
	private final ConstantPoolGen    cp;
	private final InstructionFactory factory;

	// main instruction list for the main function of this class
	private final InstructionList init;
	private final InstructionList main;
	private final MethodGen       mg;

	// the superclass arg count, 0-3 args, 4=varargs
	private int        superclassType;
	private static int SUPERTYPE_VARARGS = 4;

	// storage for goto locations
	private final int[]               targets;
	private final BranchInstruction[] branches;
	private final InstructionHandle[] branchDestHandles;
	private final InstructionHandle[] lastInstrHandles;
	private InstructionHandle         beginningOfLuaInstruction;

	// hold vararg result
	private LocalVariableGen varresult = null;
	private int              prev_line = -1;

	public JavaBuilder(ProtoInfo pi, String classname, String filename) {
		this.pi = pi;
		this.p = pi.prototype;
		this.classname = classname;

		// which class to inherit from
		superclassType = p.numparams;
		if (p.is_vararg != 0 || superclassType >= SUPERTYPE_VARARGS)
			superclassType = SUPERTYPE_VARARGS;
		for (int inst : p.code) {
			int o = Lua.GET_OPCODE(inst);
			if (o == Lua.OP_TAILCALL || o == Lua.OP_RETURN && (Lua.GETARG_B(inst) < 1 || Lua.GETARG_B(inst) > 2)) {
				superclassType = SUPERTYPE_VARARGS;
				break;
			}
		}

		MainPrototype prototype = MainPrototype.values()[superclassType];
		
		// create class generator
		cg = new ClassGen(classname, prototype.superClassName, filename, Const.ACC_PUBLIC | Const.ACC_SUPER, null);
		cp = cg.getConstantPool(); // cg creates constant pool

		// main instruction lists
		factory = new InstructionFactory(cg);
		init = new InstructionList();
		main = new InstructionList();

		// create the fields
		for (int i = 0; i < p.upvalues.length; i++) {
			boolean isrw = pi.isReadWriteUpvalue(pi.upvals[i]);
			Type uptype = isrw? Types.LUAVALUE.array : Types.LUAVALUE.object;
			FieldGen fg = new FieldGen(0, uptype, upvalueName(i), cp);
			cg.addField(fg.getField());
		}

		// create the method
		mg = new MethodGen(
				Const.ACC_PUBLIC | Const.ACC_FINAL, // access flags
				prototype.returnType, // return type
				prototype.argTypes, // argument types
				prototype.argNames, // arg names
				prototype.implName, // method
				Types.LUAVALUE.name, // defining class
				main, cp
		);

		// initialize the values in the slots
		initializeSlots();

		// initialize branching
		int nc = p.code.length;
		targets = new int[nc];
		branches = new BranchInstruction[nc];
		branchDestHandles = new InstructionHandle[nc];
		lastInstrHandles = new InstructionHandle[nc];
	}

	public void initializeSlots() {
		int slot = 0;
		createUpvalues(-1, 0, p.maxstacksize);
		if (superclassType == SUPERTYPE_VARARGS) {
			for (slot = 0; slot < p.numparams; slot++) {
				if (pi.isInitialValueUsed(slot)) {
					append(new ALOAD(1));
					append(new PUSH(cp, slot+1));
					append(factory.createInvoke(Types.VARARGS.name, "arg", Types.LUAVALUE.object, ArgTypes.INT.types, Const.INVOKEVIRTUAL));
					storeLocal(-1, slot);
				}
			}
			append(new ALOAD(1));
			append(new PUSH(cp, 1+p.numparams));
			append(factory.createInvoke(Types.VARARGS.name, "subargs", Types.VARARGS.object, ArgTypes.INT.types, Const.INVOKEVIRTUAL));
			append(new ASTORE(1));
		} else {
			// fixed arg function between 0 and 3 arguments
			for (slot = 0; slot < p.numparams; slot++) {
				this.plainSlotVars.put(slot, 1+slot);
				if (pi.isUpvalueCreate(-1, slot)) {
					append(new ALOAD(1+slot));
					storeLocal(-1, slot);
				}
			}
		}

		// nil parameters
		// TODO: remove this for lua 5.2, not needed
		for (; slot < p.maxstacksize; slot++) {
			if (pi.isInitialValueUsed(slot)) {
				loadNil();
				storeLocal(-1, slot);
			}
		}
	}

	public byte[] completeClass(boolean genmain) {

		// add class initializer
		if (!init.isEmpty()) {
			MethodGen mg = new MethodGen(Const.ACC_STATIC, Type.VOID, ArgTypes.NONE.types, new String[] {}, "<clinit>",
				cg.getClassName(), init, cg.getConstantPool());
			init.append(InstructionConst.RETURN);
			mg.setMaxStack();
			cg.addMethod(mg.getMethod());
			init.dispose();
		}

		// add default constructor
		cg.addEmptyConstructor(Const.ACC_PUBLIC);

		// gen method
		resolveBranches();
		mg.setMaxStack();
		cg.addMethod(mg.getMethod());
		main.dispose();

		// add initupvalue1(LuaValue env) to initialize environment for main chunk
		if (p.upvalues.length == 1 && superclassType == SUPERTYPE_VARARGS) {
			MethodGen mg = new MethodGen(Const.ACC_PUBLIC | Const.ACC_FINAL, // access flags
				Type.VOID, // return type
				ArgTypes.LUAVALUE.types, // argument types
				new String[] { "env" }, // arg names
				"initupvalue1", Types.LUAVALUE.name, // method, defining class
				main, cp);
			boolean isrw = pi.isReadWriteUpvalue(pi.upvals[0]);
			append(InstructionConst.THIS);
			append(new ALOAD(1));
			if (isrw) {
				append(factory.createInvoke(classname, "newupl", Types.LUAVALUE.array, ArgTypes.LUAVALUE.types,
					Const.INVOKESTATIC));
				append(factory.createFieldAccess(classname, upvalueName(0), Types.LUAVALUE.array, Const.PUTFIELD));
			} else {
				append(factory.createFieldAccess(classname, upvalueName(0), Types.LUAVALUE.object, Const.PUTFIELD));
			}
			append(InstructionConst.RETURN);
			mg.setMaxStack();
			cg.addMethod(mg.getMethod());
			main.dispose();
		}

		// add main function so class is invokable from the java command line
		if (genmain) {
			MethodGen mg = new MethodGen(
					Const.ACC_PUBLIC | Const.ACC_STATIC, // access flags
					Type.VOID, // return type
					ArgTypes.STRINGARRAY.types, // argument types
					new String[] { "arg" }, // arg names
					"main", classname, // method, defining class
					main, cp
			);
			append(factory.createNew(classname));
			append(InstructionConst.DUP);
			append(factory.createInvoke(classname, Const.CONSTRUCTOR_NAME, Type.VOID, ArgTypes.NONE.types, Const.INVOKESPECIAL));
			append(new ALOAD(0));
			append(factory.createInvoke(Types.JSEPLATFORM.name, "luaMain", Type.VOID, ArgTypes.LUAVALUE_STRINGARRAY.types, Const.INVOKESTATIC));
			append(InstructionConst.RETURN);
			mg.setMaxStack();
			cg.addMethod(mg.getMethod());
			main.dispose();
		}

		// convert to class bytes
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			cg.getJavaClass().dump(baos);
			return baos.toByteArray();
		} catch (IOException ioe) {
			throw new RuntimeException("JavaClass.dump() threw " + ioe);
		}
	}

	public void dup() {
		append(InstructionConst.DUP);
	}

	public void pop() {
		append(InstructionConst.POP);
	}

	public void loadNil() {
		append(factory.createFieldAccess(Types.LUAVALUE.name, "NIL", Types.LUAVALUE.object, Const.GETSTATIC));
	}

	public void loadNone() {
		append(factory.createFieldAccess(Types.LUAVALUE.name, "NONE", Types.LUAVALUE.object, Const.GETSTATIC));
	}

	public void loadBoolean(boolean b) {
		String field = b? "TRUE": "FALSE";
		append(factory.createFieldAccess(Types.LUAVALUE.name, field, Types.LUABOOLEAN.object, Const.GETSTATIC));
	}

	private final Map<Integer, Integer>          plainSlotVars     = new HashMap<>();
	private final Map<Integer, Integer>          upvalueSlotVars   = new HashMap<>();
	private final Map<Integer, LocalVariableGen> localVarGenBySlot = new HashMap<>();

	private int findSlot(int slot, Map<Integer, Integer> map, String prefix, Type type) {
		Integer islot = slot;
		if (map.containsKey(islot))
			return map.get(islot).intValue();
		String name = prefix+slot;
		LocalVariableGen local = mg.addLocalVariable(name, type, null, null);
		int index = local.getIndex();
		map.put(islot, index);
		localVarGenBySlot.put(islot, local);
		return index;
	}

	private int findSlotIndex(int slot, boolean isupvalue) {
		return isupvalue? findSlot(slot, upvalueSlotVars, PREFIX_UPVALUE_SLOT, Types.LUAVALUE.array)
			: findSlot(slot, plainSlotVars, PREFIX_PLAIN_SLOT, Types.LUAVALUE.object);
	}

	public void loadLocal(int pc, int slot) {
		boolean isupval = pi.isUpvalueRefer(pc, slot);
		int index = findSlotIndex(slot, isupval);
		append(new ALOAD(index));
		if (isupval) {
			append(new PUSH(cp, 0));
			append(InstructionConst.AALOAD);
		}
	}

	public void storeLocal(int pc, int slot) {
		boolean isupval = pi.isUpvalueAssign(pc, slot);
		int index = findSlotIndex(slot, isupval);
		if (isupval) {
			boolean isupcreate = pi.isUpvalueCreate(pc, slot);
			if (isupcreate) {
				append(factory.createInvoke(classname, "newupe", Types.LUAVALUE.array, ArgTypes.NONE.types,
					Const.INVOKESTATIC));
				append(InstructionConst.DUP);
				append(new ASTORE(index));
			} else {
				append(new ALOAD(index));
			}
			append(InstructionConst.SWAP);
			append(new PUSH(cp, 0));
			append(InstructionConst.SWAP);
			append(InstructionConst.AASTORE);
		} else {
			append(new ASTORE(index));
		}
	}

	public void createUpvalues(int pc, int firstslot, int numslots) {
		for (int i = 0; i < numslots; i++) {
			int slot = firstslot+i;
			boolean isupcreate = pi.isUpvalueCreate(pc, slot);
			if (isupcreate) {
				int index = findSlotIndex(slot, true);
				append(factory.createInvoke(classname, "newupn", Types.LUAVALUE.array, ArgTypes.NONE.types,
					Const.INVOKESTATIC));
				append(new ASTORE(index));
			}
		}
	}

	public void convertToUpvalue(int pc, int slot) {
		boolean isupassign = pi.isUpvalueAssign(pc, slot);
		if (isupassign) {
			int index = findSlotIndex(slot, false);
			append(new ALOAD(index));
			append(factory.createInvoke(classname, "newupl", Types.LUAVALUE.array, ArgTypes.LUAVALUE.types, Const.INVOKESTATIC));
			int upindex = findSlotIndex(slot, true);
			append(new ASTORE(upindex));
		}
	}

	private static String upvalueName(int upindex) {
		return PREFIX_UPVALUE+upindex;
	}

	public void loadUpvalue(int upindex) {
		boolean isrw = pi.isReadWriteUpvalue(pi.upvals[upindex]);
		append(InstructionConst.THIS);
		if (isrw) {
			append(factory.createFieldAccess(classname, upvalueName(upindex), Types.LUAVALUE.array, Const.GETFIELD));
			append(new PUSH(cp, 0));
			append(InstructionConst.AALOAD);
		} else {
			append(factory.createFieldAccess(classname, upvalueName(upindex), Types.LUAVALUE.object, Const.GETFIELD));
		}
	}

	public void storeUpvalue(int pc, int upindex, int slot) {
		boolean isrw = pi.isReadWriteUpvalue(pi.upvals[upindex]);
		append(InstructionConst.THIS);
		if (isrw) {
			append(factory.createFieldAccess(classname, upvalueName(upindex), Types.LUAVALUE.array, Const.GETFIELD));
			append(new PUSH(cp, 0));
			loadLocal(pc, slot);
			append(InstructionConst.AASTORE);
		} else {
			loadLocal(pc, slot);
			append(factory.createFieldAccess(classname, upvalueName(upindex), Types.LUAVALUE.object, Const.PUTFIELD));
		}
	}

	public void newTable(int b, int c) {
		append(new PUSH(cp, b));
		append(new PUSH(cp, c));
		append(factory.createInvoke(Types.LUAVALUE.name, "tableOf", Types.LUATABLE.object, ArgTypes.INT_INT.types, Const.INVOKESTATIC));
	}

	public void loadVarargs() {
		append(new ALOAD(1));
	}

	public void loadVarargs(int argindex) {
		loadVarargs();
		arg(argindex);
	}

	public void arg(int argindex) {
		if (argindex == 1) {
			append(factory.createInvoke(Types.VARARGS.name, "arg1", Types.LUAVALUE.object, ArgTypes.NONE.types, Const.INVOKEVIRTUAL));
		} else {
			append(new PUSH(cp, argindex));
			append(factory.createInvoke(Types.VARARGS.name, "arg", Types.LUAVALUE.object, ArgTypes.INT.types, Const.INVOKEVIRTUAL));
		}
	}

	private int getVarresultIndex() {
		if (varresult == null)
			varresult = mg.addLocalVariable(NAME_VARRESULT, Types.VARARGS.object, null, null);
		return varresult.getIndex();
	}

	public void loadVarresult() {
		append(new ALOAD(getVarresultIndex()));
	}

	public void storeVarresult() {
		append(new ASTORE(getVarresultIndex()));
	}

	public void subargs(int firstarg) {
		append(new PUSH(cp, firstarg));
		append(factory.createInvoke(Types.VARARGS.name, "subargs", Types.VARARGS.object, ArgTypes.INT.types, Const.INVOKEVIRTUAL));
	}

	public void getTable() {
		append(factory.createInvoke(Types.LUAVALUE.name, "get", Types.LUAVALUE.object, ArgTypes.LUAVALUE.types, Const.INVOKEVIRTUAL));
	}

	public void setTable() {
		append(
			factory.createInvoke(Types.LUAVALUE.name, "set", Type.VOID, ArgTypes.LUAVALUE_2.types, Const.INVOKEVIRTUAL));
	}

	public void unaryop(int o) {
		String op;
		switch (o) {
		default:
		case Lua.OP_UNM:
			op = "neg";
			break;
		case Lua.OP_NOT:
			op = "not";
			break;
		case Lua.OP_LEN:
			op = "len";
			break;
		}
		append(factory.createInvoke(Types.LUAVALUE.name, op, Types.LUAVALUE.object, Type.NO_ARGS, Const.INVOKEVIRTUAL));
	}

	public void binaryop(int o) {
		String op;
		switch (o) {
		default:
		case Lua.OP_ADD:
			op = "add";
			break;
		case Lua.OP_SUB:
			op = "sub";
			break;
		case Lua.OP_MUL:
			op = "mul";
			break;
		case Lua.OP_DIV:
			op = "div";
			break;
		case Lua.OP_MOD:
			op = "mod";
			break;
		case Lua.OP_POW:
			op = "pow";
			break;
		}
		append(factory.createInvoke(Types.LUAVALUE.name, op, Types.LUAVALUE.object, ArgTypes.LUAVALUE.types, Const.INVOKEVIRTUAL));
	}

	public void compareop(int o) {
		String op;
		switch (o) {
		default:
		case Lua.OP_EQ:
			op = "eq_b";
			break;
		case Lua.OP_LT:
			op = "lt_b";
			break;
		case Lua.OP_LE:
			op = "lteq_b";
			break;
		}
		append(factory.createInvoke(Types.LUAVALUE.name, op, Type.BOOLEAN, ArgTypes.LUAVALUE.types, Const.INVOKEVIRTUAL));
	}

	public void areturn() {
		append(InstructionConst.ARETURN);
	}

	public void toBoolean() {
		append(factory.createInvoke(Types.LUAVALUE.name, "toboolean", Type.BOOLEAN, Type.NO_ARGS, Const.INVOKEVIRTUAL));
	}

	public void tostring() {
		append(factory.createInvoke(Types.BUFFER.name, "tostring", Types.LUASTRING.object, Type.NO_ARGS, Const.INVOKEVIRTUAL));
	}

	public void isNil() {
		append(factory.createInvoke(Types.LUAVALUE.name, "isnil", Type.BOOLEAN, Type.NO_ARGS, Const.INVOKEVIRTUAL));
	}

	public void testForLoop() {
		append(factory.createInvoke(Types.LUAVALUE.name, "testfor_b", Type.BOOLEAN, ArgTypes.LUAVALUE_2.types,
			Const.INVOKEVIRTUAL));
	}

	public void loadArrayArgs(int pc, int firstslot, int nargs) {
		append(new PUSH(cp, nargs));
		append(new ANEWARRAY(cp.addClass(Types.LUAVALUE.name)));
		for (int i = 0; i < nargs; i++) {
			append(InstructionConst.DUP);
			append(new PUSH(cp, i));
			loadLocal(pc, firstslot++);
			append(new AASTORE());
		}
	}

	public void newVarargs(int pc, int firstslot, int nargs) {
		switch (nargs) {
		case 0:
			loadNone();
			break;
		case 1:
			loadLocal(pc, firstslot);
			break;
		case 2:
			loadLocal(pc, firstslot);
			loadLocal(pc, firstslot+1);
			append(factory.createInvoke(Types.LUAVALUE.name, "varargsOf", Types.VARARGS.object, ArgTypes.LUAVALUE_VARARGS.types,
				Const.INVOKESTATIC));
			break;
		case 3:
			loadLocal(pc, firstslot);
			loadLocal(pc, firstslot+1);
			loadLocal(pc, firstslot+2);
			append(factory.createInvoke(Types.LUAVALUE.name, "varargsOf", Types.VARARGS.object, ArgTypes.LUAVALUE_3.types,
				Const.INVOKESTATIC));
			break;
		default:
			loadArrayArgs(pc, firstslot, nargs);
			append(factory.createInvoke(Types.LUAVALUE.name, "varargsOf", Types.VARARGS.object, ArgTypes.LUAVALUEARRAY.types,
				Const.INVOKESTATIC));
			break;
		}
	}

	public void newVarargsVarresult(int pc, int firstslot, int nslots) {
//		var a = (LuaValue::varargsOf);
//		MethodHandles.Lookup lookup = MethodHandles.lookup();
//		lookup.unreflect(a);
		loadArrayArgs(pc, firstslot, nslots);
		loadVarresult();
		append(factory.createInvoke(Types.LUAVALUE.name, "varargsOf", Types.VARARGS.object, ArgTypes.LUAVALUEARRAY_VARARGS.types,
			Const.INVOKESTATIC));
	}

	public void call(int nargs) {
		switch (nargs) {
		case 0:
			append(factory.createInvoke(Types.LUAVALUE.name, "call", Types.LUAVALUE.object, ArgTypes.NONE.types, Const.INVOKEVIRTUAL));
			break;
		case 1:
			append(factory.createInvoke(Types.LUAVALUE.name, "call", Types.LUAVALUE.object, ArgTypes.LUAVALUE.types, Const.INVOKEVIRTUAL));
			break;
		case 2:
			append(factory.createInvoke(Types.LUAVALUE.name, "call", Types.LUAVALUE.object, ArgTypes.LUAVALUE_2.types, Const.INVOKEVIRTUAL));
			break;
		case 3:
			append(factory.createInvoke(Types.LUAVALUE.name, "call", Types.LUAVALUE.object, ArgTypes.LUAVALUE_3.types, Const.INVOKEVIRTUAL));
			break;
		default:
			throw new IllegalArgumentException("can't call with " + nargs + " args");
		}
	}

	public void newTailcallVarargs() {
		append(factory.createInvoke(Types.LUAVALUE.name, "tailcallOf", Types.VARARGS.object, ArgTypes.LUAVALUE_VARARGS.types,
			Const.INVOKESTATIC));
	}

	public void invoke(int nargs) {
		switch (nargs) {
		case -1, 1:
			append(factory.createInvoke(Types.LUAVALUE.name, "invoke", Types.VARARGS.object, ArgTypes.VARARGS.types, Const.INVOKEVIRTUAL));
			break;
		case 0:
			append(factory.createInvoke(Types.LUAVALUE.name, "invoke", Types.VARARGS.object, ArgTypes.NONE.types, Const.INVOKEVIRTUAL));
			break;
		case 2:
			append(factory.createInvoke(Types.LUAVALUE.name, "invoke", Types.VARARGS.object, ArgTypes.LUAVALUE_VARARGS.types, Const.INVOKEVIRTUAL));
			break;
		case 3:
			append(factory.createInvoke(Types.LUAVALUE.name, "invoke", Types.VARARGS.object, ArgTypes.LUAVALUE_2.types, Const.INVOKEVIRTUAL));
			break;
		default:
			throw new IllegalArgumentException("can't invoke with " + nargs + " args");
		}
	}

	// ------------------------ closures ------------------------

	public void closureCreate(String protoname) {
		append(factory.createNew(new ObjectType(protoname)));
		append(InstructionConst.DUP);
		append(factory.createInvoke(protoname, "<init>", Type.VOID, Type.NO_ARGS, Const.INVOKESPECIAL));
	}

	public void closureInitUpvalueFromUpvalue(String protoname, int newup, int upindex) {
		boolean isrw = pi.isReadWriteUpvalue(pi.upvals[upindex]);
		Type uptype = isrw? Types.LUAVALUE.array : Types.LUAVALUE.object;
		String srcname = upvalueName(upindex);
		String destname = upvalueName(newup);
		append(InstructionConst.THIS);
		append(factory.createFieldAccess(classname, srcname, uptype, Const.GETFIELD));
		append(factory.createFieldAccess(protoname, destname, uptype, Const.PUTFIELD));
	}

	public void closureInitUpvalueFromLocal(String protoname, int newup, int pc, int srcslot) {
		boolean isrw = pi.isReadWriteUpvalue(pi.vars[srcslot][pc].upvalue);
		Type uptype = isrw? Types.LUAVALUE.array : Types.LUAVALUE.object;
		String destname = upvalueName(newup);
		int index = findSlotIndex(srcslot, isrw);
		append(new ALOAD(index));
		append(factory.createFieldAccess(protoname, destname, uptype, Const.PUTFIELD));
	}

	private final Map<LuaValue, String> constants = new HashMap<>();

	public void loadConstant(LuaValue value) {
		switch (value.type()) {
		case LuaValue.TNIL:
			loadNil();
			break;
		case LuaValue.TBOOLEAN:
			loadBoolean(value.toboolean());
			break;
		case LuaValue.TNUMBER:
		case LuaValue.TSTRING:
			String name = constants.get(value);
			if (name == null) {
				name = value.type() == LuaValue.TNUMBER? value.isinttype()? createLuaIntegerField(value.checkint())
					: createLuaDoubleField(value.checkdouble()): createLuaStringField(value.checkstring());
				constants.put(value, name);
			}
			append(factory.createGetStatic(classname, name, Types.LUAVALUE.object));
			break;
		default:
			throw new IllegalArgumentException("bad constant type: " + value.type());
		}
	}

	private String createLuaIntegerField(int value) {
		String name = PREFIX_CONSTANT+constants.size();
		FieldGen fg = new FieldGen(Const.ACC_STATIC | Const.ACC_FINAL, Types.LUAVALUE.object, name, cp);
		cg.addField(fg.getField());
		init.append(new PUSH(cp, value));
		init.append(factory.createInvoke(Types.LUAVALUE.name, "valueOf", Types.LUAINTEGER.object, ArgTypes.INT.types, Const.INVOKESTATIC));
		init.append(factory.createPutStatic(classname, name, Types.LUAVALUE.object));
		return name;
	}

	private String createLuaDoubleField(double value) {
		String name = PREFIX_CONSTANT+constants.size();
		FieldGen fg = new FieldGen(Const.ACC_STATIC | Const.ACC_FINAL, Types.LUAVALUE.object, name, cp);
		cg.addField(fg.getField());
		init.append(new PUSH(cp, value));
		init.append(factory.createInvoke(Types.LUAVALUE.name, "valueOf", Types.LUANUMBER.object, ArgTypes.DOUBLE.types, Const.INVOKESTATIC));
		init.append(factory.createPutStatic(classname, name, Types.LUAVALUE.object));
		return name;
	}

	private String createLuaStringField(LuaString value) {
		String name = PREFIX_CONSTANT+constants.size();
		FieldGen fg = new FieldGen(Const.ACC_STATIC | Const.ACC_FINAL, Types.LUAVALUE.object, name, cp);
		cg.addField(fg.getField());
		LuaString ls = value.checkstring();
		if (ls.isValidUtf8()) {
			init.append(new PUSH(cp, value.tojstring()));
			init.append(factory.createInvoke(Types.LUASTRING.name, "valueOf", Types.LUASTRING.object, ArgTypes.STRING.types, Const.INVOKESTATIC));
		} else {
			char[] c = new char[ls.m_length];
			for (int j = 0; j < ls.m_length; j++)
				c[j] = (char) (0xff & ls.m_bytes[ls.m_offset+j]);
			init.append(new PUSH(cp, new String(c)));
			init.append(factory.createInvoke(Types.STRING.name, "toCharArray", Types.CHAR.array, Type.NO_ARGS, Const.INVOKEVIRTUAL));
			init.append(factory.createInvoke(Types.LUASTRING.name, "valueOf", Types.LUASTRING.object, ArgTypes.CHARARRAY.types, Const.INVOKESTATIC));
		}
		init.append(factory.createPutStatic(classname, name, Types.LUAVALUE.object));
		return name;
	}

	// --------------------- branching support -------------------------
	public static final int BRANCH_GOTO = 1;
	public static final int BRANCH_IFNE = 2;
	public static final int BRANCH_IFEQ = 3;

	public void addBranch(int pc, int branchType, int targetpc) {
		switch (branchType) {
		default:
		case BRANCH_GOTO:
			branches[pc] = new GOTO(null);
			break;
		case BRANCH_IFNE:
			branches[pc] = new IFNE(null);
			break;
		case BRANCH_IFEQ:
			branches[pc] = new IFEQ(null);
			break;
		}
		targets[pc] = targetpc;
		append(branches[pc]);
	}

	private void append(Instruction i) {
		conditionalSetBeginningOfLua(main.append(i));
	}

	private void append(CompoundInstruction i) {
		conditionalSetBeginningOfLua(main.append(i));
	}

	private void append(BranchInstruction i) {
		conditionalSetBeginningOfLua(main.append(i));
	}

	private void conditionalSetBeginningOfLua(InstructionHandle ih) {
		if (beginningOfLuaInstruction == null)
			beginningOfLuaInstruction = ih;
	}

	public void onEndOfLuaInstruction(int pc, int line) {
		branchDestHandles[pc] = beginningOfLuaInstruction;
		lastInstrHandles[pc] = main.getEnd();
		if (line != prev_line)
			mg.addLineNumber(beginningOfLuaInstruction, prev_line = line);
		beginningOfLuaInstruction = null;
	}

	public void setVarStartEnd(int slot, int start_pc, int end_pc, String name) {
		Integer islot = slot;
		if (localVarGenBySlot.containsKey(islot)) {
			name = name.replaceAll("[^a-zA-Z0-9]", "_");
			LocalVariableGen l = localVarGenBySlot.get(islot);
			l.setEnd(lastInstrHandles[end_pc-1]);
			if (start_pc > 1)
				l.setStart(lastInstrHandles[start_pc-2]);
			l.setName(name);
		}
	}

	private void resolveBranches() {
		int nc = p.code.length;
		for (int pc = 0; pc < nc; pc++) {
			if (branches[pc] != null) {
				int t = targets[pc];
				while ( t < branchDestHandles.length && branchDestHandles[t] == null )
					t++;
				if (t >= branchDestHandles.length)
					throw new IllegalArgumentException(
						"no target at or after " + targets[pc] + " op=" + Lua.GET_OPCODE(p.code[targets[pc]]));
				branches[pc].setTarget(branchDestHandles[t]);
			}
		}
	}

	public void setlistStack(int pc, int a0, int index0, int nvals) {
		for (int i = 0; i < nvals; i++) {
			dup();
			append(new PUSH(cp, index0+i));
			loadLocal(pc, a0+i);
			append(factory.createInvoke(Types.LUAVALUE.name, "rawset", Type.VOID, ArgTypes.INT_LUAVALUE.types,
				Const.INVOKEVIRTUAL));
		}
	}

	public void setlistVarargs(int index0, int vresultbase) {
		append(new PUSH(cp, index0));
		loadVarresult();
		append(factory.createInvoke(Types.LUAVALUE.name, "rawsetlist", Type.VOID, ArgTypes.INT_VARARGS.types,
			Const.INVOKEVIRTUAL));
	}

	public void concatvalue() {
		append(
			factory.createInvoke(Types.LUAVALUE.name, "concat", Types.LUAVALUE.object, ArgTypes.LUAVALUE.types, Const.INVOKEVIRTUAL));
	}

	public void concatbuffer() {
		append(factory.createInvoke(Types.LUAVALUE.name, "concat", Types.BUFFER.object, ArgTypes.BUFFER.types, Const.INVOKEVIRTUAL));
	}

	public void tobuffer() {
		append(factory.createInvoke(Types.LUAVALUE.name, "buffer", Types.BUFFER.object, Type.NO_ARGS, Const.INVOKEVIRTUAL));
	}

	public void tovalue() {
		append(factory.createInvoke(Types.BUFFER.name, "value", Types.LUAVALUE.object, Type.NO_ARGS, Const.INVOKEVIRTUAL));
	}

	public void closeUpvalue(int pc, int upindex) {
		// TODO: assign the upvalue location the value null;
		/*
		boolean isrw = pi.isReadWriteUpvalue( pi.upvals[upindex] );
		append(InstructionConst.THIS);
		append(InstructionConst.ACONST_NULL);
		if ( isrw ) {
			append(factory.createFieldAccess(classname, upvalueName(upindex), Types.LUAVALUE.array, Const.PUTFIELD));
		} else {
			append(factory.createFieldAccess(classname, upvalueName(upindex), Types.LUAVALUE.object, Const.PUTFIELD));
		}
		//*/
	}
}
