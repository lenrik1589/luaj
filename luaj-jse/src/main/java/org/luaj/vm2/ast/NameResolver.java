package org.luaj.vm2.ast;

import org.luaj.vm2.LuaValue;
import org.luaj.vm2.ast.Exp.Constant;
import org.luaj.vm2.ast.Exp.NameExp;
import org.luaj.vm2.ast.Exp.VarExp;
import org.luaj.vm2.ast.Stat.*;

import java.util.List;

/**
 * Visitor that resolves names to scopes. Each Name is resolved to a
 * NamedVarible, possibly in a NameScope if it is a local, or in no named scope
 * if it is a global.
 */
public class NameResolver extends Visitor {

	private NameScope scope = null;

	private void pushScope() {
		scope = new NameScope(scope);
	}

	private void popScope() {
		scope = scope.outerScope;
	}

	@Override
	public void visit(NameScope scope) {
	}

	@Override
	public void visit(Block block) {
		pushScope();
		block.scope = scope;
		super.visit(block);
		popScope();
	}

	@Override
	public void visit(FuncBody body) {
		pushScope();
		scope.functionNestingCount++;
		body.scope = scope;
		super.visit(body);
		popScope();
	}

	@Override
	public void visit(LocalFuncDef stat) {
		defineLocalVar(stat.name);
		super.visit(stat);
	}

	@Override
	public void visit(NumericFor stat) {
		pushScope();
		stat.scope = scope;
		defineLocalVar(stat.name);
		super.visit(stat);
		popScope();
	}

	@Override
	public void visit(GenericFor stat) {
		pushScope();
		stat.scope = scope;
		defineLocalVars(stat.names);
		super.visit(stat);
		popScope();
	}

	@Override
	public void visit(NameExp exp) {
		exp.name.variable = resolveNameReference(exp.name);
		super.visit(exp);
	}

	@Override
	public void visit(FuncDef stat) {
		stat.name.name.variable = resolveNameReference(stat.name.name);
		stat.name.name.variable.hasassignments = true;
		super.visit(stat);
	}

	@Override
	public void visit(Assign stat) {
		super.visit(stat);
		for (VarExp element : stat.vars) {
			VarExp v = element;
			v.markHasAssignment();
		}
	}

	@Override
	public void visit(LocalAssign stat) {
		visitExps(stat.values);
		defineLocalVars(stat.names);
		int n = stat.names.size();
		int m = stat.values != null? stat.values.size(): 0;
		boolean isvarlist = m > 0 && m < n && stat.values.get(m-1).isvarargexp();
		for (int i = 0; i < n && i < (isvarlist? m-1: m); i++)
			if (stat.values.get(i) instanceof Constant)
				stat.names.get(i).variable.initialValue = ((Constant) stat.values.get(i)).value;
		if (!isvarlist)
			for (int i = m; i < n; i++)
				stat.names.get(i).variable.initialValue = LuaValue.NIL;
	}

	@Override
	public void visit(ParList pars) {
		if (pars.names != null)
			defineLocalVars(pars.names);
		if (pars.isvararg)
			scope.define("arg");
		super.visit(pars);
	}

	protected void defineLocalVars(List<Name> names) {
		for (Name name : names)
			defineLocalVar(name);
	}

	protected void defineLocalVar(Name name) {
		name.variable = scope.define(name.name);
	}

	protected Variable resolveNameReference(Name name) {
		Variable v = scope.find(name.name);
		if (v.isLocal() && scope.functionNestingCount != v.definingScope.functionNestingCount)
			v.isupvalue = true;
		return v;
	}
}
