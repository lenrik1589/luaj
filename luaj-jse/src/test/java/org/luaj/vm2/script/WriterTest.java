package org.luaj.vm2.script;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.script.*;
import java.io.CharArrayWriter;

import static org.junit.jupiter.api.Assertions.assertEquals;

class WriterTest {
	protected ScriptEngine e;
	protected Bindings     b;

	@BeforeEach
	public void setUp() {
		this.e = new ScriptEngineManager().getEngineByName("luaj");
		this.b = e.getBindings(ScriptContext.ENGINE_SCOPE);
	}

	@Test
	void testWriter() throws ScriptException {
		CharArrayWriter output = new CharArrayWriter();
		CharArrayWriter errors = new CharArrayWriter();
		e.getContext().setWriter(output);
		e.getContext().setErrorWriter(errors);
		e.eval("io.write( [[line]] )");
		assertEquals("line", output.toString());
		e.eval("io.write( [[ one\nline two\n]] )");
		assertEquals("line one\nline two\n", output.toString());
		output.reset();
	}
}
