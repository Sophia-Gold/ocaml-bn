externals:
	cargo build
	dune build src/printmod.exe

test: externals
	dune runtest

quicktest: externals
	dune build @install @test/runtest_quick

clean:
	cargo clean
	dune clean
