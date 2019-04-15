make: 
	ERL_LIBS=deps erlc -o ebin src/*

run:
	erl -pa ./deps/*/ebin ./ebin
