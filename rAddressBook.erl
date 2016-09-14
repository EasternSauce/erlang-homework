-module(rAddressBook).
-export([start/0, stop/0, addContact/2, loop/1, addEmail/3]).


start() ->
	Book = addressBook:createAddressBook(),
	register(addrBook, spawn_link(?MODULE, loop, [Book])).

stop() -> addrBook ! {stop,self()}.

addContact(Name, Surname) ->
	addrBook ! {Name, Surname}.

addEmail(Name, Surname, Email) ->
	addrBook ! {Name, Surname, Email}.

loop(Book) ->
	%io:format("Wywolany raz jeszcze ~B ~n",[length(Book)]),
	receive
		{stop,PID} -> PID ! stopping;
		{Name, Surname} ->
			loop(addressBook:addContact(Name, Surname, Book));
		{Name, Surname, Email} ->
			loop(addressBook:addEmail(Name, Surname, Email, Book))

	end.


