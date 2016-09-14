-module(addressBook).
-export([createAddressBook/0]).
-export([addContact/3]).
-export([addEmail/4]).
-export([addPhone/4]).
-export([removeContact/3]).
-export([removeEmail/2]).
-export([removePhone/2]).
-export([getEmails/3]).
-export([getPhones/3]).
-export([findByEmail/2]).
-export([findByPhone/2]).
-export([addAddress/5]).
-export([removeAddress/3]).
-export([getAddresses/3]).
-record(address, {type, value = ""}).
-record(contact, {name = "", surname = "", phone = [], email = [], address = []}).


createAddressBook() -> [].


addContact_(Name, Surname, [], Book) ->
	[#contact{name = Name, surname = Surname} | Book];
addContact_(Name, Surname, [#contact{name = Name, surname = Surname} | _], _) ->
	{error, "Name and surname pair already exist"};
addContact_(Name, Surname, [_ | T], Book) ->
	addContact_(Name, Surname, T, Book).

addContact(Name, Surname, Book) ->
	addContact_(Name, Surname, Book, Book).


removeContact_(_, _, [], _) ->
	{error, "Name and surname pair does't exist"};
removeContact_(Name, Surname, [#contact{name = Name, surname = Surname} | T], Remainder) ->
	Remainder ++ T;
removeContact_(Name, Surname, [H | T], Remainder) ->
	removeContact_(Name, Surname, T, Remainder ++ [H]).

removeContact(Name, Surname, Book) ->
	removeContact_(Name, Surname, Book, []).


isEmailUnique(_, []) ->
	true;
isEmailUnique(Email, [Contact | T]) ->
	case lists:any(fun(X) -> X == Email end, Contact#contact.email) of
		true -> false;
		_ -> isEmailUnique(Email, T)
	end.


addEmail_(Name, Surname, Email, [], Book) ->
	case isEmailUnique(Email, Book) of
		false -> {error, "Email already exists somewhere"};
		_ -> [#contact{name = Name, surname = Surname, email = [Email]} | Book]
	end;
addEmail_(Name, Surname, Email, [Contact = #contact{name = Name, surname = Surname} | T], Remainder) ->
	case isEmailUnique(Email, T ++ [Contact] ++ Remainder) of
		false -> {error, "Email already exists somewhere"};
		_ -> Remainder ++ [Contact#contact{email = [Email | Contact#contact.email]} | T]
	end;
addEmail_(Name, Surname, Email, [H | T], Remainder) ->
	addEmail_(Name, Surname, Email, T, Remainder ++ [H]).

addEmail(Name, Surname, Email, Book) ->
	addEmail_(Name, Surname, Email, Book, []).


removeEmail_(_, [], _) ->
	{error, "Email doesn't exist"};
removeEmail_(Email, [Contact | T], Remainder) ->
	case lists:any(fun(X) -> X == Email end, Contact#contact.email) of
		true ->	Remainder ++ [Contact#contact{email = lists:filter(fun(X) -> X /= Email end, Contact#contact.email)}] ++ T;
		_ -> removeEmail_(Email, T, Remainder ++ [Contact])
	end.
removeEmail(Email, Book) ->
	removeEmail_(Email, Book, []).


getEmails(_, _, []) ->
	{error, "Name and surname pair doesn't exist"};
getEmails(Name, Surname, [Contact = #contact{name = Name, surname = Surname} | _]) ->
	Contact#contact.email;
getEmails(Name, Surname, [_ | T]) ->
	getEmails(Name, Surname, T).


findByEmail(_, []) ->
	{error, "Noone found with that email"};
findByEmail(Email, [Contact | T]) ->
	case lists:any(fun(X) -> X == Email end, Contact#contact.email) of
		true -> {Contact#contact.name, Contact#contact.surname};
		_ -> findByEmail(Email, T)
	end.


isPhoneUnique(_, []) ->
	true;
isPhoneUnique(Phone, [Contact | T]) ->
	case lists:any(fun(X) -> X == Phone end, Contact#contact.phone) of
		true -> false;
		_ -> isPhoneUnique(Phone, T)
	end.


addPhone_(Name, Surname, Phone, [], Book) ->
	case isPhoneUnique(Phone, Book) of
		false -> {error, "Phone already exists somewhere"};
		_ -> [#contact{name = Name, surname = Surname, phone = [Phone]} | Book]
	end;
addPhone_(Name, Surname, Phone, [Contact = #contact{name = Name, surname = Surname} | T], Remainder) ->
	case isPhoneUnique(Phone, T ++ [Contact] ++ Remainder) of
		false -> {error, "Phone already exists somewhere"};
		_ -> Remainder ++ [Contact#contact{phone = [Phone | Contact#contact.phone]} | T]
	end;
addPhone_(Name, Surname, Phone, [H | T], Remainder) ->
	addPhone_(Name, Surname, Phone, T, Remainder ++ [H]).

addPhone(Name, Surname, Phone, Book) ->
	addPhone_(Name, Surname, Phone, Book, []).


removePhone_(_, [], _) ->
	{error, "Phone doesn't exist"};
removePhone_(Phone, [Contact | T], Remainder) ->
	case lists:any(fun(X) -> X == Phone end, Contact#contact.phone) of
		true ->	Remainder ++ [Contact#contact{phone = lists:filter(fun(X) -> X /= Phone end, Contact#contact.phone)}] ++ T;
		_ -> removePhone_(Phone, T, Remainder ++ [Contact])
	end.
removePhone(Phone, Book) ->
	removePhone_(Phone, Book, []).


getPhones(_, _, []) ->
	{error, "Name and surname pair doesn't exist"};
getPhones(Name, Surname, [Contact = #contact{name = Name, surname = Surname} | _]) ->
	Contact#contact.phone;
getPhones(Name, Surname, [_ | T]) ->
	getPhones(Name, Surname, T).


findByPhone(_, []) ->
	{error, "Noone found with that phone"};
findByPhone(Phone, [Contact | T]) ->
	case lists:any(fun(X) -> X == Phone end, Contact#contact.phone) of
		true -> {Contact#contact.name, Contact#contact.surname};
		_ -> findByPhone(Phone, T)
	end.


isAddressUnique(_, _, []) ->
	true;
isAddressUnique(Type, Value, [Contact | T]) ->
	case lists:any(fun(X) -> ((X#address.type == Type) and (X#address.value == Value)) end, Contact#contact.address) of
		true -> false;
		_ -> isAddressUnique(Type, Value, T)
	end.


addAddress_(Name, Surname, Type, Value, [], Book) ->
	case isAddressUnique(Type, Value, Book) of
		false -> {error, "Address already exists somewhere"};
		_ -> [#contact{name = Name, surname = Surname, address = [#address{type = Type, value = Value}]} | Book]
	end;
addAddress_(Name, Surname, Type, Value, [Contact = #contact{name = Name, surname = Surname} | T], Remainder) ->
	case isAddressUnique(Type, Value, T ++ [Contact] ++ Remainder) of
		false -> {error, "Address already exists somewhere"};
		_ -> Remainder ++ [Contact#contact{address = [#address{type = Type, value = Value} | Contact#contact.address]} | T]
	end;
addAddress_(Name, Surname, Type, Value, [H | T], Remainder) ->
	addAddress_(Name, Surname, Type, Value, T, Remainder ++ [H]).

addAddress(Name, Surname, Type, Value, Book) ->
	addAddress_(Name, Surname, Type, Value, Book, []).


removeAddress_(_, _, [], _) ->
	{error, "Address doesn't exist"};
removeAddress_(Type, Value, [Contact | T], Remainder) ->
	case lists:any(fun(X) -> ((X#address.type == Type) and (X#address.value == Value)) end, Contact#contact.address) of
		true ->	Remainder ++ [Contact#contact{address = lists:filter(fun(X) -> not ((X#address.type == Type) and (X#address.value == Value)) end, Contact#contact.address)}] ++ T;
		_ -> removeAddress_(Type, Value, T, Remainder ++ [Contact])
	end.
removeAddress(Type, Value, Book) ->
	removeAddress_(Type, Value, Book, []).


getAddresses(_, _, []) ->
	{error, "Name and surname pair doesn't exist"};
getAddresses(Name, Surname, [Contact = #contact{name = Name, surname = Surname} | _]) ->
	Contact#contact.address;
getAddresses(Name, Surname, [_ | T]) ->
	getAddresses(Name, Surname, T).

