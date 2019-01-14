-module(github_db).
-import(typecast, [a2b/1]).
-export([handle_push/1]).

handle_push(Data) ->
	RepoId = write_repo(Data),
	UserId = write_user(Data),
	write_commit(RepoId, Data),
	db_user_repo:create(UserId, RepoId, commit).

write_repo(Data) ->
	Name = path([repository, full_name], Data),
	Id = path([repository, id], Data),
	Url = path([repository, url], Data),
	db_repo:create(Id, Name, Url),
	Id.

write_commit(RepoId, Data) ->
	Sha = path(['after'], Data),
	Branch = path([ref], Data),
	db_commit:create(RepoId, Sha, Branch).

write_user(Data) ->
	Id = path([sender, id], Data),
	Login = path([sender, login], Data),
	Email = path([pusher, email], Data),
	db_user:create(Id, Login, Email),
	Id.

path(_, undefined) -> undefined;
path([], M) -> M;
path([K|Rest], M) -> path(Rest, maps:get(a2b(K), M, undefined)).
