# aircraft-service

I used https://github.com/parsonsmatt/servant-persistent as the template for
this project.

May also use ideas presented here:
http://hbtvl.banquise.net/posts/2015-06-28-servantintegration.html

[Servant](https://haskell-servant.github.io/) is an awesome Haskell library for writing web APIs. It uses the type system in a way that can only be described as magic to generate type safe routes as well as clients.

[Persistent](http://www.yesodweb.com/book/persistent) is another awesome Haskell library for querying databases. It manages migrations, your schema, and querying to make data transactions mostly painless.


## Requirements:

### Haskell

You can use [stack](https://github.com/commercialhaskell/stack) to get started:

1. `stack build`
2. `stack exec perservant`

Alternatively, cabal can be used:

1. `cabal sandbox init`
2. `cabal install --dependencies-only && cabal configure && cabal build`
3. `cabal run`

### Database:

You will need PostgreSQL installed and listening on port 5432. The default configuration uses a database name `perservant` with username/password test:test.

These steps work on Ubuntu:

```haskell
$ apt install postgres libpq-dev
$ sudo -u postgres createuser -se test
$ sudo -u postgres psql -c "alter role test with password 'test'"
$ sudo -u postgres psql -c "create database airport_service"
```

These following steps worked on Arch Linux:

```
# install postgres
$ sudo pacman -S postgres

# The installation process should have created the postgres system user for us.
# Become that user in order to initialize the DB.  This is required before
# running the postgres service.
$ sudo -i -u postgres

# As the postgres user, initialize the database.
[postgres]$ initdb --locale en_US.UTF-8 -E UTF8 -D '/var/lib/postgres/data'
# Exit to go back to your normal user.
[postgres]$ exit

# As your normal user start the postgres service.
$ sudo systemctl start postgres.service

# When that starts successfully, then we need to become the postgres system
# user again to create the "test" user and perservant database.
$ sudo -i -u postgres
[postgres]$ createuser --interactive
Enter name of role to add: test
Shall the new role be a superuser? (y/n) y
[postgres]$ createdb perservant -U test
# Exit to go back to your normal user.
[postgres]$ exit

# As your normal user you can log in and play around with the DB:
$ psql -d perservant -U test
psql (9.4.4)
Type "help" for help.

perservant=#
```

## The API:

- GET `/aircrafts` returns a list of all aircrafts in the database
- GET `/aircrafts/:name` returns the first aircraft whose name is `:name`, and returns 404 if the aircraft doesn't show up.
- POST `/aircrafts` with JSON like `{ "name": "String", "email": "String" }` to create a Aircraft.

### Playing with the API from the command line

Once the compiled `perservant` binary is running, you can use `curl` like below to play with the API from the command line.

```
# create a new aircraft
$ curl --verbose --request POST --header "Content-Type: application/json" \
    --data '{"serialNumber": "SN1", "numEngines": 4}' \
	http://localhost:8081/aircrafts

# get all aircrafts in database
$ curl --verbose --request GET --header "Content-Type: application/json" \
	http://localhost:8081/aircrafts

# get certain aircraft in database
$ curl --verbose --request GET --header "Content-Type: application/json" \
	http://localhost:8081/aircrafts/SN1
```

## src/Main.hs

`main` starts off by pulling some settings from the environment, creating a connection pool, running the migrations, and finally running the app.

## src/Api.hs

This source contains the actual API definition.

## src/Config.hs

Contains the `runDb`, `makePool`, and `Config` definitions.

## src/Models.hs

Contains fairly typical Persistent schema definitions.
