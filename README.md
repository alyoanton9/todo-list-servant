# TODO-list by Servant

## Intro

Here you may find **TODO-list backend application** implemented with [**Servant**](https://docs.servant.dev/en/stable/), Haskell library for web development.

It has basic **CRUD** operations, responds with **JSON** only, uses [**Persistent**](https://hackage.haskell.org/package/persistent) as a storage interface, and additionally implements a simple custom **logger middleware**.

The main goal of this project is to provide a basic example of implementing backend with Servant library, so to make life a bit easier for (future) Haskell web developers.

This code complements the talk "How to Choose a Haskell Web Framework" I presented at [Haskell eXchange 2022](https://skillsmatter.com/skillscasts/18103-how-to-choose-a-haskell-web-framework).

[<img src="./assets/pictures/haskell-exchange-wide-banner.png" width="710"/>](image.png)

[<img src="./assets/pictures/haskell-exchange-talk-banner.png" width="710"/>](image.png)

In this talk, I show how some of Haskell web tools approach web development. In particular, there are 3 of them — Servant, Yesod and IHP.

**There are also corresponding [Yesod](https://github.com/alyoanton9/todo-list-yesod) and [IHP](https://github.com/alyoanton9/todo-list-ihp) TODO-list implementations.**

**Check out the [presentation slides](https://drive.google.com/file/d/17K6s0V3C2qL_dy82EzGH29vmmRqBjcUj/view?usp=sharing) and the [talk recording](https://drive.google.com/file/d/1phbv3ghdMDjGTvOCU5Xw3St2Zzv2ZBNt/view?usp=sharing), and [contact me](https://www.linkedin.com/in/alyoanton/) if you have any questions** 🙂 


## API

As application has only basic CRUD operations, here is the API

- `GET /api/task` — get all tasks
- `POST /api/task` — create new task
- `GET /api/task/{id}` — get existing task by `id`
- `PUT /api/task/{id}` — update existing task by `id`
- `DELETE /api/task/{id}` — delete existing task by `id`

### Request & response examples

We use [curl](https://curl.se/) to demonstrate interaction with API

```sh
> curl -X GET http://localhost:4000/api/task
[]

> curl -X POST http://localhost:4000/api/task -H "Content-Type: application/json" -d '{"content": "wake up"}'
{"content":"wake up","id":1}

> curl -X POST http://localhost:4000/api/task -H "Content-Type: application/json" -d '{"content": "drink coffee"}'
{"content":"drink coffee","id":2}

> curl -X PUT http://localhost:4000/api/task/2 -H "Content-Type: application/json" -d '{"content": "drink mooore coffee"}'
{"content":"drink mooore coffee","id":2}

> curl -X GET http://localhost:4000/api/task
[{"content":"wake up","id":1},{"content":"drink mooore coffee","id":2}]

> curl -X GET http://localhost:4000/api/task/2
{"content":"drink mooore coffee","id":2}

> curl -X GET http://localhost:4000/api/task/5


> curl -X DELETE http://localhost:4000/api/task/1
{"content":"wake up","id":1}

> curl -X GET http://localhost:4000/api/task
[{"content":"drink mooore coffee","id":2}]
```

## Custom logger middleware

Simple logger here just extracts request’s URL path and logs it with "Info" verbosity level.

For example,

```shell
[Info] url-path=api/task/5
[Info] url-path=api/task/
[Info] url-path=api/task
[Info] url-path=api/task/8/
[Info] url-path=api/task/3
[Info] url-path=/
[Info] url-path=api/task
```

## Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/)
- [PostgreSQL](https://www.postgresql.org/download/)

## Run

Firstly, you need to configure a connection string to connect to your database. You may refer to the [PostgreSQL docs](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING) for this.

In this example, the [following connection string](https://github.com/alyoanton9/todo-list-servant/blob/master/src/Config.hs#L22) is used

```hs
connectionStr = "host=localhost dbname=todolist-servant user=postgres password=postgres port=5432"
```

Which means that the local `todolist-servant` postgres database is accessed via `5432` port.

Make sure that user and database exist before running the app.

Then, run the app
```shell
> stack run
Migrating: CREATe TABLE "task"("id" SERIAL8  PRIMARY KEY UNIQUE,"content" VARCHAR NOT NULL)
[Debug#SQL] CREATe TABLE "task"("id" SERIAL8  PRIMARY KEY UNIQUE,"content" VARCHAR NOT NULL); []
...
```

---
Hope you'll find it helpful :blue_heart:
