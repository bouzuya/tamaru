# tamaru

## How to run

```
$ cp _env .env
$ docker build --tag bouzuya/tamaru .
$ docker run --publish 8080:3000 --env-file .env bouzuya/tamaru
$ curl http://localhost:8080/
```

## Note

- Google Sheets API v4
  - https://developers.google.com/sheets/api/reference/rest/
  - https://developers.google.com/apis-explorer/#p/sheets/v4/
- Halogen 3.1.3
- PureScript v0.11.7
