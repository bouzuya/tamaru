# tamaru

tamaru

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

## License

[MIT](LICENSE)

`src/Halogen/*` is Apache-2.0. See [slamdata/purescript-halogen-vdom-string-renderer][].

[slamdata/purescript-halogen-vdom-string-renderer]: https://github.com/slamdata/purescript-halogen-vdom-string-renderer

## Author

[bouzuya][user] &lt;[m@bouzuya.net][email]&gt; ([https://bouzuya.net/][url])

[user]: https://github.com/bouzuya
[email]: mailto:m@bouzuya.net
[url]: https://bouzuya.net/
