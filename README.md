# restful

REST APIs made easy.

In other words, spin up a fully standards-compliant<sup>1</sup> API over HTTP in
Common Lisp by providing a simple class.

Documentation available [here][0].

An example of an implementation available [here][4].

<sup>1</sup> Closely follows [RFC 7230][1], [RFC 7231][2] and [RFC 5789][3].

## Roadmap

- Write extensive documentation
- Define a postgresql storage

## Misc

Generate documentation using https://github.com/Ralt/documentation-template:

```lisp
(ql:quickload :restful)
(ql:quickload :documentation-template)
(ql:quickload :alexandria
(documentation-template:create-template
  :restful
  :target "api.html"
  :subtitle "REST APIs made easy"
  :example (alexandria:read-file-into-string "example.html"))
```


  [0]: http://rawgit.com/Ralt/restful/master/api.html
  [1]: https://tools.ietf.org/html/rfc7230
  [2]: https://tools.ietf.org/html/rfc7231
  [3]: https://tools.ietf.org/html/rfc5789
  [4]: https://github.com/Ralt/restful-blog
