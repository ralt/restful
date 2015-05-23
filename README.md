# restful

Spin up new REST entities like madman

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
