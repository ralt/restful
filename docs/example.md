An example of restful's usage can be found here:
https://github.com/Ralt/restful-blog

Here is a gist of it:

    (defclass article (restful:resource)
      ((slug :is-identifier t)
       (title :required t)
       (content :default ""))
      (:metaclass restful:resource-metaclass))

    (defclass comment (restful:resource)
      ((id :is-identifier t)
       (commenter :required t)
       (content :required t))
      (:metaclass restful:resource-metaclass))

    (hunchentoot:start
     (make-instance 'restful:acceptor
                    :port 4444
                    :resource-definition {
                      "article" {
                        :class 'article
                        :collection 'restful:collection
                        :storage (make-instance 'restful:memory-storage)
                        :children {
                          "comment" {
                            :class 'comment
                            :collection 'restful:collection
                            :storage (make-instance 'restful:memory-storage)
                          }
                        }
                      }
                    }))

Here is an example of a couple of endpoints:

    CL-USER> (drakma:http-request "http://localhost:4444/article"
                                  :accept "application/json")
    "[]"
    200

    CL-USER> (drakma:http-request "http://localhost:4444/article/foo"
                                  :accept "application/json"
                                  :method :put
                                  :content "{\"slug\":\"foo\",\"title\":\"some article\"}")
    "Created"
    201

    CL-USER> (drakma:http-request "http://localhost:4444/article/foo"
                                  :accept "application/json"
                                  :method :put
                                  :content "{\"slug\":\"foo\",\"title\":\"some article\"}")
    "No Content"
    204

    CL-USER> (drakma:http-request "http://localhost:4444/article/foo"
                                  :accept "application/json")
    "{\"slug\":\"foo\",\"title\":\"some article\",\"content\":\"\"}"
    200

    CL-USER> (drakma:http-request "http://localhost:4444/article/foo/comment"
                                  :accept "application/json")
    "[]"
    200

    CL-USER> (drakma:http-request "http://localhost:4444/article/foo/comment/bar"
                                  :accept "application/json"
                                  :method :put
                                  :content "{\"id\":\"bar\",\"commenter\":\"foobar\",\"content\":\"test comment, pls ignore\"}")
    "Created"
    201

    CL-USER> (drakma:http-request "http://localhost:4444/article/foo/comment/baz"
                                  :accept "application/json"
                                  :method :put
                                  :content "{\"id\":\"baz\",\"commenter\":\"foobar\",\"content\":\"test comment, pls ignore\"}")
    "Created"
    201

    CL-USER> (drakma:http-request "http://localhost:4444/article/foo/comment"
                                  :accept "application/json")
    "[{\"id\":\"baz\",\"commenter\":\"foobar\",\"content\":\"test comment, pls ignore\"},{\"id\":\"bar\",\"commenter\":\"foobar\",\"content\":\"test comment, pls ignore\"}]"
    200
