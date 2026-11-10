# print shows alert then underlying object, returns invisibly

    Code
      print(resp)
    Message
      > The query you tried was unsuccessful. See details below.
    Output
      <httr2_response>
      GET https://example.com
      Status: 404 Not Found
      Body: None

---

    Code
      print(cnd)
    Message
      > The query you tried was unsuccessful. See details below.
    Output
      <simpleError: boom>

