include struct
  open Cohttp_async
  module Body = Body
  module Request = Cohttp.Request
  module Response = Cohttp.Response
  module Server = Server
end

include struct
  open Cohttp
  module Code = Code
end
