module Vdom = struct
  module Node = struct
    type t = string
  end
end

module Effect = struct
  type 'a t = unit
end
