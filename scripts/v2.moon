export V2

class V2
  new: (@x, @y) =>

  __tostring: =>
    "V2(#{@x}, #{@y})"

  __add: (other) =>
    V2(@x + other.x, @y + other.y)

  __sub: (other) =>
    V2(@x - other.x, @y - other.y)

