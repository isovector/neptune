export V2, Color, rgb, rgba, enum

class V2
  new: (@x, @y) =>

  __tostring: =>
    "V2(#{@x}, #{@y})"

  __add: (other) =>
    V2(@x + other.x, @y + other.y)

  __sub: (other) =>
    V2(@x - other.x, @y - other.y)


class Color
  new: (@r, @g, @b, @a=1) =>

  __tostring: =>
    "rgba(#{@r}, #{@g}, #{@b}, #{@a})"

enum = (values) ->
  setmetatable {v,v for v in *values}, __index: (k) =>
    error "don't know `#{k}` for enum"

rgba = Color
rgb = Color
