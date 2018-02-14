export V2, Color, rgb, rgba

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

rgba = Color
rgb = Color
