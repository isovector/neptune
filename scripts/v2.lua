do
  local _class_0
  local _base_0 = {
    __tostring = function(self)
      return "V2(" .. tostring(self.x) .. ", " .. tostring(self.y) .. ")"
    end,
    __add = function(self, other)
      return V2(self.x + other.x, self.y + other.y)
    end,
    __sub = function(self, other)
      return V2(self.x - other.x, self.y - other.y)
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, x, y)
      self.x, self.y = x, y
    end,
    __base = _base_0,
    __name = "V2"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  V2 = _class_0
  return _class_0
end
