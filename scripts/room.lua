do
  local _class_0
  local _base_0 = {
    newActor = function(self)
      do
        local ent = Actor()
        ent.fromRoom = self.name
        return ent
      end
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, name, load)
      self.name, self.load = name, load
      rooms[self.name] = self
    end,
    __base = _base_0,
    __name = "Room"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Room = _class_0
end
roomId = enum({
  "costume",
  "city"
})
rooms = { }
