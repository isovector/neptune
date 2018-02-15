local HasGetters
do
  local _class_0
  local _base_0 = {
    getters = { },
    __inherited = function(self, cls)
      local old_init = cls.__init
      cls.__init = function(self, ...)
        old_init(self, ...)
        local mt = getmetatable(self)
        local old_index = mt.__index
        mt.__index = function(self, name)
          do
            local getter = old_index.getters[name]
            if getter then
              return getter(self)
            else
              if type(old_index) == "function" then
                return old_index(self, name)
              else
                return old_index[name]
              end
            end
          end
        end
      end
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function() end,
    __base = _base_0,
    __name = "HasGetters"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  HasGetters = _class_0
end
do
  local _class_0
  local _parent_0 = HasGetters
  local _base_0 = {
    getters = {
      pos = function(self)
        return hsEntPos(self.ent)
      end,
      talkColor = function(self)
        do
          return hsEntTalkColor(self.ent) or rgb(1, 0, 1)
        end
      end
    },
    say = function(self, what)
      return hsSay(self.talkColor, self.pos - V2(0, 30), what)
    end,
    walkTo = function(self, where)
      return hsWalkTo(self.ent, where)
    end,
    interact = function(self, verb) end
  }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  _class_0 = setmetatable({
    __init = function(self, ent)
      if ent == nil then
        ent = hsNewEntity()
      end
      self.ent = ent
    end,
    __base = _base_0,
    __name = "Actor",
    __parent = _parent_0
  }, {
    __index = function(cls, name)
      local val = rawget(_base_0, name)
      if val == nil then
        local parent = rawget(cls, "__parent")
        if parent then
          return parent[name]
        end
      else
        return val
      end
    end,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  if _parent_0.__inherited then
    _parent_0.__inherited(_parent_0, _class_0)
  end
  Actor = _class_0
  return _class_0
end
