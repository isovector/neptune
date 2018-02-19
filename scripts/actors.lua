local HasProperties
do
  local _class_0
  local _base_0 = {
    properties = { },
    __inherited = function(self, cls)
      local old_init = cls.__init
      cls.__init = function(self, ...)
        old_init(self, ...)
        local mt = getmetatable(self)
        local old_index = mt.__index
        local old_newindex = mt.__newindex
        mt.__index = function(self, name)
          do
            do
              local getter = (old_index.properties[name] and old_index.properties[name].get)
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
        mt.__newindex = function(self, name, value)
          do
            local setter = (old_index.properties[name] and old_index.properties[name].set)
            if setter then
              return setter(self, value)
            else
              if type(old_newindex) == "function" then
                return old_newindex(self, name, value)
              else
                return rawset(self, name, value)
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
    __name = "HasProperties"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  HasProperties = _class_0
end
do
  local _class_0
  local _parent_0 = HasProperties
  local _base_0 = {
    properties = {
      pos = {
        get = function(self)
          return hsGetPos(self.ent)
        end,
        set = function(self, pos)
          return hsSetPos(self.ent, pos)
        end
      },
      speed = {
        get = function(self)
          return hsGetSpeed(self.ent)
        end,
        set = function(self, spe)
          return hsSetSpeed(self.ent, spe)
        end
      },
      talkColor = {
        get = function(self)
          return hsGetTalkColor(self.ent) or rgb(1, 0, 1)
        end,
        set = function(self, col)
          return hsSetTalkColor(self.ent, col)
        end
      },
      fromRoom = {
        get = function(self)
          return hsGetFromRoom(self.ent)
        end,
        set = function(self, roo)
          return hsSetFromRoom(self.ent, roo)
        end
      },
      isAvatar = {
        get = function(self)
          return hsGetIsAvatar(self.ent)
        end,
        set = function(self, yes)
          if not yes then
            return error("you can't set isAvatar to false")
          else
            return hsSetIsAvatar(self.ent, yes)
          end
        end
      },
      hasFocus = {
        get = function(self)
          return hsGetHasFocus(self.ent)
        end,
        set = function(self, yes)
          return hsSetHasFocus(self.ent, yes)
        end
      }
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
end
getPlayer = function(self)
  return Actor(hsGetPlayer())
end
