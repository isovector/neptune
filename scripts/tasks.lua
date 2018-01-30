do
  local _class_0
  local _base_0 = {
    start = function(self, f)
      do
        local job = coroutine.create(f)
        return table.insert(self.jobs, job)
      end
    end,
    continue = function(self)
      do
        do
          local _accum_0 = { }
          local _len_0 = 1
          for _, job in ipairs(self.jobs) do
            local _continue_0 = false
            repeat
              if not (coroutine.resume(job)) then
                _continue_0 = true
                break
              end
              local _value_0 = job
              _accum_0[_len_0] = _value_0
              _len_0 = _len_0 + 1
              _continue_0 = true
            until true
            if not _continue_0 then
              break
            end
          end
          self.jobs = _accum_0
        end
      end
    end
  }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self)
      self.jobs = { }
    end,
    __base = _base_0,
    __name = "TaskManager"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  TaskManager = _class_0
end
tasks = TaskManager()
