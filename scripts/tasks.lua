local Task
do
  local _class_0
  local _base_0 = { }
  _base_0.__index = _base_0
  _class_0 = setmetatable({
    __init = function(self, fn)
      do
        self.job = coroutine.create(fn)
        self.time = 0
        self.event = nil
      end
    end,
    __base = _base_0,
    __name = "Task"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Task = _class_0
end
do
  local _class_0
  local _base_0 = {
    start = function(self, fn)
      do
        return table.insert(self.tasks, Task(fn))
      end
    end,
    continue = function(self, dt)
      do
        do
          local _accum_0 = { }
          local _len_0 = 1
          for _, task in ipairs(self.tasks) do
            local _continue_0 = false
            repeat
              task.time = task.time - dt
              if not task.event and task.time <= 0 then
                local status, typ, arg = coroutine.resume(task.job)
                if not (status) then
                  _continue_0 = true
                  break
                end
                local _exp_0 = typ
                if "sleep" == _exp_0 then
                  task.time = arg
                elseif "waitForEvent" == _exp_0 then
                  task.event = arg
                end
              end
              local _value_0 = task
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
      self.tasks = { }
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
sleep = function(dt)
  return coroutine.yield("sleep", dt)
end
waitForEvent = function(ev)
  return coroutine.yield("waitForEvent", ev)
end
