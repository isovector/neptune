export Actor, getPlayer


class HasProperties
  properties: {}
  __inherited: (cls) =>
    old_init = cls.__init
    cls.__init = (...) =>
      old_init @, ...

      mt           = getmetatable @
      old_index    = mt.__index
      old_newindex = mt.__newindex

      mt.__index = (name) => do
        if getter = (old_index.properties[name] and old_index.properties[name].get)
          getter @
        else
          if type(old_index) == "function"
            old_index @, name
          else
            old_index[name]

      mt.__newindex = (name, value) =>
        if setter = (old_index.properties[name] and old_index.properties[name].set)
          setter @, value
        else
          if type(old_newindex) == "function"
            old_newindex @, name, value
          else
            rawset(@, name, value)


class Actor extends HasProperties
  new: (@ent=hsNewEntity()) =>

  properties:
    pos:
      get:       => hsGetPos(@ent)
      set: (pos) => hsSetPos(@ent, pos)
    talkColor:
      get:       => hsGetTalkColor(@ent) or rgb(1, 0, 1)
      set: (col) => hsSetTalkColor(@ent, col)

  say: (what) =>
    hsSay(@talkColor, @pos - V2(0, 30), what)

  walkTo: (where) =>
    hsWalkTo(@ent, where)

  interact: (verb) =>

getPlayer = () => Actor(hsGetPlayer())

