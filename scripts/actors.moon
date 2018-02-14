export Actor


class HasGetters
  getters: {}
  __inherited: (cls) =>
    old_init = cls.__init
    cls.__init = (...) =>
      old_init @, ...

      mt = getmetatable @
      old_index = mt.__index

      mt.__index = (name) =>
        if getter = old_index.getters[name]
          getter @
        else
          if type(old_index) == "function"
            old_index @, name
          else
            old_index[name]


class Actor extends HasGetters
  new: (@ent=hsNewEntity()) =>

  getters: {
    pos:       => hsEntPos(@ent)
    talkColor: => do
      color = hsEntTalkColor(@ent)
      if color
        color
      else
        rgb(1, 0, 1)
  }

  say: (what) =>
    hsSay(@talkColor, @pos - V2(0, 30), what)

  walkTo: (where) =>
    hsWalkTo(@ent, where)

  interact: (verb) =>


