func get_Msg() -> Model {
  return Model(value:Tuple(name:"Msg",body:[
    Model(value:Chain(types:[
        Model(value:List(constant:"")),
        Model(value:Number())])),
    Model(value:Chain(types:[
        Model(value:List(constant:"")),
        get_MUC(),
        get_P2P()])),
    Model(value:List(constant:nil,model:get_Bin())),
    Model(value:Chain(types:[
        Model(value:List(constant:"")),
        Model(value:Atom())]))]))}
