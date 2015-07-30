module Make (S : AgentBased.System) = struct

  module Exp = Explore.Make (S)

  module Bac = BranchAndCut.Make(Exp)

  let run ?size_limit rules obs_id = 

    try
      let env = Exp.environment ?size_limit rules in
      let init_id, env = Exp.generate_id env in
      let s = Exp.initial_pstory env init_id obs_id in

      Log.put "Starting the branch and cut algorithm..." ;
      let final_env = Bac.run env s in
      final_env.Exp.stories
    with Exp.Unreachable -> []

end
