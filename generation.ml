module Make (S : AgentBased.System) = struct

  module Exp = Explore.Make (S)

  module Bac = BranchAndCut.Make(Exp)

  let run ?(size_limit=20) ?(stories_limit=10) rules obs_ids = 

    try
      let env = Exp.environment ~size_limit ~stories_limit rules in

      let pss, env = 
        List.fold_right (fun obs (pss, env) -> 
          let ps, env = Exp.initial_pstory env obs in
          (ps :: pss, env)
        ) obs_ids ([], env) in

      let final_env = Bac.run env pss in
      final_env.Exp.stories
    with Exp.Terminated -> []

end
