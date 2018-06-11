# let us try to analyze games
examples.analyze = function() {
  set.storing(TRUE)
  games.dir = "D:/libraries/gtree/myproject/games"
	restore.point.options(display.restore.point = !TRUE)


	backup = new.env()
	gameId = c("UltimatumGame","StackelInfo", "Stackelberg","ChattyDictator","Cournot")
	gameId = "Centipede"
	gameId = NULL
	tag.df = gtree_analyze(gameId=gameId, games.dir=games.dir, backup=backup)

	df = tag.df %>% filter(tag %in% c("sim_move","dictator_like"))

	library(tidyr)
	var.df = filter(tag.df, object=="game", !is.na(variant)) %>%
	  spread(key=tag, value=value, convert=TRUE) %>%
	  select(-object, -objectId)


	df = backup$df


  tag.df = gtree_analyze_perfect_info(gameId, games.dir=games.dir)

	gameIds = list.dirs(games.dir, recursive=FALSE,full.names = FALSE)
	for (gameId in gameIds) {
	  cat("\ngameId")
    tag.df = gtree_analyze_perfect_info(gameId, games.dir=games.dir)
	}
  tag.df
}

gtree_analyze = function(analyzer=NULL, gameId=NULL, variant=NULL, games.dir, rg=NULL, vg=NULL,tg=NULL, backup=NULL, ...) {
  restore.point("gtree_analyze")

  # multiple games
  if (is.null(gameId)) {
	  gameId = list.dirs(games.dir, recursive=FALSE,full.names = FALSE)
  }

  # load all analyzer infos and check whether we need
  # the tg
  anfo = gtree.analyzers.info()
  if (is.null(analyzer)) {
    analyzer = anfo$analyzer
  } else {
    anfo = anfo[match(analyzer,anfo$analyzer),]
  }

  game.analyzers = analyzer[!anfo$by.variant]
  var.analyzers = analyzer[anfo$by.variant]


  # separate into by variant analyzers and by game analyzers
  by.game = gtree_analyze.by.game(game.analyzers,gameId=gameId, games.dir=games.dir,rg=NULL,backup=backup,analyzer.info = anfo[!anfo$by.variant,])

  by.var = gtree_analyze.by.variant(var.analyzers,gameId=gameId, games.dir=games.dir,rg=NULL,backup=backup, analyzer.info = anfo[anfo$by.variant,])


  df = bind_rows(by.game,by.var) %>%
    arrange(gameId)
  df
}

gtree_analyze.by.game = function(analyzer=NULL, gameId=NULL, variant=NULL, games.dir, rg=NULL, backup=NULL, analyzer.info=NULL, ...) {

  if (length(gameId)>1) {
    return(bind_rows(lapply(gameId, function(gameId) {
      cat("\ngameId = ", gameId)
      df = gtree_analyze.by.game(analyzer=analyzer,gameId=gameId, variant=variant, games.dir=games.dir,rg=NULL,backup=backup,analyzer.info=analyzer.info,...)
      if (is.environment(backup))
        backup$df = bind_rows(backup$df, df)
      return(df)
    })))
  }

  if (is.null(rg)) {
    rg = try(get.rg(gameId=gameId, games.dir=games.dir))
    if (is(rg,"try-error")) {
      cat("\nSkip", gameId," due to error when creating rg.")
      return(NULL)
    }
  }

  analyzer.info = analyzer.info[analyzer.info$analyzer %in% analyzer,]

  if (length(analyzer)>1) {
    cat("\nRun Analyzers: ")
    return(bind_rows(lapply(analyzer, function(analyzer) {
      cat(" ",analyzer)
      gtree_analyze.by.game(analyzer=analyzer, gameId=gameId, games.dir=games.dir,rg=rg,backup=backup,analyzer.info=analyzer.info,...)
    })))
  }

  restore.point("gtree_analyze.by.game.inner")

  fun = paste0("gtree_analyze_", analyzer)
  df = do.call(fun, list(gameId=gameId, games.dir=games.dir, rg=rg))

  mutate(df,
    gameId=gameId,
    variant=NA_character_
  )
}


gtree_analyze.by.variant = function(analyzer=NULL, gameId=NULL, variant=NULL, games.dir, rg=NULL, vg=NULL,tg=NULL, backup=NULL, analyzer.info, ...) {

  # multiple games
  if (is.null(gameId)) {
	  gameId = list.dirs(games.dir, recursive=FALSE,full.names = FALSE)
  }

  if (length(gameId)>1) {
    return(bind_rows(lapply(gameId, function(gameId) {
      cat("\ngameId = ", gameId)
      df = gtree_analyze.by.variant(analyzer=analyzer,gameId=gameId, variant=variant, games.dir=games.dir,rg=NULL,backup=backup, analyzer.info=analyzer.info,...)
      if (is.environment(backup))
        backup$df = bind_rows(backup$df, df)
      return(df)
    })))
  }


  if (is.null(rg)) {
    rg = try(get.rg(gameId=gameId, games.dir=games.dir))
    if (is(rg,"try-error")) {
      cat("\nSkip", gameId," due to error when creating rg.")
      return(NULL)
    }
  }

  # Deal with multiple variants
  if (is.null(variant))
    variant = rg$variants

  if (is.numeric(variant))
    variant = rg$variants[variant]

  if (length(variant)>1) {
    return(bind_rows(lapply(variant, function(variant)
      gtree_analyze.by.variant(analyzer=analyzer, gameId=gameId, variant=variant, games.dir=games.dir,rg=rg, backup=backup, analyzer.info=analyzer.info,...)
    )))
  }

  if (is.null(vg)) {
    vg = try(get.vg(rg=rg, variant=variant, games.dir=games.dir))
    if (is(vg,"try-error")) {
      cat("\nSkip", gameId,"variant", variant," due to error when creating vg.")
      return(NULL)
    }

  }

  #restore.point("hkshfkdhf")
  #stop()
  analyzer.info = analyzer.info[analyzer.info$analyzer %in% analyzer,]


  need.tg = any(analyzer.info$need.tg)
  # possibly we should also load tg here...
  if (is.null(tg) & need.tg) {
    cat("\nGet tg for ", gameId,"...")
    tg = try(get.tg(gameId=gameId, variant=variant, games.dir=games.dir))
    if (is(tg, "try-error")) {
      tg = list(ok=FALSE)
    } else {
      tg$ok = tg$kel$count == 0
    }
    cat(" ... done.")
  }

  if (length(analyzer)>1) {
    cat("\nRun Analyzers: ")
    return(bind_rows(lapply(analyzer, function(analyzer) {
      cat(" ",analyzer)
      gtree_analyze.by.variant(analyzer=analyzer, gameId=gameId, variant=variant, games.dir=games.dir,rg=rg,vg=vg,tg=tg,backup=backup, analyzer.info=analyzer.info,...)
    })))
  }

  restore.point("gtree_analyze.by.variant.inner")


  fun = paste0("gtree_analyze_", analyzer)
  df = do.call(fun, list(gameId=gameId, variant=variant, games.dir=games.dir, rg=rg, vg=vg, tg=tg))


  mutate(df,
    gameId=gameId,
    variant=variant
  )
}

gtree.analyze.spread.values = function(df) {
  restore.point("gtree.analyze.spread.values")
  #df$string_value[df$string_value=="NULL"] = NA_character_
  spread(df,key=tag, value=value, convert=TRUE)
}

gtree.analyzers.info = function() {
  bind_rows(list(
    list(analyzer="rg_basic",need.tg=TRUE, by.variant=FALSE),
    list(analyzer="rg_addins",need.tg=FALSE, by.variant=FALSE),

    list(analyzer="feasible_gametree",need.tg=TRUE, by.variant=TRUE),
    list(analyzer="tg_basic",need.tg=TRUE, by.variant=TRUE),
    list(analyzer="tg_addins",need.tg=TRUE, by.variant=TRUE),
    list(analyzer="perfect_info",need.tg=TRUE, by.variant=TRUE),
    list(analyzer="move_sequence",need.tg=TRUE, by.variant=TRUE)

  ))
}


gtree_analyze_feasible_gametree = function(tg, gameId,...) {
  restore.point("gtree_analyze_feasible_gametree")
  gtree_df(tag=c("feasible_gametree","branching_limit"), object="game", objectId=gameId, value=list(!is.null(tg$ise.df),tg$branching.limit))
}


gtree_analyze_rg_basic = function(rg,gameId, ...) {
  restore.point("gtree_analyze_rg_basic")
  num_variants = length(rg$variants)
  max_players = max(rg$varpar$numPlayers)
  min_players = min(rg$varpar$numPlayers)

  gtree_df(nlist(num_variants, max_players, min_players), "game",gameId)
}


# problem: we do not yet know, for which variants the addins are
# indeed used
gtree_analyze_rg_addins = function(rg, gameId,...) {
  restore.point("gtree_analyze_feasible_addins")
  if (length(rg$ai.types)==0)
    return(NULL)

  gtree_df(tag=paste0("has_", paste0(rg$ai.types)),"game", gameId, value=TRUE)

}

gtree_analyze_tg_basic = function(tg,gameId, ...) {
  restore.point("gtree_analyze_tg_basic")

  ise.df = tg$ise.df
  if (is.null(ise.df)) return(NULL)

  num_players = tg$numPlayers
  num_action_players = n_distinct(ise.df$.player)
  has_passive_player = num_action_players<tg$numPlayers


  df = gtree_df(tag=c("num_players", "num_action_players","has_passive_player"), object="game", objectId=gameId, value=list(num_players,num_action_players,has_passive_player))

  # check if we have a dictator like game
  if (has_passive_player & num_action_players == 1) {
    # make sure that payoffs of active and passive
    # players are not fixed
    pp = setdiff(1:tg$numPlayers, ise.df$.player)
    pp.differ = n_distinct(unlist(tg$oco.df[,paste0("payoff_", pp)])) > 1
    dictator_like = pp.differ

    df = bind_rows(df,
      gtree_df(tag=c("dictator_like"), object="game", objectId=gameId, value=dictator_like)
    )
  }

  df
}

# Find all addins relevant for the current variant
gtree_analyze_tg_addins = function(tg,gameId,rg, ...) {
  restore.point("gtree_analyze_tg_addins")

  if (!tg$ok) return(NULL)

  # Extract all addin types of stages that are indeed
  # played in the variant
  ai.types = unique(unlist(lapply(seq_along(tg$stages),function(stage.ind) {
    if (tg$stages[[stage.ind]]$never.played) return(NULL)
    sapply(rg$stages[[stage.ind]]$ai.li, function(ai) ai$type)
  })))

  if (length(ai.types)==0) return(NULL)

  gtree_df(tag=paste0("has_", ai.types),"game", gameId, value=TRUE)

}



gtree_analyze_perfect_info = function(tg,gameId, ...) {
  restore.point("gtree_analyze_perfect_info")

  ise.df = tg$ise.df
  if (is.null(ise.df)) {
    # cannot analyze perfect info since no feasible gametree
    return(NULL)
  }


  # actions that are chosen under imperfect information
  ii.actions = unique(ise.df$.var[ise.df$.num.nodes>1])
  # actions that are chosen given perfect information
  pi.actions = setdiff(unique(ise.df$.var), ii.actions)

  game = gtree_df(tag="perfect_info", "game", gameId, value=length(ii.actions)==0)

  actions = gtree_df(tag="all_infoset_singleton", object="action", objectId=c(ii.actions, pi.actions), value=c(rep(FALSE, length(ii.actions)),rep(TRUE, length(pi.actions))))

  df = rbind(game, actions)

  # Do we have a multi action game

}


gtree_analyze_move_sequence = function(tg, gameId,...) {
  restore.point("gtree_analyze_move_sequence")
  n = tg$numPlayers
  ise.df = tg$ise.df
  if (is.null(ise.df)) return(NULL)

  df = unique(select(ise.df, .player, .var))
  multi_actions = any( table(df$.player) > 1)

  if (multi_actions) {
    # TO DO, WE MUST REMOVE ADDITIONAL VARIABLES IN A STAGE
    # IF A PLAYER SETS HER VARIABLES IN MULTIPLE STAGES
    # IT IS NO MORE A SIMULTANEOUS MOVE GAME
    restore.point("multimove.sim.detection")
    last.stage = 0
    ignore.lev.num = NULL

    for (lev in tg$lev.li) {
      if (lev$type != "action") next
      # additional action taken simultaneously in
      # same stage. Ignore for simultaneous move
      # action space
      if (lev$stage.num == last.stage) {
        ignore.lev.num = c(ignore.lev.num, lev$lev.num)
      } else {
        last.stage = lev$stage.num
      }
    }
    # remove additional actions in a stage from ise.df
    ise.df = ise.df[! ise.df$.lev.num %in% ignore.lev.num,]
  }

  # we have a simultanous
  # move game whenever each player has only a single
  # information set (ignoring multiple actions in a stage)
  sim_move = sum(duplicated(unique(select(ise.df, .info.set.ind, .player))$.player))==0

  # we need at least two action players to truly have a simultaneous
  # move game (dictator game should not count)
  if (n_distinct(ise.df$.player)==1) sim_move = FALSE

  gtree_df(tag=c("has_multi_actions","sim_move"), object="game", objectId=gameId, value=c(multi_actions, sim_move))


}

gtree_df = function(tag, object="game", objectId, value=NULL) {

  if (is.list(tag)) {
    value = as.character(tag)
    tag = names(tag)
  } else {
    value = as.character(value)
  }
  fast_df(tag=tag, object=object, objectId=objectId, value=value)
}
