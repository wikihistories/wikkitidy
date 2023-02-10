get_user_contributions <- function(user, limit=50, ...) {
  params <- rlang::list2(
    list="usercontribs",
    ucuser=user,
    uclimit=limit,
    ...
  )
  resp <- wiki_action_request(!!!params)
}
