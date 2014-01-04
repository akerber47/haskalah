let findi_all f xs =
  let rec do_ix i acc =
    if i >= 0 then
      if f (xs.(i)) then
        do_ix (i-1) (i::acc)
      else
        do_ix (i-1) acc
    else
      acc
  in do_ix (Array.length xs - 1) []
;;
