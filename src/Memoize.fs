namespace fsandbox

open fsandbox.Common

module Memoize =

    let private useCache<'key, 'value when 'key : equality and 'value : equality> () =
        let mutable dic = System.Collections.Generic.Dictionary<'key, 'value>()

        let tryGetValue n =
            match dic.TryGetValue n with
            |true, m -> ValueSome m
            |false, _ -> ValueNone
        
        let set n m =
            dic[n] <- m
            m

        tryGetValue, set

    let memoize f =
        let get, set = useCache()

        let f' n =
            match get n with
            |ValueSome m -> m
            |ValueNone -> f n |> set n

        f'

    let memoize2 f =
        uncurry f |> memoize |> curry

    let memoize3 f =
        uncurry3 f |> memoize |> curry3


    let memoize_cps f =
        let get, set = useCache()

        let f' n cont =
            match get n with
            |ValueSome m -> m |> cont
            |ValueNone -> f n (fun m -> m |> set n |> cont)

        f'

    let memoize2_cps f =
        uncurry f |> memoize_cps |> curry

    let memoize3_cps f =
        uncurry3 f |> memoize_cps |> curry3

    
    let fix_memo f = 
        let rec fix x = f m_fix x
        and m_fix = memoize fix

        m_fix

    let fix_memo2 f = 
        let rec fix x = f m_fix x 
        and m_fix = memoize2 fix

        m_fix

    let fix_memo3 f =
        let rec fix x = f m_fix x
        and m_fix = memoize3 fix

        m_fix

    let fix_memo_cps f =
        let rec fix x = f m_fix x
        and m_fix = memoize_cps fix

        fun x -> m_fix x id

    let fix_memo2_cps f =
        let rec fix x = f m_fix x
        and m_fix = memoize2_cps fix

        fun x y -> m_fix x y id

    let fix_memo3_cps f =
        let rec fix x = f m_fix x
        and m_fix = memoize3_cps fix

        fun x y z -> m_fix x y z id