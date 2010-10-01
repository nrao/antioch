> module Antioch.Filters where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities    (showList', dt2semester, overlie)

Pass on to the simulation only the history of pre-scheduled periods that 
we care about: those that fall in between the dates we are simulating for.
We do this, because otherwise the reports at the end of the simulations will
be confused and raise false alarams.

> filterHistory :: [Period] -> DateTime -> Int -> [Period]
> filterHistory ps start daysDur = filter overlie' ps
>   where
>     overlie' p = overlie start (daysDur*24*60) p

> typeOpen , typeWindowed , typeFixed :: Session -> Bool
> typeOpen s = sType s == Open
> typeWindowed s = sType s == Windowed
> typeFixed s = sType s == Fixed

Not all sessions should be considered for scheduling.  We may not one to pass
Sessions that:
   * are disabled/unauthorized
   * have no time left (due to Periods)
   * have been marked as complete
   * more ...

> type SelectionCriteria = DateTime -> Minutes -> Session -> Bool

Possible factors:
   - project time available
   - session time available
   - project semester time available

> hasTimeSchedulable :: SelectionCriteria
> hasTimeSchedulable _ _ s = sAvail > 0 &&
>                            sAvail >= minDur &&
>                            pAvail > 0 &&
>                            pAvail >= minDur
>   where 
>     pAvail = pAvailT . project $ s
>     sAvail = sAvailT s
>     minDur = minDuration s

Possible factors:
   - project complete flag
   - session complete flag
   - project time available
   - session time available

> isNotComplete :: SelectionCriteria
> isNotComplete _ _ s = not . sComplete $ s

> isNotTerminated :: SelectionCriteria
> isNotTerminated _ _ s = not . sTerminated $ s

> isTypeOpen :: SelectionCriteria
> isTypeOpen _ _ s = sType s == Open

> isGradeA_B :: SelectionCriteria
> isGradeA_B _ _ s = grade s >= 2.8

> isNotMaintenance :: SelectionCriteria
> isNotMaintenance _ _ s = (pName . project $ s) /= "Maintenance"

> isBackup :: SelectionCriteria
> isBackup _ _ s = backup s

> isApproved :: SelectionCriteria
> isApproved _ _ s = all (\f -> f s) [enabled, authorized]

> isAuthorized :: SelectionCriteria
> isAuthorized _ _ s = authorized s

> hasObservers :: SelectionCriteria
> hasObservers _ _ s = not . null . observers . project $ s

Filter candidate sessions dependent on its type.

> isSchedulableType :: SelectionCriteria
> isSchedulableType dt dur s
>   -- Open
>   | isTypeOpen dt dur s = True
>   | typeWindowed s      = activeWindows (windows s)
>   | otherwise           = False -- must be Fixed.
>     where
>       activeWindows ws
>         -- Windowed with no windows overlapping the scheduling range
>         -- or those windows that are overlapped by the scheduling range
>         -- also overlap their default periods.
>         | filter schedulableWindow ws == [] = False
>         | otherwise                         = True
>
>       schedulableWindow w = all ($ w) [intersect, withNoDefault, needsPeriod]
>       intersect w = wStart w < dtEnd && dt < wEnd w
>       withNoDefault w = not $ overlie dt dur (maybe defaultPeriod id . wPeriod $ w)
>       needsPeriod w = not . wHasChosen $ w
>       dtEnd = dur `addMinutes` dt

We are explicitly ignoring grade here: it has been decided that a human
should deal with closing old B projects, etc.

> -- TBF is this needed?
> isSchedulableSemester :: SelectionCriteria 
> isSchedulableSemester dt _ s = (semester $ project s) <= current_semester
>    where
>      current_semester = dt2semester dt

> filterSessions :: DateTime -> Minutes -> [SelectionCriteria] -> [Session] -> [Session]
> filterSessions dt _   []       ss = ss
> filterSessions dt dur (sc:scs) ss = filterSessions dt dur scs $ filter (sc dt dur) ss

> meetsCriteria :: DateTime -> Minutes -> Session -> [SelectionCriteria] -> Bool
> meetsCriteria dt _ s  []        = True
> meetsCriteria dt dur s (sc:scs) = (sc dt dur s) && (meetsCriteria dt dur s scs)

Note, selection by type is handled separately by isSchedulableType
because it requires arguments describing the time period being
scheduled.

> schedulableCriteria :: [SelectionCriteria]
> schedulableCriteria = [
>         hasTimeSchedulable
>       , isNotComplete
>       , isApproved
>       , hasObservers
>       , isSchedulableType
>                       ]

> schedulableSessions :: DateTime -> Minutes -> [Session] -> [Session]
> schedulableSessions dt dur = filterSessions dt dur schedulableCriteria

> clearWindowedTimeBilled :: Session -> Session
> clearWindowedTimeBilled s
>   | (windows s) == [] = s
>   | otherwise         = makeSession s (windows s) ps
>       where
>         ps = map clear . periods $ s
>         clear p
>           | elem (peId p) pIds = p { pDuration = 0 }
>           | otherwise          = p
>         pIds = [wPeriodId w | w <- (windows s)]

> schedulableSession :: DateTime -> Minutes -> Session -> Bool
> schedulableSession dt dur s = meetsCriteria dt dur s schedulableCriteria

> scoringSessions :: DateTime -> Minutes -> [Session] -> [Session]
> scoringSessions dt dur = filterSessions dt dur [
>         isGradeA_B
>       , isNotMaintenance
>        ]

