> module Antioch.Filters where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.TimeAccounting
> import Antioch.Utilities    (between, showList', dt2semester, overlie)

Pass on to the simulation only the history of pre-scheduled periods that 
we care about: those that fall in between the dates we are simulating for.
We do this, because otherwise the reports at the end of the simulations will
be confused and raise false alarams.

> filterHistory :: [Period] -> DateTime -> Int -> [Period]
> filterHistory ps start dur = filter inWindow ps
>   where
>     end = (dur*24*60) `addMinutes'` start
>     endTime p = (duration p) `addMinutes'` (startTime p)
>     inWindow p = endTime p >= start && startTime p <= end 

> filterHistory' :: [Period] -> DateTime -> Minutes -> [Period]
> filterHistory' ps start dur = filter inWindow ps
>   where
>     end = dur `addMinutes'` start
>     endTime p = (duration p) `addMinutes'` (startTime p)
>     inWindow p = endTime p >= start && startTime p <= end 

Not all sessions should be considered for scheduling.  We may not one to pass
Sessions that:
   * are disabled/unauthorized
   * have no time left (due to Periods)
   * have been marked as complete
   * more ...

> type SelectionCriteria = DateTime -> Session -> Bool

Possible factors:
   - project time available
   - session time available
   - project semester time available

> hasTimeSchedulable :: SelectionCriteria
> hasTimeSchedulable dt s = sAvail > 0 &&
>                           sAvail >= minDur &&
>                           pAvail > 0 &&
>                           pAvail >= minDur
>   where 
>     pAvail = pAvailS sem . project $ s
>     sAvail = sAvailS sem s
>     minDur = minDuration s
>     sem = dt2semester dt

Possible factors:
   - project complete flag
   - session complete flag
   - project time available
   - session time available

> isNotComplete :: SelectionCriteria
> isNotComplete _ s = not . sComplete $ s

> isNotTerminated :: SelectionCriteria
> isNotTerminated _ s = not . sTerminated $ s

> isTypeOpen :: SelectionCriteria
> isTypeOpen _ s = sType s == Open

> isGradeA_B :: SelectionCriteria
> isGradeA_B _ s = grade s >= 2.8

> isNotMaintenance :: SelectionCriteria
> isNotMaintenance _ s = (pName . project $ s) /= "Maintenance"

> isBackup :: SelectionCriteria
> isBackup _ s = backup s

> isApproved :: SelectionCriteria
> isApproved _ s = all (\f -> f s) [enabled, authorized]

> isAuthorized :: SelectionCriteria
> isAuthorized _ s = authorized s

> hasObservers :: SelectionCriteria
> hasObservers _ s = not . null . observers . project $ s

Filter candidate sessions dependent on its type.

> isSchedulableType :: DateTime -> Minutes -> Session -> Bool
> isSchedulableType dt dur s
>   -- Open
>   | isTypeOpen dt s     = True
>   | sType s == Windowed = activeWindows (windows s)
>   | otherwise           = False -- must be Fixed.  
>     where
>       activeWindows ws
>         -- Windowed with no windows overlapping the scheduling range
>         -- or those windows that are overlapped by the scheduling range
>         -- also overlap their default periods.
>         | filter schedulableWindow ws == [] = False
>         | otherwise                         = True
>
>       schedulableWindow w = (intersect w) && (withNoDefault $ w)
>       intersect w = wStart w < dtEnd && dt < wEnd w
>       withNoDefault w = not $ overlie dt dur (maybe defaultPeriod id . wPeriod $ w)
>       wEnd w = (wDuration w) `addMinutes` (wStart w)
>       dtEnd = dur `addMinutes` dt

We are explicitly ignoring grade here: it has been decided that a human
should deal with closing old B projects, etc.

> -- TBF is this needed?
> isSchedulableSemester :: SelectionCriteria 
> isSchedulableSemester dt s = (semester $ project s) <= current_semester
>    where
>      current_semester = dt2semester dt

> filterSessions :: DateTime -> [SelectionCriteria] -> [Session] -> [Session]
> filterSessions dt []       ss = ss
> filterSessions dt (sc:scs) ss = filterSessions dt scs $ filter (sc dt) ss

> meetsCriteria :: DateTime -> Session -> [SelectionCriteria] -> Bool
> meetsCriteria dt s []       = True
> meetsCriteria dt s (sc:scs) = (sc dt s) && (meetsCriteria dt s scs)

Note, selection by type is handled separately by isSchedulableType
because it requires arguments describing the time period being
scheduled.

> schedulableCriteria :: [SelectionCriteria]
> schedulableCriteria = [
>         hasTimeSchedulable
>       , isNotComplete
>       , isApproved
>       , hasObservers
>                       ]

> schedulableSessions :: DateTime -> [Session] -> [Session]
> schedulableSessions dt = filterSessions dt schedulableCriteria

> clearWindowedTimeBilled :: Session -> Session
> clearWindowedTimeBilled s
>   | (windows s) == [] = s
>   | otherwise         = makeSession s (windows s) ps
>       where
>         ps = map clear . periods $ s
>         clear p
>           | elem (peId p) pIds = p { pTimeBilled = 0 }
>           | otherwise          = p
>         pIds = [wPeriodId w | w <- (windows s)]

> schedulableSession :: DateTime -> Session -> Bool
> schedulableSession dt s = meetsCriteria dt s schedulableCriteria

> scoringSessions :: DateTime -> [Session] -> [Session]
> scoringSessions dt = filterSessions dt [
>         isGradeA_B
>       , isNotMaintenance
>        ]

