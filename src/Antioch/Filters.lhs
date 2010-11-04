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

> isNotTypeFixed :: SelectionCriteria
> isNotTypeFixed _ _ s = sType s /= Fixed

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
>   | typeWindowed s      = (activeWindows dt dur (windows s)) /= []
>   | otherwise           = False -- must be Fixed.

An active window:
    - overlaps the scheduling range
    - does not overlap its default period
    - has time remaining
    - is not complete

> activeWindows :: DateTime -> Minutes -> [Window] -> [Window]
> activeWindows dt dur ws = filter schedulableWindow ws
>     where
>   dtEnd = dur `addMinutes` dt
>   schedulableWindow w = all ($ w) [intersect, withNoDefault, hasTime, isNotComplete]
>   intersect w     = wStart w < dtEnd && dt < wEnd w
>   withNoDefault w = not $ overlie dt dur (maybe defaultPeriod id . wPeriod $ w)
>   hasTime w       = (wTotalTime w) >= quarter
>   isNotComplete w = not . wComplete $ w

Modify the min/max duration & alloted time of a windowed session 
to the length of its first active window's total time.  This ensures:
   * that we will get a chosen period scheduled of the correct duration
   * that we will only get ONE chosen period scheduled in the call 2 pack
Note this code assumes that minDuration == maxDuration.

> adjustWindowSessionDuration :: DateTime -> Minutes -> Session -> Session
> adjustWindowSessionDuration dt dur s
>     | sType s == Windowed        = s'
>     | otherwise                  = s
>   where
>     aws = activeWindows dt dur . windows $ s
>     s'
>         | aws == []   = s
>         | otherwise   = s { minDuration = mmd
>                           , maxDuration = mmd   
>                           -- we want the allotted time to be enough such
>                           -- that the session has just enough sAvailT
>                           -- such that it gets scheduled only once
>                           , sAllottedT  = mmd  + (sCommittedT s) 
>                           , sAllottedS  = mmd  + (sCommittedT s) 
>                           }
>     -- for windows read from the DB, wTotalTime is really the time
>     -- remaining for a window, which is dynamically calculated when
>     -- read from the DB as being the window's total time minus the
>     -- time billed of all other periods in the window.
>     mmd = wTotalTime . head $ aws 

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
>       , projectNotBlackedOut
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

If a session's project's blackout dates completly cover the given
time range, then this is true.

> projectNotBlackedOut :: SelectionCriteria
> projectNotBlackedOut dt dur s = all (not . cover start end) bs
>   where
>     bs = pBlackouts . project $ s
>     start = dt
>     end = addMinutes' dur dt
>     cover s1 e1 (s2, e2) = (s2 <= s1) && (e1 <= e2)  

