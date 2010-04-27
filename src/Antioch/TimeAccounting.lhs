> module Antioch.TimeAccounting where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities

The names of the core functions for time accounting consist of three fields:
    [ p | s ]
    [ Allotted | Committed | Used | Past | Avail | Remain | Future ]
    [ S | T ]
  where:
    p = project
    s = session
    -
    Allotted = apportioned time for the project or session
    Committed = pTimeBilled in its Pending/Scheduled/Completed periods
    Used = pTimeBilled in its Scheduled/Completed periods
    Past = pTimeBilled in its Scheduled/Completed periods started
           prior to the scheduling time
    Avail = Allotted* minus Committed
    Remain = Allotted* minus Used
    Future = Allotted* minus Past
    -
    S = Semester
    T = Total

* - semester Avail/Remain time uses the minimum of the semester and
    total allotted time.

Time Accounting is conceptually simple: sessions and projects have been
allotted only so much time, and we don't want to schedule them over this
amount of time.

But the devil is in the details, e.g., Project Time vs. Session Time - a
session can't be scheduled anymore if it's project is out of time.  Thus
you have to check both session & project.

   * Possible factors:
      - project/session close flag
      - project/session time available
      - project semester time available

The requirements below for project completion are drawn from Project
Note 11.3.  A project is considered closed within the DSS when any of:
   1. The total amount of time allotted for that project has been billed
      (less than 15 minutes remains),
   2. The project is marked as closed by a project investigator or the
      GBT telescope scheduler,

Checked factors:
   - project close flag
   - project time available

> pComplete :: Project -> Bool
> pComplete p = (pClosed p) || ((pAvailT p) < quarter )
> -- NOTE: project completeness is NOT dependent on session completeness:
> --  where
> --    allSessClosed p = all (==True) $ map sClosed $ sessions p

How much time has this session used up in periods?
The requirements below for session completion are drawn from Project Note 11.3.
A session is considered closed within the DSS when any of:
   1. the total amount of time allotted for that session has been billed (less than 15 minutes remains),
   2. the session is marked as closed by a project investigator or the GBT telescope scheduler, or
   3. the project to which it belongs is closed.

Checked factors:
   - project close flag
   - session close flag
   - session time available

> sComplete :: Session -> Bool
> sComplete s = (sTerminated s) || ((sAvailT s) < quarter )

> sTerminated :: Session -> Bool
> sTerminated s = (sClosed s) || (pClosed . project $ s)

How much time has this session used up in periods?

> sCommittedT :: Session -> Minutes
> sCommittedT = sum . map pTimeBilled . periods

> sCommittedS :: SemesterName -> Session -> Minutes
> sCommittedS sem s = sum $ map pTimeBilled $ periodsBySemester sem s

> sUsedT :: Session -> Minutes
> sUsedT = sum . map pTimeBilled . filter isUsed . periods

> sUsedS :: SemesterName -> Session -> Minutes
> sUsedS sem s = sum $ map pTimeBilled . filter isUsed . periodsBySemester sem $ s 

> isUsed :: Period -> Bool
> isUsed p = (pState p) `elem` [Scheduled, Complete]

> sPastT :: DateTime -> Session -> Minutes
> sPastT dt = sum . map pTimeBilled . filter (isPast dt) . periods

> sPastS :: DateTime -> Session -> Minutes
> sPastS dt s = sum $ map pTimeBilled . filter (isPast dt) . periodsBySemester (dt2semester dt) $ s

> isPast :: DateTime -> Period -> Bool
> isPast dt p = isUsed p && (startTime p) < dt

How much time has this project used up in periods?

> pCommittedT :: Project -> Minutes
> pCommittedT p = sum . map sCommittedT . sessions $ p

> pCommittedS :: SemesterName -> Project -> Minutes
> pCommittedS sem p= sum . map (sCommittedS sem) . sessions $ p

> pUsedT :: Project -> Minutes
> pUsedT p = sum . map sUsedT . sessions $ p

> pUsedS :: SemesterName -> Project -> Minutes
> pUsedS sem p = sum . map (sUsedS sem) . sessions $ p

> pPastT :: DateTime -> Project -> Minutes
> pPastT dt p = sum . map (sPastT dt) . sessions $ p

> pPastS :: DateTime -> Project -> Minutes
> pPastS dt p = sum . map (sPastS dt) . sessions $ p

Returns the minutes available for scheduling for this session,
i.e., time that is not encumbered in any way and therefore
completely open for scheduling, tentative or not.

Checked factors:
   - session time available

> sAvailT :: Session -> Minutes
> sAvailT s = (sAllottedT s) - (sCommittedT s)

> sRemainT :: Session -> Minutes
> sRemainT s = (sAllottedT s) - (sUsedT s)

> sFutureT :: DateTime -> Session -> Minutes
> sFutureT dt s = (sAllottedT s) - (sPastT dt s)

The time available to this session might actually be further restricted by 
the time available to it's project, which may depend on which its semester.

Checked factors:
   - project time available
   - session time available
   - project semester time available

> sAvailS :: SemesterName -> Session -> Minutes
> sAvailS sem s = min (sAllottedT s) (sAllottedS s) - (sCommittedS sem s)

> sRemainS :: SemesterName -> Session -> Minutes
> sRemainS sem s = min (sAllottedT s) (sAllottedS s) - (sUsedS sem s)

> sFutureS :: DateTime -> Session -> Minutes
> sFutureS dt s = min (sAllottedT s) (sAllottedS s) - (sPastS dt s)

Checked factors:
   - project time available

> pAvailT :: Project -> Minutes
> pAvailT p = (pAllottedT p) - (pCommittedT p)

> pRemainT :: Project -> Minutes
> pRemainT p = (pAllottedT p) - (pUsedT p)

> pFutureT :: DateTime -> Project -> Minutes
> pFutureT dt p = (pAllottedT p) - (pPastT dt p)

Usually, the time available for a project is simply it's total time minus
the time it has already used up.  But for large projects, it may be allowed
only a certain amount of time per semester.

Checked factors:
   - project time available
   - project semester time available

> pAvailS :: SemesterName -> Project -> Minutes
> pAvailS sem p = min (pAllottedT p) (pAllottedS p) - (pCommittedS sem p)

> pRemainS :: SemesterName -> Project -> Minutes
> pRemainS sem p = min (pAllottedT p) (pAllottedS p) - (pUsedS sem p)

> periodsBySemester :: SemesterName -> Session -> [Period]
> periodsBySemester sem s = filter (isSemester sem) $ periods s
>   where
>     isSemester sem p = sem == (dt2semester  . startTime $ p)


