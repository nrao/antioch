> module Antioch.TimeAccounting where

> import Antioch.DateTime
> import Antioch.Types
> import Antioch.Utilities
> --import Debug.Trace

The names of the core functions for time accounting consist of three fields:
    [ p | s ] [ Allotted | Committed | Used | Avail ] [ S | T ]
  where:
    p = project
    s = session
    S = Semester
    T = Total
    Allotted = apportioned time for the project or session
    Committed = pTimeBilled in Pending and Scheduled periods
    Used = pTimeBilled in Scheduled periods
    Avail = Allotted minus Committed

Time Accounting is conceptually simple: sessions and projects have been
allotted only so much time, and we don't want to schedule them over this
amount of time.

But the devil is in the details.  Like:

   * Project Time vs. Session Time - a session can't be scheduled anymore if
   it's project is out of time.  Thus you have to check both session & project.

   * Project.pAllottedS - a project, especially a large one, may only
   be allowed to observe so much in a single semester (and this may be
   less then the projects total alloted time).  This means that query's into
   the available time of a semester have to specify the semester as well.

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
> pComplete p = (pClosed p) || ((pAvailT p) <= quarter )
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
> sComplete s = (sTerminated s) || ((sAvailT s) <= quarter )

> sTerminated :: Session -> Bool
> sTerminated s = (sClosed s) || (pClosed . project $ s)

How much time has this session used up in periods?

> sCommittedT :: Session -> Minutes
> sCommittedT = sum . map pTimeBilled . periods

> sCommittedS :: SemesterName -> Session -> Minutes
> sCommittedS sem s = sum $ map pTimeBilled $ periodsBySemester sem s

> sUsedT :: Session -> Minutes
> sUsedT = sum . map pTimeBilled . filter isCommitted . periods

> sUsedS :: SemesterName -> Session -> Minutes
> sUsedS sem s = sum $ map pTimeBilled . filter isCommitted . periodsBySemester sem $ s 

> isCommitted :: Period -> Bool
> isCommitted p = (pState p) `elem` [Scheduled, Complete]

How much time has this project used up in periods?

> pCommittedT :: Project -> Minutes
> pCommittedT p = sum . map sCommittedT . sessions $ p

> pCommittedS :: SemesterName -> Project -> Minutes
> pCommittedS sem p= sum . map (sCommittedS sem) . sessions $ p

> pUsedT :: Project -> Minutes
> pUsedT p = sum . map sUsedT . sessions $ p

> pUsedS :: SemesterName -> Project -> Minutes
> pUsedS sem p= sum . map (sUsedS sem) . sessions $ p

Returns the minutes available for scheduling for this session,
i.e., time that is not encumbered in any way and therefore
completely open for scheduling, tentative or not.

Checked factors:
   - session time available

> sAvailT :: Session -> Minutes
> sAvailT s = (sAllottedT s) - (sCommittedT s)

The time available to this session might actually be further restricted by 
the time available to it's project, which may depend on which its semester.

Checked factors:
   - project time available
   - session time available
   - project semester time available

> sAvailS :: SemesterName -> Session -> Minutes
> sAvailS sem s = min (sAllottedT s) (sAllottedS s) - (sCommittedS sem s)

Checked factors:
   - project time available

> pAvailT :: Project -> Minutes
> pAvailT p = (pAllottedT p) - (pCommittedT p)

Usually, the time available for a project is simply it's total time minus
the time it has already used up.  But for large projects, it may be allowed
only a certain amount of time per semester.

Checked factors:
   - project time available
   - project semester time available

> pAvailS :: SemesterName -> Project -> Minutes
> pAvailS sem p = min (pAllottedT p) (pAllottedS p) - (pCommittedS sem p)

> periodsBySemester :: SemesterName -> Session -> [Period]
> periodsBySemester sem s = filter (isSemester sem) $ periods s
>   where
>     isSemester sem p = sem == (dt2semester  . startTime $ p)


