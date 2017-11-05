-- DSL. Tasks, gates, etc. The diagram is a _compilation target_.
module Processes where

data Task
  = UserTask { userTaskName :: TaskName
             , assignee :: Assignee }
  | ServiceTask { serviceTaskName :: TaskName }
  | CombinedTask (Task, Task)
newtype TaskName = TaskName String

data Assignee
  = User UserID
  | Group GroupID
newtype UserID = UserID String
newtype GroupID = GroupID String

