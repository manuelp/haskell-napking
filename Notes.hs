module Notes where

data Atom
  = TextNote [Tag]
             Notes
             [Atom]
  | Link URL
         [Tag]
         Notes
         [Atom]
  | Asset FilePath
          [Tag]
          Notes
          [Atom]

type Tag = String
type URL = String
type Notes = String

-- See also: https://github.com/synchrony/smsn-why/blob/master/invitations/to-coders.md
