-- Expr: Scheme expression module
--
-- Copyright (C) 2008 Mats Klingberg

module Expr (Expr) where

data Expr = Symbol String           -- Scheme symbol
          | Number Integer          -- We only support integers so far       
          | String String           -- String ("asdf")
          | Bool Bool               -- Booleans
          | Pair Expr Expr          -- Basic list building block
          | Nil                     -- Empty list


