-- Expr: Scheme expression module
--
-- Copyright (C) 2008 Mats Klingberg

module Expr (Expr) where

data Expr = Symbol String           -- Scheme symbol
          | Number Integer          -- We only support integers so far       
          | Bool Bool               -- Booleans
          | String String           -- String ("asdf")
          | Pair Expr Expr          -- Basic list building block
          | Null                    -- Empty list
          deriving (Show)

