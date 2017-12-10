module Main where

import Snap
import Api.Yab

main = serveSnaplet defaultConfig appInit
