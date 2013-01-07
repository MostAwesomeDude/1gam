module Gemstone.Color where

import Graphics.Rendering.OpenGL

type RGB = Color3 GLubyte

red, green, blue :: RGB
red = Color3 255 0 0
green = Color3 0 255 0
blue = Color3 0 0 255

black, white :: RGB
black = Color3 0 0 0
white = Color3 255 255 255

skyBlue, grassGreen, stoneGray :: RGB
skyBlue = Color3 127 127 255
grassGreen = Color3 0 255 63
stoneGray = Color3 127 127 127
