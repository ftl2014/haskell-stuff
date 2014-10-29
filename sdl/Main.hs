{-# LANGUAGE OverloadedStrings #-}

import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Types as SDL
import qualified Graphics.UI.SDL.Event as SDL.Event
import qualified Graphics.UI.SDL.Video as SDL.Video

import Data.Word

import Control.Monad (forever)

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign
import Foreign.C.String

windowWidth 	= 800
windowHeight 	= 600
windowTitle  	= "SDL2 Bitmapped Window"
bitmapPath	 	= "haskell.bmp"

eventLoop renderer = (malloc :: IO (Ptr SDL.Event)) >>= eventLoop0 renderer
			where eventLoop0 renderer eventPtr = 
				do
					SDL.Video.renderPresent renderer
					SDL.Event.waitEvent eventPtr
					event <- peek eventPtr
					case event of
						SDL.QuitEvent _ _ -> return ()
						otherwise -> eventLoop0 renderer eventPtr


main = do
	windowTitleCStr <- newCAString windowTitle
	bitmapPathCStr  <- newCAString bitmapPath
	windowXPos		<- return (windowHeight `div` 2)
	windowYPos		<- return (windowWidth `div` 2)

	SDL.init 0x10 -- Init Video
	window <- SDL.Video.createWindow windowTitleCStr windowXPos windowYPos windowWidth windowHeight  0
	windowSurface <- SDL.Video.getWindowSurface window
	renderer <- SDL.Video.createRenderer window (-1) 0
	bmp	<- SDL.Video.loadBMP bitmapPathCStr

	texture <- SDL.Video.createTextureFromSurface renderer bmp

	SDL.Video.renderClear renderer
	SDL.Video.renderCopy renderer texture nullPtr nullPtr
	SDL.Video.renderPresent renderer
	
	SDL.Video.freeSurface bmp
			
	eventLoop renderer

	SDL.Video.destroyTexture texture
	SDL.Video.destroyRenderer renderer
	SDL.Video.destroyWindow window

	--updateRect screen (Rect 0 0 actualWidth actualHeight)
	
