tell application "TextEdit"
	activate
	
	-- Get actual screen dimensions
	set {screenWidth, screenHeight, scale} to words of (do shell script "system_profiler SPDisplaysDataType | awk '/Main Display: Yes/{found=1} /Resolution/{width=$2; height=$4} /Retina/{scale=($2 == \"Yes\" ? 2 : 1)} /^ {8}[^ ]+/{if(found) {exit}; scale=1} END{printf \"%d %d %d\\n\", width, height, scale}'")
	
	-- Convert strings to numbers
	set screenWidth to screenWidth as integer
	set screenHeight to screenHeight as integer
	set scale to scale as integer
	
	-- Create and position windows
	set windowCount to 1
	set columnsPerRow to 25
	set rowsTotal to 15
	
	-- Calculate base window dimensions 
	set baseWidth to screenWidth / (columnsPerRow * 2.4)
	set baseHeight to screenHeight / (rowsTotal * 2.4)
	
	-- Close any existing windows
	close every window
	
	repeat with i from 0 to (columnsPerRow - 1)
		repeat with j from 0 to (rowsTotal - 1)
			-- Create a pattern using modulo
			set diagonalWave to ((i + j) mod (random number from 5 to 10))
			set circularWave to ((i * i + j * j) mod (random number from 5 to 12))
			set pattern to diagonalWave + (circularWave mod (random number from 4 to 8))
			
			-- Vary window sizes based on position
			set sizeVariation to (((i + j) mod (random number from 3 to 6)) / (random number from 3 to 6))
			set windowWidth to baseWidth * (1 + sizeVariation)
			set windowHeight to baseHeight * (1 + sizeVariation)
			
			-- Create windows based on pattern value
			if pattern > 6 then
				make new document
				
				tell front window
					set xPos to (i * baseWidth * 1.2) as integer
					set yPos to (j * baseHeight * 1.2) as integer
					set bounds to {xPos, yPos, (xPos + windowWidth) as integer, (yPos + windowHeight) as integer}
				end tell
				
				set windowCount to windowCount + 1
			end if
		end repeat
	end repeat
	
	-- Show dialog to close windows
	display dialog "Click OK to close all windows" buttons {"OK"} default button "OK"
	close every window
end tell