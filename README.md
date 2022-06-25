# OBchan
## a simple image board made with the Obelisk
This will be a simple *chan styled image board with heavily commented code when done.  This seems like a fun simple project to learn how to build a full stack application with Haskell and  [Obelisk](https://github.com/obsidiansystems/obelisk).

### Done
 - Routes
 - Database
 - Backend
 - barebones frontend
 - watched and rewatched several hours of lectures on frp
 - achieved total enlightened
 - got it to compile and run, load threads on first visit, goes to a thread once you posted it.
 - some basic css styling
 - /p/x redirects to x's op if x is a comment
 - comment box clear and thread refreshes after comment sent
 - main page now renders the thread op and three last comments
 - threads are ordered by last activity, bump works
 - paging
 - getThreadPreviews function added to migration
### Current project
 - find out why linebreaks wont render
 - >greentext
   - check _postResponse_content for any ">"
     - split Text into [Text] separating from > to linebreak
     - the > Text will be rendered with a different class_
 - images
   - find out why frontend is sending empty Text for image
   - figure out how the image upload works
     - inputElement renders a file picker and yields ```Dynamic t [Files]```
     - capture the event of inputElement updating
       - pull out the first file uploaded, check that its and image before continuing
       - pass ```Event t File``` to ```dataUrlFileReader```
     - ```dataUrlFileReader```
       - generates  a new ```FileReader```
       - passes in the FileReader and the File in to ```readAsDataUrl``` to get an Event t m()
       - somehow capture the result as an ```Event t (Maybe Text)``` using ```wrapDomEvent```.  I still don't fully understand how
     - if the result is an image, it is used as the src of the img element
   - use image upload example to use file upload to get base64 encoded image
     - inputBox now yields (image,content)::(Text,Text)
     - renderComment will check image
       - if empty, then do the usual
       - else add an elAttr "img" with source of the value of image
     - upate threadRequest and commentRequest to take an extra Text argument for the image
     - update everything that calls those
   - fix all postrequest or postresponse that got a bool instead of maybe text
   - figure out how to save images to storage
     - change ```readAsDataUrl``` to ```readAsArrayBuffer```
     - change img tag src to location of image
### TODO
 - scroll to bottom after comment
 - link to comment with >>[id number]
 - clean and comment up the code
 - fix http response codes from backend
