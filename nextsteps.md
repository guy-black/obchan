### Current project
 - figure out import issues on backend
 - figure out why green text only sometimes works
 - images
   - update threadRequest and commentRequest to use postForms, FormValue, fileToFormValue
     - now they return the XhrRespones
     - no more performRequestAsync in app
   - figure out what to do with the image when it's on the back end, File can be converted to Blob
     - Snap.Utils.FileUploads.handleFormUploads
     - System.IO.openBinaryFile ("/imgs/" ++ (show id) ++ ".png") WriteMode
       - creates an empty file at given filepath and gives a IO Handle
       - System.IO.hputBuf handle (buf :: Ptr a) (x :: Int) will write x bytes from buf to the handle
   - make backend just check if a key-value "image"-File exists in map and assign the bool accordingly in db
     - if newThread, reject it
     - after i figure out how to write the image to disk i will change the image field from bool to text of url
### TODO
 - scroll to bottom after comment
 - link to comment with >>[id number]
 - clean and comment up the code
 - fix http response codes from backend
 - make sql function work from migration
   `CREATE OR REPLACE FUNCTION getThreadPreviews(page INTEGER, count INTEGER) RETURNS SETOF posts
   AS
   $$
   DECLARE
     op posts%rowtype;
   BEGIN
     FOR op IN
     SELECT * FROM posts WHERE posts.op is NULL ORDER BY posts.lastact desc OFFSET page LIMIT count
     LOOP
       RETURN NEXT op;
       RETURN QUERY SELECT * FROM posts WHERE posts.op = op.id ORDER BY posts.datetime desc LIMIT 3;
     END LOOP;
     RETURN;
   END;
   $$
   LANGUAGE plpgsql:`
