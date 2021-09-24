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
### Current project
 - find out why linebreaks wont render
 - >greentext
   - check _postResponse_content for any ">"
     - split Text into [Text] separating from > to linebreak
     - the > Text will be rendered with a different class_
 - images
### TODO
 - scroll to bottom after comment
 - link to comment with >>[id number]
 - clean and comment up the code
 - fix http response codes from backend
 - make sql function work from migration
   CREATE OR REPLACE FUNCTION getThreadPreviews(page INTEGER, count INTEGER) RETURNS SETOF posts
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
   LANGUAGE plpgsql:
