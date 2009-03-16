{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Data.TCList where

import Prelude hiding ((!!))
import Foreign.C.Types
import Foreign.C.String

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal

import Data.Word
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe

#include <tcutil.h>

data TCList = TCList !(ForeignPtr LIST)

new :: IO TCList
new = c_tclistnew >>= newForeignPtr tclistFinalizer >>= return . TCList

new2 :: Int -> IO TCList
new2 n = do
  l <- c_tclistnew2 (fromIntegral n)
  p <- newForeignPtr tclistFinalizer l
  return $ TCList p

copy :: TCList -> IO TCList
copy (TCList fptr) =
    withForeignPtr fptr $ \p -> do
      l <- c_tclistdup p
      TCList `fmap` newForeignPtr tclistFinalizer l

delete :: TCList -> IO ()
delete (TCList fptr) = finalizeForeignPtr fptr

num :: TCList -> IO Int
num (TCList fptr) =
    withForeignPtr fptr $ \p -> do
        n <- c_tclistnum p
        return $ fromIntegral n

get :: TCList -> Int -> IO (Maybe ByteString)
get (TCList fptr) index =
    withForeignPtr fptr $ \p ->
        alloca $ \sizbuf -> do
            vbuf <- c_tclistval p (fromIntegral index) sizbuf
            if vbuf == nullPtr
              then return Nothing
              else do siz  <- peek sizbuf
                      Just `fmap` unsafePackCStringLen (vbuf, fromIntegral siz)

push :: TCList -> ByteString -> IO ()
push (TCList fptr) val =
    withForeignPtr fptr $ \p ->
        unsafeUseAsCStringLen val $ \(vbuf, vsiz) ->
            c_tclistpush p (castPtr vbuf) (fromIntegral vsiz)

pop :: TCList -> IO (Maybe ByteString)
pop (TCList fptr) =
    withForeignPtr fptr $ \p ->
        alloca $ \sizbuf -> do
          vbuf <- c_tclistpop p sizbuf
          if vbuf == nullPtr
            then return Nothing
            else do siz <- peek sizbuf
                    Just `fmap` unsafePackCStringFinalizer (castPtr vbuf)
                                    (fromIntegral siz) (free vbuf)

data LIST

foreign import ccall safe "tclistnew"
  c_tclistnew :: IO (Ptr LIST)

foreign import ccall safe "tclistnew2"
  c_tclistnew2 :: CInt -> IO (Ptr LIST)

foreign import ccall safe "tclistdup"
  c_tclistdup :: Ptr LIST -> IO (Ptr LIST)

foreign import ccall safe "tclistdel"
  c_tclistdel :: Ptr LIST -> IO ()

foreign import ccall "&tclistdel"
  tclistFinalizer :: FunPtr (Ptr LIST -> IO ())

foreign import ccall safe "tclistnum"
  c_tclistnum :: Ptr LIST -> IO CInt

foreign import ccall safe "tclistval"
  c_tclistval :: Ptr LIST -> CInt -> Ptr CInt -> IO CString

foreign import ccall safe "tclistval2"
  c_tclistval2 :: Ptr LIST -> CInt -> IO CString

foreign import ccall safe "tclistpush"
  c_tclistpush :: Ptr LIST -> Ptr Word8 -> CInt -> IO ()

foreign import ccall safe "tclistpush2"
  c_tclistpush2 :: Ptr LIST -> CString -> IO ()

foreign import ccall safe "tclistpop"
  c_tclistpop :: Ptr LIST -> Ptr CInt -> IO CString

foreign import ccall safe "tclistpop2"
  c_tclistpop2 :: Ptr LIST -> IO CString

{-

/* Add an element at the top of a list object.
   `list' specifies the list object.
   `ptr' specifies the pointer to the region of the new element.
   `size' specifies the size of the region. */
void tclistunshift(TCLIST *list, const void *ptr, int size);


/* Add a string element at the top of a list object.
   `list' specifies the list object.
   `str' specifies the string of the new element. */
void tclistunshift2(TCLIST *list, const char *str);


/* Remove an element of the top of a list object.
   `list' specifies the list object.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the removed element.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use.  If the list is empty, the return value is `NULL'. */
void *tclistshift(TCLIST *list, int *sp);


/* Remove a string element of the top of a list object.
   `list' specifies the list object.
   The return value is the string of the removed element.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use.  If the list is empty, the return
   value is `NULL'. */
char *tclistshift2(TCLIST *list);


/* Add an element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the new element.
   `ptr' specifies the pointer to the region of the new element.
   `size' specifies the size of the region.
   If `index' is equal to or more than the number of elements, this function has no effect. */
void tclistinsert(TCLIST *list, int index, const void *ptr, int size);


/* Add a string element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the new element.
   `str' specifies the string of the new element.
   If `index' is equal to or more than the number of elements, this function has no effect. */
void tclistinsert2(TCLIST *list, int index, const char *str);


/* Remove an element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the element to be removed.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the removed element.
   Because an additional zero code is appended at the end of the region of the return value,
   the return value can be treated as a character string.  Because the region of the return
   value is allocated with the `malloc' call, it should be released with the `free' call when it
   is no longer in use.  If `index' is equal to or more than the number of elements, no element
   is removed and the return value is `NULL'. */
void *tclistremove(TCLIST *list, int index, int *sp);


/* Remove a string element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the element to be removed.
   The return value is the string of the removed element.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use.  If `index' is equal to or more
   than the number of elements, no element is removed and the return value is `NULL'. */
char *tclistremove2(TCLIST *list, int index);


/* Overwrite an element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the element to be overwritten.
   `ptr' specifies the pointer to the region of the new content.
   `size' specifies the size of the new content.
   If `index' is equal to or more than the number of elements, this function has no effect. */
void tclistover(TCLIST *list, int index, const void *ptr, int size);


/* Overwrite a string element at the specified location of a list object.
   `list' specifies the list object.
   `index' specifies the index of the element to be overwritten.
   `str' specifies the string of the new content.
   If `index' is equal to or more than the number of elements, this function has no effect. */
void tclistover2(TCLIST *list, int index, const char *str);


/* Sort elements of a list object in lexical order.
   `list' specifies the list object. */
void tclistsort(TCLIST *list);


/* Search a list object for an element using liner search.
   `list' specifies the list object.
   `ptr' specifies the pointer to the region of the key.
   `size' specifies the size of the region.
   The return value is the index of a corresponding element or -1 if there is no corresponding
   element.
   If two or more elements correspond, the former returns. */
int tclistlsearch(const TCLIST *list, const void *ptr, int size);


/* Search a list object for an element using binary search.
   `list' specifies the list object.  It should be sorted in lexical order.
   `ptr' specifies the pointer to the region of the key.
   `size' specifies the size of the region.
   The return value is the index of a corresponding element or -1 if there is no corresponding
   element.
   If two or more elements correspond, which returns is not defined. */
int tclistbsearch(const TCLIST *list, const void *ptr, int size);


/* Clear a list object.
   `list' specifies the list object.
   All elements are removed. */
void tclistclear(TCLIST *list);


/* Serialize a list object into a byte array.
   `list' specifies the list object.
   `sp' specifies the pointer to the variable into which the size of the region of the return
   value is assigned.
   The return value is the pointer to the region of the result serial region.
   Because the region of the return value is allocated with the `malloc' call, it should be
   released with the `free' call when it is no longer in use. */
void *tclistdump(const TCLIST *list, int *sp);


/* Create a list object from a serialized byte array.
   `ptr' specifies the pointer to the region of serialized byte array.
   `size' specifies the size of the region.
   The return value is a new list object.
   Because the object of the return value is created with the function `tclistnew', it should
   be deleted with the function `tclistdel' when it is no longer in use. */
TCLIST *tclistload(const void *ptr, int size);
-}
