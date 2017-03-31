{-|
Description: Anki storage; read-only access to Anki database.
Copyright: (c) Michael Mounteney, 2015
License: BSD 3 clause
Maintainer: the project name, all lower case, at landcroft dot com
Stability: experimental
Portability: undefined
-}


{-# LANGUAGE QuasiQuotes, TemplateHaskell, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, ExistentialQuantification, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_HADDOCK prune #-}


module AnkiSchema where


import qualified Yesod as Y
import Data.Int (Int32)
import Data.Text (Text)


Y.share [Y.mkPersist Y.sqlSettings] [Y.persistLowerCase|
TableColumn
	crt	Int32 NOT NULL
	mod	Int32 NOT NULL
	scm	Int32 NOT NULL
	ver	Int32 NOT NULL
	dty	Int32 NOT NULL
	usn	Int32 NOT NULL
	ls	Int32 NOT NULL
	conf	Text NOT NULL
	models	Text NOT NULL
	decks	Text NOT NULL
	dconf	Text NOT NULL
	tags	Text NOT NULL
DataNote
	guid	Text NOT NULL
	mid	Int32 NOT NULL
	mod	Int32 NOT NULL
	usn	Int32 NOT NULL
	tags	Text NOT NULL
	flds	Text NOT NULL
	sfld	Int32 NOT NULL
	csum	Int32 NOT NULL
	flags	Int32 NOT NULL
	data	Text NOT NULL
DataCard
	nid	Int32 NOT NULL
	did	Int32 NOT NULL
	ord	Int32 NOT NULL
	mod	Int32 NOT NULL
	usn	Int32 NOT NULL
	type	Int32 NOT NULL
	queue	Int32 NOT NULL
	due	Int32 NOT NULL
	ivl	Int32 NOT NULL
	factor	Int32 NOT NULL
	reps	Int32 NOT NULL
	lapses	Int32 NOT NULL
	left	Int32 NOT NULL
	odue	Int32 NOT NULL
	odid	Int32 NOT NULL
	flags	Int32 NOT NULL
	data	Text NOT NULL
RevLog
	cid	Int32 NOT NULL
	usn	Int32 NOT NULL
	ease	Int32 NOT NULL
	ivl	Int32 NOT NULL
	lastIvl       Int32 NOT NULL
	factor	Int32 NOT NULL
	time	Int32 NOT NULL
	type	Int32 NOT NULL
Graves
	usn	Int32 NOT NULL
	oid	Int32 NOT NULL
	type	Int32 NOT NULL
	Primary usn oid
|]
