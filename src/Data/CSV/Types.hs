{-# LANGUAGE DataKinds #-}

newtype Column = Column String
newtype Row = Row [()]
newtype Header = Header ()
newtype CSV = CSV [Row]