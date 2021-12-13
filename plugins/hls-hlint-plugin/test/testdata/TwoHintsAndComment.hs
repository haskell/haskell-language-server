module TwoHintsAndComment where
biggest items = foldr1 max items -- the line above will show two hlint hints, "eta reduce" and "use maximum"
