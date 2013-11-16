; /lib/chicanery/color.clp - Routines for handling chicanery color features

(defgeneric translate/standard-color)
(defgeneric intensity-to-cmap)
(defmethod translate/standard-color
  ((?color LEXEME))
  (switch (lowcase (string-to-field ?color))
          (case opaque then 0)
          (case transparent then 1)
          (case black then 2)
          (case white then 3)
          (case red then 4)
          (case green then 5)
          (case blue then 6)
          (case cyan then 7)
          (case magenta then 8)
          (case yellow then 9)
          (case paleyellow then 10)
          (case darkyellow then 11)
          (case darkgreen then 12)
          (case palegreen then 13)
          (case medgreen then 14)
          (case darkblue then 15)
          (case palebluegreen then 16)
          (case paleblue then 17)
          (case bluegreen then 18)
          (case greygreen then 19)
          (case palegreygreen then 20)
          (case yellowgreen then 21)
          (case medblue then 22)
          (case greyblue then 23)
          (case palegreyblue then 24)
          (case purpleblue then 25)
          (case notacolor then 26)
          (case nofill then 27)
          (default 28)))

(defmethod get-standard-color
  ((?color LEXEME INSTANCE-NAME))
  (get-standard-color 
    (translate/standard-color ?color)))

(defmethod intensity-to-cmap
  "Converts an intensity value to a greyscale color"
  ((?intensity INTEGER (<= 0 ?intensity 255)))
  (rgb-to-cmap ?intensity
               ?intensity
               ?intensity))

(defmethod rgb-to-cmap
  ((?tuple INTEGER MULTIFIELD (>= (length$ ?tuple) 3)))
  (rgb2cmap (nth$ 1 ?tuple)
            (nth$ 2 ?tuple)
            (nth$ 3 ?tuple)))

