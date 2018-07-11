(ns assignment3.core
  (:gen-class)
  (:import [java.awt.image BufferedImage])
  (:import [javax.imageio ImageIO])
  (:import [java.io File])
)

(defn new-image
  "Function to create a new image."
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
  )

(defn read-image
  "Function to read an image from a file."
  [filename]
  (let [file (File. filename)]
    (ImageIO/read file)
    )
  )

(defn save-image
  "Function to save an image with a particular extension to a file."
  [image extension filename]
  (let [file (File. filename)]
    (ImageIO/write image extension file)
    )
  )

(def get-width
  "Function to get the width of an image."
  (memoize
    (fn[image]
  (.getWidth image)))
  )

(def get-height
  "Function to get the height of an image."
  (memoize
    (fn[image]
  (.getHeight image)))
  )

(defn duplicate-image
  "Duplicates Image so you can manipulate it without damaging the original"
  [filename]
  (let[image(read-image filename)
       width(get-width image)
       height(get-height image)]
    (new-image width height))
  )

(def get-grey
  "Function to get the RGB components of a pixel in a vector of length 3."
  (memoize
    (fn [image x y]
      (let [rgb (.getRGB image x y)
            grey (bit-and rgb 0xFF)]
        grey)))
  )


(defn set-rgb
  "Function to set the RGB components of a pixel."
  [image x y [red green blue]]
  (let [rgb (+ (bit-shift-left red 16)
               (bit-shift-left blue 8)
               (bit-shift-left green 0))]
    (.setRGB image x y rgb)
    )
  )

(defn set-grey
  "Function to set the grey value of a pixel."
  [image x y grey]

  (set-rgb image x y [grey grey grey])
  )

(defn create-filt
  "Create a matrix with given params"
  [i]
  (let [Hi [[-1 0 1 -2 0 2 -1 0 1]
            [-2 -1 0 -1 0 1 0 1 2]
            [-1 -2 -1 0 0 0 1 2 1]
            [0 -1 -2 1 0 -1 2 1 0]
            [1 0 -1 2 0 -2 1 0 -1]
            [2 1 0 1 0 -1 0 -1 -2]
            [1 2 1 0 0 0 -1 -2 -1]
            [0 1 2 -1 0 1 -2 -1 0]]]

    (get Hi i))
  )

(defn clamp
  "If the intensity after applying filter exceeded the max or min value then fix it"
  [i]
  (if(> i 255) 255 (if (< i 0) 0 i))
  )

(def read-image-memoized (memoize read-image))



(defn kirsh
  "Takes an image and an index and performs the kirsh @ index on each pixel"
  [file i]
  (let [image (read-image-memoized file)
        dup-image (duplicate-image file)
        filter (create-filt i)
        width (- (get-width image) 2)     ;minus by two as the filter can't run on edges
        height (- (get-height image) 2)]

    (dotimes [y height]             ;do for each pixel in the image
      (dotimes [x width]
        (let [edges (into []
                          (for [row (range -1 2)
                                col (range -1 2)
                                :let [grey (get-grey image (+ x row 1) (+ y col 1))
                                      valu (nth grey 0)]] valu))
              multiplied (into [] (for [pos (range 9)
                                        :let [newVal (* (nth edges pos) (nth filter pos))]]
                                    newVal)) pixel (+ (reduce + multiplied) 127)]
          (set-grey dup-image (inc x) (inc y) (clamp pixel))))) dup-image)
  )



(defn get-edge-mag
  "Get the edge magnitude of one pixel"
  [image x y i]
  (let [filter (create-filt i)]
    (let[edges (for[row(range 3)
                    col(range 3)]
                 (get-grey image (+ x row -1) (+ y col -1)))
         pixel (reduce + (map * edges filter))]
      (clamp pixel))
    )
  )

(defn create-histogram
  "Gets values after running a filter over ever pixel, normalises the values and puts them into a histogram of 8 bins"
  [bins]
  (let [pixels (double (count bins))]
    (mapv
      #(/ % pixels)
      (reduce (fn [hist bin] (update hist bin inc))
              (into [] (repeat 8 0))
              bins)))
  )

(defn edge-magnitude-hist
  "Returns  a  normalised  frequency  histogram with 8 bins containing edge magnitudes."
  [file]
  (let[image (read-image-memoized file)
       width (- (get-width image) 2)
       height (- (get-height image) 2)
       mags (for [y (range 1 height) x (range 1 width)]
             (reduce max (map #(quot (get-edge-mag image x y %) 32) (range 8))))]
    (create-histogram mags))
  )

(defn edge-direction-hist
  "Returns  a  normalised  direction  histogram with 8 bins containing edge directions."
  [file]
  (let[image (read-image-memoized file)
       width (- (get-width image) 2)
       height (- (get-height image) 2)
       mags (for [y (range 1 height) x (range 1 width)]
              (apply max-key #(get-edge-mag image x y %) (range 8)))]
    (create-histogram mags))
  )

(defn intensity-hist
  "Returns a normalised intensity histogram "
  [file]
  (let[image (read-image-memoized file)
       width (- (get-width image) 2)
       height (- (get-height image) 2)
       intensities (for [y (range 1 height) x (range 1 width)]
                   (quot (get-grey image x y) 32))]
    (create-histogram intensities))
  )

(def image-descriptor
  "Concatenate histograms"
  (memoize
    (fn [file]
      (map #(/ % 3.0)           ;Dividing the vector of histograms by 3 to re-normalise
           (into [] (concat (edge-magnitude-hist file) (edge-direction-hist file) (intensity-hist file))))))
  )

(defn image-similarity
  "Returns a value between 0.0 and 1.0"
  [file1 file2]
  (let [descriptor1 (image-descriptor file1)
        descriptor2 (image-descriptor file2)
        sims (reduce + (map min descriptor1 descriptor2))]
    (/ (Math/round (* sims 1000.0)) 1000.0)) ;rounding the values as there are some rounding issues with longs
  )
(def compare-sets
  "Compares every image in a set with every image in another set"
  (memoize(fn[set1 set2]
    (doseq [i (range 1 21) j(range 1 21)]
      (println (image-similarity (str set1 j ".jpg") (str set2 i ".jpg"))))))
  )

  (defn -main
    "Test the image functions."
    [& args]
    (compare-sets "car" "train")
    )