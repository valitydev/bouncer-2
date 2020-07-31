(ns bouncer.ip
  (:refer-clojure :exclude (range))
  (:require [clojure.core :as c]
            [clojure.string :as s])
  (:import [java.net InetAddress]
           [java.math BigInteger]
           [clojure.lang Util]))

(declare Address->String)

(deftype Address [^BigInteger repr size]
  Comparable
  (compareTo [this other]
    (let [c (compare repr (.-repr ^Address other))]
      (if (zero? c)
        (compare size (.-size ^Address other))
        c)))
  Object
  (equals [this other]
    (and (= repr (.-repr ^Address other))
         (= size (.-size ^Address other))))
  (hashCode [this]
    (Util/hashCombine (.hashCode (.-repr this))
                      (.hashCode (.-size this))))
  (toString [this]
    (Address->String this)))

(deftype Range [^Address addr width]
  Comparable
  (compareTo [this other]
    (let [c (compare addr (.-addr ^Range other))]
      (if (zero? c)
        (compare width (.-width ^Range other))
        c)))
  Object
  (equals [this other]
    (and (= addr (.-addr ^Range other))
         (= width (.-width ^Range other))))
  (hashCode [this]
    (Util/hashCombine (.hashCode (.-addr this))
                      (.hashCode (.-width this))))
  (toString [this]
    (str (Address->String addr) "/" width)))

(defn- clear-bit [^BigInteger i n] (.clearBit i n))
(defn- and-not [^BigInteger i ^BigInteger mask] (.andNot i mask))

(defn- bitsize [^Address addr] (* 8 (.-size addr)))
(defn- maskbit [^Address addr width] (- (bitsize addr) width))

(defn range
  "Instantiate new Range from an Address and width."
  [^Address addr ^long width]
  (let [mask (-> (biginteger 1)
                 (.shiftLeft (maskbit addr width))
                 (.subtract (biginteger 1)))
        repr (and-not (.-repr addr) mask)
        size (.-size addr)
        addr (Address. repr size)]
    (Range. addr width)))

(defn expand
  "Expands a Range into a wider Range once."
  [^Range range]
  (let [addr ^Address (.-addr range)
        width (int (.-width range))]
    (when (> width 0)
      (Range. (Address. (clear-bit (.-repr addr) (maskbit addr width))
                        (.-size addr))
              (dec width)))))

(defn expansions
  "Produces a lazy sequence of consecutive Range expansions, up to '0.0.0.0/0'."
  [^Range range]
  (take-while some? (iterate expand range)))

(defn ranges
  "Produces a lazy sequence of ever-expanding Ranges containing an Address."
  [^Address addr]
  (expansions (Range. addr (bitsize addr))))

(defn partial?
  "Tells whether a Range covers only a subset of addresses."
  [^Range {width :width}]
  (> width 0))

(defn String->Address
  "Parse an Address from a String instance."
  ^Address [s]
  (let [bytes (-> (InetAddress/getByName s) (.getAddress))
        size  (count bytes)]
    (Address. (BigInteger. bytes) size)))

(defn Address->String
  "Represents Address as a String."
  ^String [^Address addr]
  (let [size        (.-size addr)
        to-bytes    (byte-array size (byte 0))
        repr-bytes  (bytes (.toByteArray ^BigInteger (.-repr addr)))
        repr-size   (count repr-bytes)
        offset      (int (- size repr-size))
        repr-from   (Math/abs (min 0 offset))]
    (doseq [idx (c/range repr-from repr-size)]
      (->> (aget repr-bytes idx)
           (aset-byte to-bytes (+ offset idx))))
    (-> (InetAddress/getByAddress to-bytes)
        (.getHostAddress))))

(defn String->Range
  "Parse a Range from a String instance, which looks like '10.42.42.0/24'."
  [s]
  (let [[addr width] (s/split s #"/")
        addr (String->Address addr)
        width (int (Integer/parseInt width))]
    (range addr width)))

(defmethod print-method Address [^Address addr ^java.io.Writer w]
  (.write w (Address->String addr)))

(defmethod print-method Range [^Range range ^java.io.Writer w]
  (do
    (.write w (Address->String (.-addr range)))
    (.write w "/")
    (.write w (str (.-width range)))))

#_(map str (ranges (String->Address "192.168.127.41")))
#_(map str (ranges (String->Address "fc00:42:42::11")))
#_(criterium.core/quick-bench (ranges (String->Address "fc00:42:42::11")))

#_(compare (String->Range "1.1.1.1/32") (String->Range "1.1.1.1/31"))
#_(= (String->Address "1.1.1.1") (String->Address "1.1.1.1"))
#_(= (String->Range "1.1.1.1/32") (String->Range "1.1.1.0/31"))

#_(prn (String->Range "1.1.1.1/32"))
