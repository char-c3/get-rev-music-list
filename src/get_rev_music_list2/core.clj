(ns get-rev-music-list2.core
  (:require [net.cgrand.enlive-html :as eh]
            [clojure.java.io :as io]
            [clojure.data.json :as json])
  (:use [clojure.pprint :only [pprint]]))

;;(defrecord Music [title bpm lv attr shop-song? could-obtain?])

;; 後で埋める
(def not-available-musics ["Light My Fire"
                           "SOMEDAY -00.prologue-"
                           "Chase the WAVE"
                           "Reseed (Another Edit)"
                           "MEGALOMAN[i]A"])

(def original-page-count 6)
(def license-page-count 4)
(def cxb-implant-count 2)

(def original-page-address "http://www.capcom.co.jp/arcade/rev/PC/music_original.html")
(def license-page-address "http://www.capcom.co.jp/arcade/rev/PC/music_license.html")
(def cxb-implant-address "http://www.capcom.co.jp/arcade/rev/PC/music_foregoing.html")

(def icon-shop-uri "../common/img_common/ico_mlist-shop.png")

(defn scrape [uri]
  (with-open [r (io/reader uri)]
    (eh/html-resource r)))

(defn get-title [res]
  (-> res
      (eh/select #{[:p.n-mTitle]})
      (first)
      (:content)
      (first)))

(defn get-bpm [res]
  (let [bpm-str (-> res
                    (eh/select #{[:p.n-mDataBpm :span]})
                    (first)
                    (:content)
                    (first))
        re #"\d+\.?\d*"
        min-bpm (Float. (re-find re bpm-str))
        max-bpm-str (re-find (re-pattern (str re #"\s*$")) bpm-str)]
    (if (nil? max-bpm-str)
      [min-bpm min-bpm]
      [min-bpm (Float. max-bpm-str)])))

(defn get-level [res]
  (let [lvres (eh/select res #{[:p.n-mDataLv :span]})]
    (letfn [(get-lv [arg]
              (let [lv-str (first (:content arg))]
                (if (= lv-str "--") 0
                  (Integer. lv-str))))]
      (map get-lv lvres))))

(defn get-shop-song? [res]
  (let [icon-zone (-> res
                       (eh/select #{[:div.iconZone :img]}))]
    (if (empty? icon-zone) false
      (->> icon-zone
           (map #(:attrs %))
           (map #(:src %))
           (map #(= % icon-shop-uri))
           ;; iconZone内にshopアイコンが一つでもあればshop曲
           (reduce #(or %1 %2))))))

(defn get-could-obtain? [title]
  (let [ret (some #(= title %) not-available-musics)]
    (if (nil? ret) false ret)))

(defn li->music [res attr]
  (let [title (get-title res)
        bpm (get-bpm res)
        lv (get-level res)
        shop-song? (get-shop-song? res)
        could-obtain? (get-could-obtain? title)]
    ;;(Music. title bpm lv attr shop-song? could-obtain?)
    {:title title, :bpm bpm, :level lv, :attr attr, :isShopSong shop-song?, :couldObtain could-obtain?}))

(comment
(defn get-cxb []
  (let [mlist (-> (scrape cxb-implant-address)
                  (eh/select #{[:ul.n-mList :li.gr-Black2]}))]
    (map #(li->music % "cxb") mlist)))

(defn get-original []
  (loop [i 1 ret []]
    (if (<= i original-page-count)
      (let [mlist (-> (scrape (str original-page-address "?page=" i "#l"))
                      (eh/select #{[:ul.n-mList :li.gr-Black2]}))]
        (recur (inc i) (concat ret (map #(li->music % "org") mlist))))
      ret))))

(defn get-music-data [uri pages attr]
  ;;(pprint (eh/select (scrape (str uri "?page=" "1" "#l")) #{[:ul.n-mList :li.gr-Black2]}))
  (loop [i 1 ret []]
    (if (<= i pages)
      (let [mlist (-> (scrape (str uri "?page=" i "#l"))
                      (eh/select #{[:ul.n-mList :li.gr-Black2]}))]
        (recur (inc i) (concat ret (map #(li->music % attr) mlist))))
      ret)))

(def data (concat [] 
        (get-music-data original-page-address original-page-count "org")
        (get-music-data license-page-address license-page-count "lic")
        (get-music-data cxb-implant-address cxb-implant-count "cxb")))

(defn -main [& args]
  (binding [*out* (java.io.FileWriter. "revdata.json")]
    (json/pprint data)
    (flush)))
  ;;(spit "revdata.json" (json/write-str data)))

;;(defn -main [args]
;;  (get-cxb))
