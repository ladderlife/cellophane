(ns cellophane.checksums-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cellophane.test-utils :refer [remove-whitespace]]
            [cellophane.checksums :as chk]))

(deftest test-checksum
  (are [data res] (= (chk/checksum data) res)
    "<div data-reactid=\".p55bcrvgg0\"></div>" -47641439
    "<div data-reactid=\".0\" id=\"foo\">Hello World</div>" -1847259110
    (remove-whitespace "<div data-reactid=\".0\">
                          <div data-reactid=\".0.0\">
                            <button data-reactid=\".0.0.0\">Load children</button>
                            <ul data-reactid=\".0.0.1\">
                              <li data-reactid=\".0.0.1.0\">1</li>
                              <li data-reactid=\".0.0.1.1\">2</li>
                              <li data-reactid=\".0.0.1.2\">3</li>
                            </ul>
                          </div>
                        </div>") 821447442
    "Lorem ipsum dolor sit amet, ea nam mutat probatus. Erat volutpat liberavisse eu his,
 id qui eius congue accumsan. Pro erat natum ponderum ut. Vidit assum eu eam. Mei animal
 epicurei facilisi te. In eum euismod principes, id soluta volutpat pri. Nulla harum ex has,
 aliquam verterem recteque has eu. Cu noster utamur quaestio quo, eos eius diceret ei. Ponderum
 atomorum has et. Mel amet dolores philosophia ut, eam id erat noluisse postulant. Sit vide
 regione eu. Ne vis justo liber." -1507348871))

(deftest test-assign-react-checksum
  (is (= (chk/assign-react-checksum "<div data-reactid=\".p55bcrvgg0\"></div>")
         "<div data-reactid=\".p55bcrvgg0\" data-react-checksum=\"-47641439\"></div>"))
  (is (= (chk/assign-react-checksum "<div data-reactid=\".0\" id=\"foo\">Hello World</div>")
         "<div data-reactid=\".0\" id=\"foo\" data-react-checksum=\"-1847259110\">Hello World</div>")))
