(function() {var implementors = {};
implementors["postcard"] = [{"text":"impl&lt;'a, F&gt; <a class=\"trait\" href=\"serde/ser/trait.Serializer.html\" title=\"trait serde::ser::Serializer\">Serializer</a> for &amp;'a mut <a class=\"struct\" href=\"postcard/struct.Serializer.html\" title=\"struct postcard::Serializer\">Serializer</a>&lt;F&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;F: <a class=\"trait\" href=\"postcard/flavors/trait.SerFlavor.html\" title=\"trait postcard::flavors::SerFlavor\">SerFlavor</a>,&nbsp;</span>","synthetic":false,"types":["postcard::ser::serializer::Serializer"]}];
implementors["serde"] = [];
implementors["serde_json"] = [{"text":"impl&lt;'a, W, F&gt; <a class=\"trait\" href=\"serde/ser/trait.Serializer.html\" title=\"trait serde::ser::Serializer\">Serializer</a> for &amp;'a mut <a class=\"struct\" href=\"serde_json/struct.Serializer.html\" title=\"struct serde_json::Serializer\">Serializer</a>&lt;W, F&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;W: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.63.0/std/io/trait.Write.html\" title=\"trait std::io::Write\">Write</a>,<br>&nbsp;&nbsp;&nbsp;&nbsp;F: <a class=\"trait\" href=\"serde_json/ser/trait.Formatter.html\" title=\"trait serde_json::ser::Formatter\">Formatter</a>,&nbsp;</span>","synthetic":false,"types":["serde_json::ser::Serializer"]},{"text":"impl <a class=\"trait\" href=\"serde/ser/trait.Serializer.html\" title=\"trait serde::ser::Serializer\">Serializer</a> for <a class=\"struct\" href=\"serde_json/value/struct.Serializer.html\" title=\"struct serde_json::value::Serializer\">Serializer</a>","synthetic":false,"types":["serde_json::value::ser::Serializer"]}];
implementors["serde_yaml"] = [{"text":"impl&lt;'a, W&gt; <a class=\"trait\" href=\"serde/ser/trait.Serializer.html\" title=\"trait serde::ser::Serializer\">Serializer</a> for &amp;'a mut <a class=\"struct\" href=\"serde_yaml/struct.Serializer.html\" title=\"struct serde_yaml::Serializer\">Serializer</a>&lt;W&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;W: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.63.0/std/io/trait.Write.html\" title=\"trait std::io::Write\">Write</a>,&nbsp;</span>","synthetic":false,"types":["serde_yaml::ser::Serializer"]},{"text":"impl <a class=\"trait\" href=\"serde/ser/trait.Serializer.html\" title=\"trait serde::ser::Serializer\">Serializer</a> for <a class=\"struct\" href=\"serde_yaml/value/struct.Serializer.html\" title=\"struct serde_yaml::value::Serializer\">Serializer</a>","synthetic":false,"types":["serde_yaml::value::ser::Serializer"]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()