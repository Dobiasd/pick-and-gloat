#!/usr/bin/env bash

rm -r build

mkdir build
mkdir build/js

elm-make src/elm/Main.elm --output build/js/colored_shape_snatch_uncompressed.js

if [ $? -eq 0 ]
then

  cp ./src/.htaccess ./build

  uglifyjs build/js/colored_shape_snatch_uncompressed.js > build/js/colored_shape_snatch.js
  rm build/js/colored_shape_snatch_uncompressed.js

  cp ./src/index.html ./build/index.html
  uglifyjs ./src/htmlmain.js > ./build/js/htmlmain.js
  cp ./src/jquery-1.11.1.min.js ./build/js/jquery-1.11.1.min.js
  yui-compressor ./src/style.css > ./build/style.css

fi