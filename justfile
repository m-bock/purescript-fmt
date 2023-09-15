gen:
    purs-to-md --input-purs demo/src/Demo.purs --output-md README.md
    purs-to-md --input-purs demo/src/DemoMore.purs --output-md Samples.md
    purs-to-md --input-purs demo/src/DemoPerf.purs --output-md Performance.md

build-es:
    spago build -p fmt --backend-args '-g js,corefn'
    purs-backend-es build
    prettier --write output-es/*/index.js