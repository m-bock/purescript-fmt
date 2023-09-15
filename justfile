gen:
    purs-to-md --input-purs demo/src/Demo.purs --output-md README.md
    purs-to-md --input-purs demo/src/DemoMore.purs --output-md Samples.md
    purs-to-md --input-purs demo/src/DemoPerf.purs --output-md Performance.md

build-es:
    cd demo; spago build -p fmt-demo --output '../output' --backend-args '-g js,corefn'
    purs-backend-es build
    prettier --write output-es/*/index.js