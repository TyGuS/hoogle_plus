'use babel';

import TestAtomPackageView from './test-atom-package-view';
import { CompositeDisposable } from 'atom';
import request from 'request'

export default {

  testAtomPackageView: null,
  modalPanel: null,
  subscriptions: null,

  activate(state) {
    this.testAtomPackageView = new TestAtomPackageView(state.testAtomPackageViewState);
    this.modalPanel = atom.workspace.addModalPanel({
      item: this.testAtomPackageView.getElement(),
      visible: false
    });

    // Events subscribed to in atom's system can be easily cleaned up with a CompositeDisposable
    this.subscriptions = new CompositeDisposable();

    // Register command that toggles this view
    this.subscriptions.add(atom.commands.add('atom-workspace', {
      'test-atom-package:toggle': () => this.toggle()
    }));
  },

  deactivate() {
    this.modalPanel.destroy();
    this.subscriptions.dispose();
    this.testAtomPackageView.destroy();
  },

  serialize() {
    return {
      testAtomPackageViewState: this.testAtomPackageView.serialize()
    };
  },
  http(config) {
    return new Promise((resolve, reject ) => {
        testj = {
                  "methodName": "shear",
                  "paramNames": [
                    "sypet_arg0",
                    "sypet_arg1",
                    "sypet_arg2"
                  ],
                  "srcTypes": [
                    "java.awt.geom.Rectangle2D",
                    "double",
                    "double"
                  ],
                  "tgtType": "java.awt.geom.Rectangle2D",
                  "packages": [
                    "java.awt.geom"
                  ],
                  "testBody": "public static boolean test() throws Throwable { java.awt.geom.Rectangle2D area = new java.awt.geom.Rectangle2D.Double(10, 20, 10, 10); java.awt.geom.Rectangle2D target = new java.awt.geom.Rectangle2D.Double(20, 24, 15, 14); java.awt.geom.Rectangle2D result = shear(area, 0.5, 0.4); return (target.equals(result));}"
                }
        console.log(JSON.stringify(config))
        console.log(JSON.stringify(testj))


        options = {
            url: 'http://127.0.0.1:9092',
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            //body: JSON.stringify(testj)
            body: JSON.stringify(config)
        };
        //console.log(JSON.stringify(testj))
        request.post(options, function (err, response, body) {
            if(!err && response.statusCode == 200) {
                resolve(body)
            }
            else {
                reject({
                    'error': err
                })
           atom.notifications.addError("Server Error")
            }
           console.log(err, body);
       });
    })

},
toggle() {
  //try {
    const editor = atom.workspace.getActiveTextEditor();
    const buf =  editor.getSelectedBufferRange()
    const words = editor.getSelectedText().split('\n');
    console.log(words)
    w = []
    test  =[]
    intest = false
    for(var x =0; x < words.length ; x++) {
        if (words[x].slice(0,3) == "//#") {
            w.push(words[x].slice(4).trim())
            x++;
            w.push(words[x].slice(4).trim())
            x++;
            w.push(words[x].slice(4).trim())
            x++;
            w.push(words[x])
        }
       if ( words[x].slice(0,7) == "// # //") {
           if(intest) {
               intest  = false
           }
           else {
              intest = true
           }
       }
       else if(intest) {
            test.push(words[x])
        }

    }
    test = test.join('').replace(/\r/g, "").replace(w[2] + "(" , "test(")
    console.log(test)
    var regex = /((?:(?:public|private|protected|static|final|abstract|synchronized|volatile)\s+)*)\s*(\w+)\s*(\w+)\((.*?)\)\s*/g
    console.log(w);
    var match = regex.exec(w[3])

    console.log(w[3])
    console.log(match[1])
    console.log(match[2])
    console.log(match[3])
    console.log(match[4])

    var regex2 = /{(.*)}/g
    var match2 = regex2.exec(w[3])

    match[4] = match[4].replace(/,/g, "" )
    l = match[4].split(" ")
    //console.log(l)
    args = []
    types = []
    packages = w[0].split(" ")
    tgtType = w[1].split(" ")
    hints = w[2].split(" ")

    //for (var i = 0; i < (libs.length); i+= 1) {
    //    libs[i] = "./lib/" + libs[i]
    //
    //}

    for (var i = 0; i+1 < (l.length); i+= 2) {
        args.push(l[i+1])
        types.push(l[i])
    }
    console.log(args)

    hints.shift()
    packages.shift()
    tgtType = tgtType[1]

    var config = {}
    config["methodName"] = match[3]
    config["paramNames"] = args
    config["srcTypes"] = types
    config["packages"] = packages
    config["tgtType"] = tgtType
    config["hints"] = hints
    config["testBody"] = test

    console.log(config)



    //this.testAtomPackageView.setCount(w.join("\n"));
    /*var blob = new Blob([w.join("\n")], {type: "text/plain;charset=utf-8"});
    console.log(w.join("\n"))
    FileSaver.saveAs(blob, "hello.txt");
    */

    this.http(config).then((code) => {
        console.log("Returned!")
        console.log(code)
        editor.scanInBufferRange(/\/\/#SyPet/g,  buf, (match) => {
            console.log(match)
            match.replace(code)
        })
    })
    //} catch(error) {
    //    console.log(error)
    //    atom.notifications.addError("Error! Please recheck selection and syntax.")
    //}
    //this.modalPanel.show();
    }


};
